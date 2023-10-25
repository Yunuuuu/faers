#' Methods for FAERS class
#'
#' Utils function for [FAERS] class.
#' @param object A [FAERS] object.
#' @param ... Other arguments passed to specific methods.
#'  - `faers_filter`: other arguments passed to `.fn`.
#'  - `faers_phv_table`: other arguments passed to `faers_filter` and `...` is
#'    solely used when `interested` is `NULL`.
#'  - `faers_phv_signal`: other arguments passed to `faers_phv_table`.
#' @details
#'  - `faers_get`: Extract a specific field [data.table][data.table::data.table]
#'    from [FAERS] object.
#'  - `faers_keep`: only keep data from specified `primaryid`. Note: `year`,
#'    `quarter`, `deletedCases` will be kept as the original. So make sure you
#'    didn't filter a whole period FAERS quarterly data, in this way, it's much
#'    better to run [faers].
#'  - `faers_filter`: apply a function to extract wanted `primaryid`, then use
#'    `faers_keep` to filter.
#'  - `faers_phv_table`: build a contingency table for all events in `pt`
#'    column.
#'  - `faers_phv_signal`: Pharmacovigilance Analysis used contingency table
#'    constructed with `faers_phv_table`. Details see [phv_signal].
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_get", function(object, ...) {
    methods::makeStandardGeneric("faers_get")
})

#' @param field A string indicates the FAERS fields to used. Only values "demo",
#' "drug", "indi", "ther", "reac", "rpsr", and "outc" can be used.
#' @export
#' @method faers_get FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_get", "FAERSascii", function(object, field) {
    field <- match.arg(field, faers_ascii_file_fields)
    object@data[[field]]
})

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_keep", function(object, ...) {
    methods::makeStandardGeneric("faers_keep")
})

#' @export
#' @param primaryid An atomic character or integer specifies the reports to
#' keep. If `NULL`, will do nothing.
#' @method faers_keep FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_keep", "FAERSascii", function(object, primaryid = NULL) {
    if (is.null(primaryid)) {
        return(object)
    }
    # as all data has a column primaryid, we just rename the variable to use it
    # in the data.table `i`
    .__primaryid__. <- primaryid
    object@data <- lapply(object@data, function(x) {
        x[primaryid %in% .__primaryid__.]
    })
    object
})

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_filter", function(object, ...) {
    methods::makeStandardGeneric("faers_filter")
})

#' @param field A string indicates the FAERS fields data applied with `.fn` to
#' extract primaryid. If `NULL`, the object will be passed to `.fn` directly.
#' For string, only values "demo", "drug", "indi", "ther", "reac", "rpsr", and
#' "outc" can be used.
#' @param .fn A function or formula.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs.
#'
#'   If a **string**, the function is looked up in `globalenv()`.
#' @export
#' @method faers_filter FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_filter", "FAERSascii", function(object, .fn, ..., field = NULL) {
    if (is.null(field)) {
        data <- object
    } else {
        data <- faers_get(object, field = field)
    }
    ids <- rlang::as_function(.fn)(data, ...)
    if (!(is.numeric(ids) || is.character(ids))) {
        cli::cli_abort("{.arg .fn} must return an atomic integer or character")
    }
    faers_keep(object, primaryid = ids)
})

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric(
    "faers_phv_table",
    function(object, ..., interested, object2) {
        methods::makeStandardGeneric("faers_phv_table")
    }
)

#' @param pt A character specify the events column in `reac` field of object.
#' @param interested A [FAERSascii] object with data from interested drug, must
#' be a subset of `object`. If `interested` and `object2` are both `missing`,
#' the `faers_filter` function will be employed to extract data for the drug of
#' interest from the `object`. The value `n11` or `a` will be calculated from
#' `interested` .
#' @rdname FAERS-methods
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "missing", object2 = "missing"),
    function(object, pt = "soc_name", ..., interested, object2) {
        interested <- faers_filter(object, ...)
        faers_phv_table(object = object, pt = pt, interested = interested)
    }
)

#' @rdname FAERS-methods
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "missing"),
    function(object, pt = "soc_name", interested, object2) {
        full_primaryids <- faers_get(object, field = "demo")$primaryid
        interested_primaryids <- faers_get(interested, field = "demo")$primaryid
        if (!all(interested_primaryids %in% full_primaryids)) {
            cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg object}")
        }
        full_reac <- faers_get(object, field = "reac")
        interested_reac <- faers_get(interested, field = "reac")

        n <- nrow(full_reac) # scalar
        n1. <- nrow(interested_reac) # scalar
        out <- data.table::merge(
            full_reac[, list(n.1 = .N), by = pt],
            interested_reac[, list(a = .N), by = pt],
            by = pt, all = TRUE, allow.cartesian = TRUE
        )
        out[, a := data.table::fifelse(is.na(a), 0L, a)] # nolint
        out[, b := n1. - a] # nolint
        out[, c := n.1 - a] # nolint
        out[, d := n - (n1. + n.1 - a)] # nolint
        out <- out[, !"n.1"]
        data.table::setcolorder(out, c(pt, "a", "b", "c", "d"))[]
    }
)

#' @param object2 A [FAERSascii] object with data from another interested drug,
#' In this way, `object` and `object2` should be not overlapped. The value `n11`
#' or `a` will be calculated from `object`
#' @rdname FAERS-methods
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "missing", object2 = "FAERSascii"),
    function(object, pt = "soc_name", interested, object2) {
        primaryids <- faers_get(object, field = "demo")$primaryid
        primaryids2 <- faers_get(object2, field = "demo")$primaryid
        overlapped_idx <- primaryids %in% primaryids2
        if (any(overlapped_idx)) {
            cli::cli_warn("{.val {overlapped_idx}} report{?s} are overlapped between {.arg object} and {.arg object2}")
        }
        interested_reac <- faers_get(object, field = "reac")
        interested_reac2 <- faers_get(object2, field = "reac")
        n1. <- nrow(interested_reac)
        n0. <- nrow(interested_reac2)
        out <- data.table::merge(
            interested_reac[, list(a = .N), by = pt],
            interested_reac2[, list(c = .N), by = pt],
            by = pt, all = TRUE, allow.cartesian = TRUE
        )
        out[, c("a", "c") := lapply(.SD, function(x) {
            data.table::fifelse(is.na(x), 0L, x)
        }), .SDcols = c("a", "c")]
        out[, b := n1. - a] # nolint
        out[, d := n0. - c] # nolint
        data.table::setcolorder(out, c(pt, "a", "b", "c", "d"))[]
    }
)

#' @rdname FAERS-methods
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "FAERSascii"),
    function(object, interested, object2) {
        cli::cli_abort("{.arg interested} and {.arg object2} are both exclusive, must be provided only one or none")
    }
)

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_phv_signal", function(object, ...) {
    methods::makeStandardGeneric("faers_phv_signal")
})

#' @inheritParams phv_signal
#' @seealso [phv_signal]
#' @method faers_phv_signal FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_phv_signal", "FAERSascii", function(object, pt = "soc_name", ..., methods = NULL, alpha = 0.05, correct = TRUE, n_mcmc = 1e5L, alpha1 = 0.5, alpha2 = 0.5) {
    out <- faers_phv_table(object, pt = pt, ...)
    cbind(
        out,
        do.call(phv_signal, c(out[, .SD, .SDcols = -pt], list(
            methods = methods, alpha = alpha, correct = correct,
            n_mcmc = n_mcmc, alpha1 = alpha1, alpha2 = alpha2
        )))
    )
})

utils::globalVariables(c("a", "b", "d", "n.1"))
