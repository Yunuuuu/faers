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
methods::setGeneric("faers_phv_table", function(object, ...) {
    methods::makeStandardGeneric("faers_phv_table")
})

#' @param pt A string specify the events column in `reac` field of object.
#' @param interested A [FAERSascii] object with data from interested drug, must
#' be a subset of `object`. If `interested` is set to `NULL`, the `faers_filter`
#' function will be employed to extract data for the drug of interest from the
#' `object`.
#' @method faers_phv_table FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_phv_table", "FAERSascii", function(object, pt = "soc_name", ..., interested = NULL) {
    assert_string(pt, empty_ok = FALSE)
    full_reac <- faers_get(object, field = "reac")
    full_reac <- full_reac[[pt]]
    if (is.null(full_reac)) {
        cli::cli_abort("Cannot find {.arg pt} in {.field reac} field")
    }
    assert_s4_class(interested, "FAERSascii", null_ok = TRUE)
    if (is.null(interested)) {
        interested <- faers_filter(object, ...)
    } else {
        full_primaryids <- faers_get(object, field = "demo")$primaryid
        interested_primaryids <- faers_get(interested, field = "demo")$primaryid
        if (!all(interested_primaryids %in% full_primaryids)) {
            cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg object}")
        }
    }
    interested_reac <- faers_get(interested, field = "reac")[[pt]]
    # assert again, `interested` must be a subset of `object`
    if (!is.null(interested) && !all(interested_reac %in% full_reac)) {
        cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg object}")
    }
    n1. <- length(interested_reac) # scalar
    n <- length(full_reac) # scalar
    n.1 <- c(table(full_reac))
    reac_names <- names(n.1)
    n11 <- c(table(interested_reac))[reac_names]
    n11 <- data.table::fifelse(is.na(n11), 0L, n11)
    data.table::data.table(
        reac_events = reac_names,
        a = n11, b = n1. - n11, c = n.1 - n11,
        d = n - (n1. + n.1 - n11)
    )
})

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
methods::setMethod("faers_phv_signal", "FAERSascii", function(object, ..., methods = NULL, alpha = 0.05, correct = TRUE, n_mcmc = 1e5L, alpha1 = 0.5, alpha2 = 0.5) {
    out <- faers_phv_table(object, ...)
    cbind(
        out,
        do.call(phv_signal, c(out[, !"reac_events"], list(
            methods = methods, alpha = alpha, correct = correct,
            n_mcmc = n_mcmc, alpha1 = alpha1, alpha2 = alpha2
        )))
    )
})
