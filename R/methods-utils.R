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
#'  - `faers_get`, `[[`, `$`, and `[`: Extract a specific field
#'    [data.table][data.table::data.table] or a list of field
#'    [data.table][data.table::data.table] from [FAERS] object.
#'  - `faers_primaryid`: Extract the `primaryid` from `demo` field.
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

#' @param field A string indicates the FAERS fields to use. Only values "demo",
#' "drug", "indi", "ther", "reac", "rpsr", and "outc" can be used. For
#' `faers_filter`, this filed data will be passed to `.fn` to extract primaryid;
#' if `NULL`, the `object` will be passed to `.fn` directly.
#' @export
#' @method faers_get FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_get", "FAERSascii", function(object, field) {
    field <- match.arg(field, faers_ascii_file_fields)
    out <- object@data[[field]]
    if (object@standardization && any(field == c("indi", "reac"))) {
        cbind(out[, !"meddra_idx"], object@meddra[out$meddra_idx])
    } else {
        out
    }
})

#' @export
#' @aliases faers_primaryid
#' @rdname FAERS-methods
methods::setGeneric("faers_primaryid", function(object, ...) {
    methods::makeStandardGeneric("faers_primaryid")
})

#' @export
#' @method faers_primaryid FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_primaryid", "FAERSascii", function(object) {
    object@data$demo$primaryid
})


#######################################################
#' @param x A [FAERSascii] object.
#' @param i,name Indices specifying elements to extract. For `i`, it will be
#' okay to use integer indices.
#' @export
#' @aliases [,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("[", "FAERSascii", function(x, i) {
    data <- x@data
    out <- data[use_indices(i, names(data))]
    if (x@standardization) {
        ii <- intersect(names(out), c("indi", "reac"))
        for (i in ii) {
            meddra_idx <- out[[i]]$meddra_idx
            out[[i]] <- cbind(out[[i]][, !"meddra_idx"], x@meddra[meddra_idx])
        }
    }
    out
})

#' @export
#' @aliases [[,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("[[", "FAERSascii", function(x, i) {
    assert_length(i, 1L)
    x[i][[1L]]
})

#' @export
#' @aliases $,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("$", "FAERSascii", function(x, name) {
    x[[rlang::as_name(rlang::ensym(name))]]
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
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        interested <- faers_filter(object, ...)
        faers_phv_table(object = object, pt = pt, interested = interested)
    }
)

#' @rdname FAERS-methods
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "missing"),
    function(object, pt = "soc_name", interested, object2) {
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        if (!interested@standardization) {
            cli::cli_abort("{.arg interested} must be standardized using {.fn faers_standardize}")
        }
        full_primaryids <- faers_primaryid(object)
        interested_primaryids <- faers_primaryid(interested)
        if (!all(interested_primaryids %in% full_primaryids)) {
            cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg object}")
        }
        full_reac <- faers_get(object, field = "reac")
        interested_reac <- faers_get(interested, field = "reac")

        n <- nrow(full_reac) # scalar
        n1. <- nrow(interested_reac) # scalar
        out <- merge(
            eval(substitute(
                full_reac[, list(n.1 = .N), by = pt],
                list(pt = pt)
            )),
            eval(substitute(
                interested_reac[, list(a = .N), by = pt],
                list(pt = pt)
            )),
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
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        if (!object2@standardization) {
            cli::cli_abort("{.arg object2} must be standardized using {.fn faers_standardize}")
        }
        primaryids <- faers_primaryid(object)
        primaryids2 <- faers_primaryid(object2)
        overlapped_idx <- primaryids %in% primaryids2
        if (any(overlapped_idx)) {
            cli::cli_warn("{.val {overlapped_idx}} report{?s} are overlapped between {.arg object} and {.arg object2}")
        }
        interested_reac <- faers_get(object, field = "reac")
        interested_reac2 <- faers_get(object2, field = "reac")
        n1. <- nrow(interested_reac)
        n0. <- nrow(interested_reac2)
        out <- merge(
            eval(substitute(
                interested_reac[, list(a = .N), by = pt],
                list(pt = pt)
            )),
            eval(substitute(
                interested_reac2[, list(c = .N), by = pt],
                list(pt = pt)
            )),
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

utils::globalVariables(c("a", "b", "d", "n.1"))

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

#########################################################
use_indices <- function(i, names, arg = rlang::caller_arg(i), call = rlang::caller_env()) {
    if (anyNA(i)) {
        cli::cli_abort(
            sprintf("%s cannot contain `NA`", style_arg(arg)),
            call = call
        )
    }
    if (is.character(i)) {
        outbounded_values <- setdiff(i, names)
        if (length(outbounded_values)) {
            cli::cli_abort(sprintf(
                "%s contains outbounded values: {outbounded_values}",
                style_arg(arg)
            ), call = call)
        }
    } else if (is.numeric(i)) {
        if (any(i < 1L) || any(i > length(names))) {
            cli::cli_abort(sprintf(
                "%s contains out-of-bounds indices", style_arg(arg)
            ), call = call)
        }
    } else {
        cli::cli_abort(sprintf(
            "%s must be an atomic numeric or character",
            style_arg(arg)
        ), call = call)
    }
    i
}
