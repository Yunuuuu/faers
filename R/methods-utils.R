#' Methods for FAERS class
#' 
#' Utils function for [FAERS] class.
#' @param object A [FAERS] object.
#' @param ... Other arguments passed to specific methods. For `faers_filter`
#' Other arguments passed to `.fn`.
#' @details
#'  - `faers_get`: Extract a specific field [data.table][data.table::data.table]
#'    from [FAERS] object.
#'  - `faers_keep`: only keep data from specified `primaryid`. Note: `year`,
#'    `quarter`, `deletedCases` will be kept as the original. So make sure you
#'    didn't filter a whole period FAERS quarterly data, in this way, it's much
#'    better to run [faers].
#'  - `faers_filter`: apply a function to extract wanted `primaryid`, then use
#'    `faers_keep` to filter.
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
#'   very compact anonymous functions (lambdas) with up to two inputs. Functions
#'   created from formulas have a special class. Use `is_lambda()` to test for
#'   it.
#'
#'   If a **string**, the function is looked up in `globalenv()`.
#' @export
#' @method faers_filter FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_filter", "FAERSascii", function(object, field, .fn, ...) {
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
