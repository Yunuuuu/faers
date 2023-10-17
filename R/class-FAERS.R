#' FAERS class
#'
#' @description Provide a container for FAERS Quarterly Data file
#' @slot year An integer specifies the year information.
#' @slot quarter A string specifies the quarter information.
#' @slot data For `FAERSxml`, a [data.table][data.table::data.table]. For
#' `FAERSascii`, a list of [data.table][data.table::data.table].
#' @slot type: A string of "ascii" or "xml" indicates the file type used.
#' @slot deletedCases: A list of integers, as of 2019 Quarter one there are new
#' files that lists deleted cases.
#' @details
#'  - `faers_data`: Extract the `data` slot.
#'  - `faers_year`: Extract the `year` slot.
#'  - `faers_quarter`: Extract the `quarter` slot.
#'  - `faers_period`: Extract the `period` slot (just Concatenate the year and
#'    quarter slot).
#'  - `faers_deleted_cases`: Extract the `deletedCases` slot.
#'  - `faers_get`: Extract a specific [data.table][data.table::data.table] field
#'    from [FAERS] object.
#'  - `faers_keep`: only keep data from specified `primaryid`. Note: `year`,
#'    `quarter`, `deletedCases` will be kept as the original. So make sure you
#'    didn't filter a whole period FAERS quarterly data, in this way, it's much
#'    better to run [faers].
#'  - `faers_filter`: apply a function to extract wanted `primaryid`, then use
#'    `faers_keep` to filter.
#' @aliases FAERS
#' @name FAERS-class
methods::setClass(
    "FAERS",
    slots = list(
        year = "integer",
        quarter = "character",
        data = "ANY",
        type = "character"
    ),
    prototype = list(data = NULL)
)

## Validator for FAERS

################ utils methods ########################
# methods::setGeneric("faers_data_period", function(object) {
#     methods::makeStandardGeneric("faers_data_period")
# })

# methods::setMethod("faers_data_period", "FAERSxml", function(object) {
#     faers_period(object)
# })

# methods::setMethod("faers_data_period", "FAERSascii", function(object) {
#     object@data$demo
# })

validate_faers <- function(object) {
    if (length(object@year) != length(object@quarter)) {
        return("the length of `@year` and `@quarter` must be the same")
    }
    if (!all(object@quarter %in% faers_file_quarters)) {
        return(sprintf(
            "`@quarter` must be values in %s",
            oxford_comma(faers_file_quarters, final = "or")
        ))
    }
    if (anyDuplicated(faers_period(object))) {
        return("the period combined from `@year` and `@quarter` must be unique, you cannot import duplicated FAERS Quarterly Data file")
    }
    if (!rlang::is_string(object@type, faers_file_types)) {
        return(sprintf(
            "`@type` must be a string of %s",
            oxford_comma(faers_file_types, final = "or")
        ))
    }
    ### Also, we check if year-quarter in @data slot contain all data from
    # @year-@quarter
    # period <- faers_period(object)
    # if (!setequal(period, faers_data_period(object))) {
    #     return("`@data` must be compatible with `@year` and `@quarter`")
    # }
    TRUE
}

methods::setValidity("FAERS", validate_faers)

#' @aliases FAERSascii
#' @rdname FAERS-class
methods::setClass(
    "FAERSascii",
    slots = list(data = "list", deletedCases = "list"),
    prototype = list(
        data = list(),
        deletedCases = list(),
        type = "ascii"
    ),
    contains = "FAERS"
)

methods::setValidity("FAERSascii", function(object) {
    data <- object@data
    if (!all(faers_ascii_file_fields %in% names(data))) {
        return(sprintf(
            "`@data` must contain the all ascii fields, including %s", oxford_comma(faers_ascii_file_fields)
        ))
    }
    if (!all(vapply(data, data.table::is.data.table, logical(1L)))) {
        return("`@data` must be a list of `data.table`")
    }
    validate_faers(object)
})

#' @aliases FAERSxml
#' @rdname FAERS-class
methods::setClass(
    "FAERSxml",
    slots = list(data = "data.table", header = "list"),
    prototype = list(
        data = data.table::data.table(),
        header = list(), type = "xml"
    ),
    contains = "FAERS"
)

#' @param object A [FAERS] object.
#' @importFrom methods show
#' @export
#' @method show FAERS
#' @rdname FAERS-class
methods::setMethod("show", "FAERS", function(object) {
    l <- length(faers_period(object))
    cat(sprintf(
        "A total of %s FAERS Quarterly %s Data file%s",
        l, object@type, if (l > 1L) "s" else ""
    ), sep = "\n")
})

#######################################################
#' @export
#' @aliases faers_header
#' @rdname FAERS-class
methods::setGeneric("faers_header", function(object) {
    methods::makeStandardGeneric("faers_header")
})

#' @export
#' @method faers_header FAERSxml
#' @aliases faers_header
#' @rdname FAERS-class
methods::setMethod("faers_header", "FAERSxml", function(object) {
    object@header
})

#######################################################
#' @param i Indices specifying elements to extract.
#' @export
#' @rdname FAERS-class
`[[.FAERS` <- function(object, i) {
    object@data[[use_indices(i, names(object@data))]]
}

#' @export
#' @rdname FAERS-class
`[.FAERS` <- function(object, i) {
    object@data[use_indices(i, names(object@data))]
}

#######################################################
#' @param object A [FAERS] object.
#' @param ... Other arguments passed to specific methods. For `faers_filter`
#' Other arguments passed to `.fn`.
#' @export
#' @aliases faers_data
#' @rdname FAERS-class
methods::setGeneric("faers_data", function(object, ...) {
    methods::makeStandardGeneric("faers_data")
})

#' @export
#' @method faers_data FAERS
#' @rdname FAERS-class
methods::setMethod("faers_data", "FAERS", function(object) {
    object@data
})

#' @export
#' @rdname FAERS-class
methods::setGeneric("faers_year", function(object) {
    methods::makeStandardGeneric("faers_year")
})

#' @export
#' @include class-FAERS.R
#' @method faers_year FAERS
#' @aliases faers_year
#' @rdname FAERS-class
methods::setMethod("faers_year", "FAERS", function(object) {
    object@year
})

##################################################################
#' @export
#' @aliases faers_quarter
#' @rdname FAERS-class
methods::setGeneric("faers_quarter", function(object) {
    methods::makeStandardGeneric("faers_quarter")
})

#' @export
#' @method faers_quarter FAERS
#' @aliases faers_quarter
#' @rdname FAERS-class
methods::setMethod("faers_quarter", "FAERS", function(object) {
    object@quarter
})

#################################################################
#' @export
#' @aliases faers_period
#' @rdname FAERS-class
methods::setGeneric("faers_period", function(object) {
    methods::makeStandardGeneric("faers_period")
})

#' @export
#' @method faers_period FAERS
#' @aliases faers_period
#' @rdname FAERS-class
methods::setMethod("faers_period", "FAERS", function(object) {
    paste0(object@year, object@quarter)
})

#################################################################
#' @export
#' @aliases faers_deleted_cases
#' @rdname FAERS-class
methods::setGeneric("faers_deleted_cases", function(object, ...) {
    methods::makeStandardGeneric("faers_deleted_cases")
})

#' @param flatten If `TRUE`, will flatten the `deletedCases` list.
#' @export
#' @method faers_deleted_cases FAERS
#' @aliases faers_field
#' @rdname FAERS-class
methods::setMethod("faers_deleted_cases", "FAERSascii", function(object, flatten = TRUE) {
    out <- object@deletedCases
    if (isTRUE(flatten)) {
        unique(unlist(out, recursive = FALSE, use.names = FALSE))
    } else {
        out
    }
})

##############################################################
#' @export
#' @rdname FAERS-class
methods::setGeneric("faers_get", function(object, ...) {
    methods::makeStandardGeneric("faers_get")
})

#' @param field A string indicates the FAERS fields to used. Only values "demo",
#' "drug", "indi", "ther", "reac", "rpsr", and "outc" can be used.
#' @export
#' @method faers_get FAERSascii
#' @rdname FAERS-class
methods::setMethod("faers_get", "FAERSascii", function(object, field) {
    field <- match.arg(field, faers_ascii_file_fields)
    object@data[[field]]
})

##############################################################
#' @export
#' @rdname FAERS-class
methods::setGeneric("faers_keep", function(object, ...) {
    methods::makeStandardGeneric("faers_keep")
})

#' @export
#' @param primaryid An atomic character or integer specifies the reports to
#' keep. If `NULL`, will do nothing.
#' @method faers_keep FAERSascii
#' @rdname FAERS-class
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
#' @method faers_keep FAERSascii
#' @rdname FAERS-class
methods::setGeneric("faers_filter", function(object, ...) {
    methods::makeStandardGeneric("faers_filter")
})

#' @param field A string indicates the FAERS fields data applied with `.fn` to
#' extract primaryid. Only values "demo", "drug", "indi", "ther", "reac",
#' "rpsr", and "outc" can be used.
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
#'   If a **string**, the function is looked up in `env`. Note that
#'   this interface is strictly for user convenience because of the
#'   scoping issues involved. Package developers should avoid
#'   supplying functions by name and instead supply them by value.
#' @export
#' @method faers_filter FAERSascii
#' @rdname FAERS-class
methods::setMethod("faers_filter", "FAERSascii", function(object, field, .fn, ...) {
    data <- faers_get(object, field = field)
    ids <- rlang::as_function(.fn)(data, ...)
    if (!(is.numeric(ids) || is.character(ids))) {
        cli::cli_abort("{.arg .fn} must return an atomic integer or character")
    }
    faers_keep(object, primaryid = ids)
})

#############################################################
build_periods <- function(
    periods, years, quarters,
    arg_periods = rlang::caller_arg(periods),
    arg_years = rlang::caller_arg(years),
    arg_quarters = rlang::caller_arg(quarters),
    call = rlang::caller_env()) {
    if (is.null(years) && is.null(quarters)) {
        if (is.null(periods)) {
            cli::cli_abort("either both {.arg {arg_years}} and {.arg {arg_quarters}} {.arg {arg_periods}}", call = call)
        }
        return(periods)
    } else if (!is.null(years) && !is.null(quarters)) {
        assert_inclusive(quarters, faers_file_quarters,
            arg = arg_quarters, call = call
        )
        return(paste0(as.integer(years), quarters))
    } else if (!is.null(periods)) {
        cli::cli_abort(c(
            "{.arg {arg_periods}} can be used when both {.arg {arg_years}} and {.arg {arg_quarters}} are absent",
            i = "You should set both {.arg {arg_years}} and {.arg {arg_quarters}} or set {.arg {arg_periods}} only"
        ), call = call)
    }
    cli::cli_abort("both {.arg {arg_years}} and {.arg {arg_quarters}} should be set", call = call)
}

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
