#' FAERS class
#'
#' @description Provide a container for FAERS Quarterly Data file
#' @slot year An integer specifies the year information.
#' @slot quarter A string specifies the quarter information.
#' @slot data For `FAERSxml`, a [data.table][data.table::data.table]. For
#' `FAERSascii`, a list of [data.table][data.table::data.table].
#' @slot meddra A [data.table][data.table::data.table] or `NULL` representing
#' the meddra data used for standardization.
#' @slot format A string of "ascii" or "xml" indicates the file format used.
#' @slot deletedCases A list of integers, as of 2019 Quarter one there are new
#' files that lists deleted cases.
#' @details
#'  - `faers_data`: Extract the `data` slot.
#'  - `faers_year`: Extract the `year` slot.
#'  - `faers_quarter`: Extract the `quarter` slot.
#'  - `faers_period`: Extract the `period` slot (just Concatenate the year and
#'    quarter slot).
#'  - `faers_deleted_cases`: Extract the `deletedCases` slot.
#' @aliases FAERS
#' @name FAERS-class
NULL

#' @importClassesFrom data.table data.table
methods::setClassUnion("DTOrNull", c("NULL", "data.table"))

methods::setClass(
    "FAERS",
    slots = list(
        year = "integer",
        quarter = "character",
        data = "ANY",
        meddra = "DTOrNull",
        deduplication = "logical",
        standardization = "logical",
        format = "character"
    ),
    prototype = list(
        data = NULL, meddra = NULL,
        deduplication = FALSE,
        standardization = FALSE
    )
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

    if (length(object@standardization) != 1L) {
        return("@standardization must be a scalar logical")
    }
    if (length(object@deduplication) != 1L) {
        return("@deduplication must be a scalar logical")
    }

    if (!rlang::is_string(object@format, faers_file_format)) {
        return(sprintf(
            "`@format` must be a string of %s",
            oxford_comma(faers_file_format, final = "or")
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
        format = "ascii"
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
    slots = list(data = "DTOrNull", header = "list"),
    prototype = list(
        data = NULL,
        header = list(), format = "xml"
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
    general_msg <- sprintf(
        "FAERS data from %s Quarterly %s file%s",
        l, object@format, if (l > 1L) "s" else ""
    )
    if (object@standardization && object@deduplication) {
        prefix <- "Standardized and De-duplicated"
    } else if (object@standardization) {
        prefix <- "Standardized"
    } else if (object@deduplication) {
        prefix <- "De-duplicated"
    } else {
        prefix <- NULL
    }
    if (!is.null(prefix)) general_msg <- paste(prefix, general_msg, sep = " ")
    cat(general_msg, sep = "\n")
    invisible(object)
})

#' @export
#' @method show FAERSascii
#' @rdname FAERS-class
methods::setMethod("show", "FAERSascii", function(object) {
    methods::callNextMethod(object)
    n_reports <- nrow(object@data$demo)
    if (object@deduplication) {
        msg <- sprintf(
            "  Total unique report%s: %s",
            if (n_reports > 1L) "s" else "", n_reports
        )
    } else {
        msg <- sprintf(
            "  Total report%s: %s (with duplicates)",
            if (n_reports > 1L) "s" else "", n_reports
        )
    }
    cat(msg, sep = "\n")
    invisible(object)
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
#' @param object A [FAERS] object.
#' @param ... Other arguments passed to specific methods.
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
#' @aliases faers_year
#' @rdname FAERS-class
methods::setGeneric("faers_year", function(object) {
    methods::makeStandardGeneric("faers_year")
})

#' @export
#' @method faers_year FAERS
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
