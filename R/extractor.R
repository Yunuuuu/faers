#######################################################
#' Extract elements from FAERS or ListOfFAERS objects
#'
#' @description
#'  - `faers_year`: Extract the year info, for [FAERS], an integer, for
#'    [ListOfFAERS], an atomic integer.
#'  - `faers_quarter`: Extract the quarter info, for [FAERS], a string, for
#'    [ListOfFAERS], an atomic character.
#'  - `faers_period`: Extract the period info (just Concatenate  the year and
#'    quarter info), for [FAERS], a string, for [ListOfFAERS], an atomic
#'    character.
#'  - `faers_data`: Extract `data` slot from a [FAERS] object or a specific
#'    [FAERS] object of [ListOfFAERS] object.
#'  - `faers_field`: Extract a FAERS data field from [FAERS] or [ListOfFAERS]
#'    object.
#'  - `faers_fields`: Extract multiple FAERS data fields from [FAERS] or
#'    [ListOfFAERS] object.
#'  - `faers_get`: Extract a specific [data.table][data.table::data.table] field
#'    from [FAERS] object or a specific [FAERS] object from [ListOfFAERS].
#'  - `faers_subset`: Extract a list of [data.table][data.table::data.table]
#'    fields from [FAERS] object or a subset of [ListOfFAERS] object by keeping
#'    selected [FAERS] objects.
#' @param object A [FAERS] or [ListOfFAERS] object.
#' @export
#' @aliases faers_year
#' @name FAERS-extractor
methods::setGeneric("faers_year", function(object) {
    methods::makeStandardGeneric("faers_year")
})

#' @export
#' @include class-FAERS.R
#' @method faers_year FAERS
#' @aliases faers_year
#' @rdname FAERS-extractor
methods::setMethod("faers_year", "FAERS", function(object) {
    object@year
})

#' @export
#' @include class-ListOfFAERS.R
#' @method faers_year ListOfFAERS
#' @aliases faers_year
#' @rdname FAERS-extractor
methods::setMethod("faers_year", "ListOfFAERS", function(object) {
    vapply(object@container, faers_year, integer(1L))
})

##################################################################
#' @export
#' @aliases faers_quarter
#' @rdname FAERS-extractor
methods::setGeneric("faers_quarter", function(object) {
    methods::makeStandardGeneric("faers_quarter")
})

#' @export
#' @method faers_quarter FAERS
#' @aliases faers_quarter
#' @rdname FAERS-extractor
methods::setMethod("faers_quarter", "FAERS", function(object) {
    object@quarter
})

#' @export
#' @method faers_quarter ListOfFAERS
#' @aliases faers_quarter
#' @rdname FAERS-extractor
methods::setMethod("faers_quarter", "ListOfFAERS", function(object) {
    vapply(object@container, faers_quarter, character(1L))
})

#################################################################
#' @export
#' @aliases faers_period
#' @rdname FAERS-extractor
methods::setGeneric("faers_period", function(object) {
    methods::makeStandardGeneric("faers_period")
})

#' @export
#' @method faers_period FAERS
#' @aliases faers_period
#' @rdname FAERS-extractor
methods::setMethod("faers_period", "FAERS", function(object) {
    paste0(object@year, object@quarter)
})

#' @export
#' @method faers_period ListOfFAERS
#' @aliases faers_period
#' @rdname FAERS-extractor
methods::setMethod("faers_period", "ListOfFAERS", function(object) {
    names(object@container)
})


#################################################################
#' @export
#' @rdname FAERS-extractor
methods::setGeneric("faers_data", function(object, ...) {
    methods::makeStandardGeneric("faers_data")
})

#' @export
#' @method faers_data FAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_data", "FAERS", function(object) {
    object@data
})

#' @export
#' @method faers_data ListOfFAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_data", "ListOfFAERS", function(object, year = NULL, quarter = NULL, period = NULL) {
    faers_data(object[[year = year, quarter = quarter, period = period]])
})

##############################################################
#' @export
#' @rdname FAERS-extractor
methods::setGeneric("faers_get", function(object, ...) {
    methods::makeStandardGeneric("faers_get")
})

#' @param field,fields A string or an atomic character indicates the FAERS
#' fields to used. Only values "demo", "drug", "indi", "ther", "reac", "rpsr",
#' and "outc" can be used.
#' @export
#' @method faers_get FAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_get", "FAERS", function(object, field) {
    field <- match.arg(field, faers_ascii_file_fields)
    object@data[[field]]
})

#' @param year,years A number or an atomic numeric coerced to integer indicates
#' the years of FAERS data to used.
#' @param quarter,quarters A string or an atomic character indicates the
#' quarters of FAERS data to used.
#' @param period,periods A string or an atomic character indicates the periods
#' of FAERS data to used.
#' @export
#' @method faers_get ListOfFAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_get", "ListOfFAERS", function(object, year = NULL, quarter = NULL, period = NULL) {
    assert_length(year, 1L)
    assert_string(quarter, empty_ok = FALSE, null_ok = TRUE)
    assert_string(period, empty_ok = FALSE, null_ok = TRUE)
    period <- build_periods(period, year, quarter)
    if (!any(period == faers_period(object))) {
        cli::cli_abort("Cannot find {period}")
    }
    object@container[[period]]
})

#############################################################
#' @export
#' @rdname FAERS-extractor
methods::setGeneric("faers_subset", function(object, ...) {
    methods::makeStandardGeneric("faers_subset")
})

#' @export
#' @method faers_subset FAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_subset", "FAERS", function(object, fields) {
    assert_inclusive(fields, faers_ascii_file_fields)
    object@data[fields]
})

#' @export
#' @method faers_subset ListOfFAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_subset", "ListOfFAERS", function(object, years = NULL, quarters = NULL, periods = NULL) {
    periods <- build_periods(periods, years, quarters)
    missed_periods <- setdiff(periods, faers_period(object))
    if (length(missed_periods)) {
        cli::cli_abort("Cannot find {missed_periods}")
    }
    methods::new("ListOfFAERS",
        container = object@container[periods], type = object@type
    )
})


#################################################################
#' @param ... Other arguments passed to specific methods.
#' @export
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setGeneric("faers_field", function(object, ...) {
    methods::makeStandardGeneric("faers_field")
})

#' @export
#' @method faers_field FAERS
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setMethod("faers_field", "FAERS", function(object, field) {
    out <- faers_get(object, field)
    out$year <- faers_year(object)
    out$quarter <- faers_quarter(object)
    data.table::setcolorder(out, c("year", "quarter"), before = 1L)
    out
})

#' @param combine If `TRUE`, will combine the final list into a
#' [data.table][data.table::data.table].
#' @export
#' @method faers_field ListOfFAERS
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setMethod("faers_field", "ListOfFAERS", function(object, field, combine = TRUE) {
    out <- lapply(object@container, faers_field, field = field)
    if (isTRUE(combine)) {
        out <- data.table::rbindlist(out, fill = TRUE, use.names = TRUE)
    } else {
        data.table::setattr(out, "names", faers_period(object))
    }
    out
})

#################################################################
#' @export
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setGeneric("faers_fields", function(object, ...) {
    methods::makeStandardGeneric("faers_fields")
})

#' @export
#' @method faers_fields FAERS
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setMethod("faers_fields", "FAERS", function(object, fields = NULL) {
    if (is.null(fields)) {
        fields <- faers_ascii_file_fields
    } else {
        assert_inclusive(fields, faers_ascii_file_fields)
        fields <- unique(fields)
    }
    out <- lapply(fields, faers_field, object = object)
    data.table::setattr(out, "names", fields)
    out
})

#' @export
#' @method faers_fields ListOfFAERS
#' @aliases faers_fields
#' @rdname FAERS-extractor
methods::setMethod("faers_fields", "ListOfFAERS", function(object, fields = NULL, combine = TRUE) {
    if (isTRUE(combine)) {
        if (is.null(fields)) {
            fields <- faers_ascii_file_fields
        } else {
            assert_inclusive(fields, faers_ascii_file_fields)
            fields <- unique(fields)
        }
        out <- lapply(fields, faers_field, object = object, combine = combine)
        data.table::setattr(out, "names", fields)[]
    } else {
        lapply(object@container, faers_fields, fields = fields)
    }
})

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
