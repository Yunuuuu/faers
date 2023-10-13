#######################################################
#' @param value For [faers_year], a numeric coerced to integer, for
#' [faers_quarter], a string.
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
methods::setMethod("faers_period", "FAERS", function(object) {
    names(object@container)
})


#################################################################
#' Extract data slot of a [FAERS] object
#'
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


#' @inheritParams [[.ListOfFAERS
#' @export
#' @method faers_data ListOfFAERS
#' @rdname FAERS-extractor
methods::setMethod("faers_data", "ListOfFAERS", function(object, year = NULL, quarter = NULL, period = NULL) {
    faers_data(object[[year = year, quarter = quarter, period = period]])
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
    assert_string(field, empty_ok = FALSE, na_ok = FALSE)
    assert_inclusive(field, faers_ascii_file_fields)
    object@data[[field]]
})

#' @inheritParams faers_data
#' @export
#' @method faers_field ListOfFAERS
#' @aliases faers_field
#' @rdname FAERS-extractor
methods::setMethod("faers_field", "ListOfFAERS", function(object, field, year = NULL, quarter = NULL, period = NULL) {
    assert_string(field, empty_ok = FALSE, na_ok = FALSE)
    assert_inclusive(field, faers_ascii_file_fields)
    faers_field(faers_data(
        object,
        year = year, quarter = quarter, period = period
    ), field)
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
methods::setMethod("faers_fields", "FAERS", function(object, fields) {
    assert_inclusive(fields, faers_ascii_file_fields)
    object@data[fields]
})
