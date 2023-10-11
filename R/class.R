#' FAERS classes
#'
#' @slot year: An integer specifies the year information.
#' @slot quarter: A string specifies the quarter information.
#' @slot datatable: For FAERSxml, a [data.table][data.table::data.table], for
#' FAERSascii, a list of [data.table][data.table::data.table].
#' @aliases FAERS
#' @name FAERS-class
methods::setClass(
    "FAERS",
    slots = list(
        year = "integer",
        quarter = "character",
        datatable = "ANY"
    ),
    prototype = list(
        year = NA_integer_,
        quarter = NA_character_,
        datatable = NULL
    )
)

## Validator for FAERS
check_faers <- function(object) {
    if (length(object@year) != 1L) {
        return("`@year` must be a scalar integer")
    }
    if (length(object@quarter) != 1L) {
        return("`@quarter` must be a string")
    }
    if (!any(object@quarter == available_quarter)) {
        return(sprintf(
            "Only %s are available in `@quarter`",
            oxford_comma(available_quarter)
        ))
    }
    TRUE
}
methods::setValidity("FAERS", check_faers)

#' @aliases FAERSascii
#' @rdname FAERS-class
methods::setClass(
    "FAERSascii",
    slots = list(datatable = "list"),
    prototype = list(datatable = list()),
    contains = "FAERS"
)
methods::setValidity("FAERSascii", function(object) {
    cur_table <- datatable(object)
    if (length(cur_table) &&
        !all(vapply(cur_table, data.table::is.data.table, logical(1L)))) {
        return("@datatable must be a list of data.table")
    }
    check_faers(object)
})

#' @aliases FAERSxml
#' @rdname FAERS-class
methods::setClass(
    "FAERSxml",
    slots = list(datatable = "data.table"),
    prototype = list(datatable = data.table::data.table()),
    contains = "FAERS"
)

######################################################
#' @param object A [FAERS] object.
#' @param value For [year], a numeric coerced to integer, for [quarter], a
#' string.
#' @importFrom methods show
#' @export
#' @method show FAERS
#' @rdname FAERS-class
methods::setMethod("show", "FAERS", function(object) {
    cat(sprintf(
        "FDA Adverse Event Reporting System Quarterly Data of %s",
        paste0(object@year, object@quarter)
    ), sep = "\n")
})

#######################################################
methods::setGeneric("year", function(object) {
    methods::makeStandardGeneric("year")
})
methods::setGeneric("year<-", function(object, value) {
    methods::makeStandardGeneric("year<-")
})

#' @export
#' @method year FAERS
#' @aliases year
#' @rdname FAERS-class
methods::setMethod("year", "FAERS", function(object) {
    object@year
})

#' @export
#' @method year<- FAERS
#' @aliases year<-
#' @rdname FAERS-class
methods::setMethod("year<-", "FAERS", function(object, value) {
    object@year <- as.integer(value)
    methods::validObject(object)
    invisible(object)
})

##################################################################
methods::setGeneric("quarter", function(object) {
    methods::makeStandardGeneric("quarter")
})
methods::setGeneric("quarter<-", function(object, value) {
    methods::makeStandardGeneric("quarter<-")
})

#' @export
#' @method quarter FAERS
#' @aliases quarter
#' @rdname FAERS-class
methods::setMethod("quarter", "FAERS", function(object) {
    object@quarter
})

#' @aliases quarter<-
#' @method quarter<- FAERS
#' @export
#' @rdname FAERS-class
methods::setMethod("quarter<-", "FAERS", function(object, value) {
    object@quarter <- as.character(value)
    methods::validObject(object)
    invisible(object)
})

#################################################################
methods::setGeneric("datatable", function(object) {
    methods::makeStandardGeneric("datatable")
})
methods::setGeneric("datatable<-", function(object, value) {
    methods::makeStandardGeneric("datatable<-")
})

#' @export
#' @method datatable FAERS
#' @aliases datatable
#' @rdname FAERS-class
methods::setMethod("datatable", "FAERS", function(object) {
    object@datatable
})

#' @export
#' @method datatable<- FAERS
#' @aliases datatable<-
#' @rdname FAERS-class
methods::setMethod("datatable<-", "FAERS", function(object, value) {
    object@datatable <- value
    methods::validObject(object)
    invisible(object)
})
