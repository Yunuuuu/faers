#' FAERS and ListOfFAERS class
#'
#' @slot year An integer specifies the year information.
#' @slot quarter A string specifies the quarter information.
#' @slot data For `FAERSxml`, a [data.table][data.table::data.table]. For
#' `FAERSascii`, a list of [data.table][data.table::data.table].
#' @slot type: A string of "ascii" or "xml" indicates the file type used.
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
validate_faers <- function(object) {
    if (length(object@year) != 1L) {
        return("`@year` must be a scalar integer")
    }
    if (!rlang::is_string(object@quarter, faers_file_quarters)) {
        return(sprintf(
            "`@quarter` must be a string of %s",
            oxford_comma(faers_file_quarters, final = "or")
        ))
    }
    if (!rlang::is_string(object@type, faers_file_types)) {
        return(sprintf(
            "`@type` must be a string of %s",
            oxford_comma(faers_file_types, final = "or")
        ))
    }
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
    cur_data <- object@data
    if (length(cur_data) &&
        !all(vapply(cur_data, data.table::is.data.table, logical(1L)))) {
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
    cat(sprintf(
        "FAERS Quarterly %s Data of %s",
        object@type, faers_period(object)
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
