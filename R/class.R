#' FAERS classes
#'
#' @slot year: An integer specifies the year information.
#' @slot quarter: A string specifies the quarter information.
#' @slot datatable: For `FAERSxml`, a [data.table][data.table::data.table]. For
#' `FAERSascii`, a list of [data.table][data.table::data.table].
#' @aliases FAERS
#' @name FAERS-class
methods::setClass(
    "FAERS",
    slots = list(
        year = "integer",
        quarter = "character",
        datatable = "ANY",
        type = "character"
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
    if (!rlang::is_string(object@quarter, faers_quarter)) {
        return(sprintf(
            "`@quarter` must be a string of %s",
            oxford_comma(faers_quarter, final = "or")
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

methods::setValidity("FAERS", check_faers)

#' @aliases FAERSascii
#' @rdname FAERS-class
methods::setClass(
    "FAERSascii",
    slots = list(datatable = "list"),
    prototype = list(datatable = list(), type = "ascii"),
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
    slots = list(datatable = "data.table", header = "list"),
    prototype = list(
        datatable = data.table::data.table(),
        header = list(), type = "xml"
    ),
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
        "FAERS Quarterly %s Data of %s",
        object@type, paste0(object@year, object@quarter)
    ), sep = "\n")
})

#######################################################
#' @export
#' @aliases year
#' @rdname FAERS-class
methods::setGeneric("year", function(object) {
    methods::makeStandardGeneric("year")
})

#' @export
#' @aliases year<-
#' @rdname FAERS-class
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
#' @export
#' @aliases quarter
#' @rdname FAERS-class
methods::setGeneric("quarter", function(object) {
    methods::makeStandardGeneric("quarter")
})

#' @aliases quarter<-
#' @export
#' @rdname FAERS-class
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

#' @export
#' @method quarter<- FAERS
#' @aliases quarter<-
#' @rdname FAERS-class
methods::setMethod("quarter<-", "FAERS", function(object, value) {
    object@quarter <- as.character(value)
    methods::validObject(object)
    invisible(object)
})

#################################################################
#' @export
#' @aliases datatable
#' @rdname FAERS-class
methods::setGeneric("datatable", function(object) {
    methods::makeStandardGeneric("datatable")
})
#' @export
#' @aliases datatable<-
#' @rdname FAERS-class
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

#######################################################
#' @export
#' @aliases header
#' @rdname FAERS-class
methods::setGeneric("header", function(object) {
    methods::makeStandardGeneric("header")
})

#' @export
#' @aliases header<-
#' @rdname FAERS-class
methods::setGeneric("header<-", function(object, value) {
    methods::makeStandardGeneric("header<-")
})

#' @export
#' @method header FAERSxml
#' @aliases header
#' @rdname FAERS-class
methods::setMethod("header", "FAERSxml", function(object) {
    object@header
})

#' @export
#' @method header<- FAERSxml
#' @aliases header<-
#' @rdname FAERS-class
methods::setMethod("header<-", "FAERSxml", function(object, value) {
    object@year <- as.integer(value)
    methods::validObject(object)
    invisible(object)
})

###########################################################
#' Create ListOfFAERS class
#'
#' Packed a list of [FAERSascii] or [FAERSxml] objects into a single object
#' @param x A list of [FAERSascii] or [FAERSxml] objects.
#' @return A `ListOfFAERS` object
#' @export 
ListOfFAERS <- function(x) {
    assert_(x, is.list, "a list")
    type <- build_ListOfFAERS_type(x)
    if (is.null(type)) abort_ListOfFAERS()
    new_ListOfFAERS(x, type)
}

#' @param x A [ListOfFAERS] object.
#' @param ... Not used currently.
#' @export
#' @rdname ListOfFAERS
print.ListOfFAERS <- function(x, ...) {
    cat(sprintf(
        "A total of %s FAERS Quarterly %s Data file%s",
        length(x), attr(x, "type"), if (length(x) > 1L) "s" else ""
    ), sep = "\n")
}

new_ListOfFAERS <- function(x, type) {
    structure(x, type = type, class = "ListOfFAERS")
}

build_ListOfFAERS_type <- function(x) {
    for (allowed_faers in c("FAERSascii", "FAERSxml")) {
        if (all(vapply(x, methods::is, logical(1L), class2 = allowed_faers))) {
            return(str_remove(allowed_faers, "^FAERS"))
        }
    }
    NULL
}

validate_ListOfFAERS <- function(x) {
    actual_type <- attr(x, "type")
    if (!rlang::is_string(actual_type, faers_file_types)) {
        cli::cli_abort(c(
            "Invalid {.cls ListOfFAERS} object",
            i = sprintf(
                "{.filed type} attribute must be a string of %s",
                oxford_comma(style_val(faers_file_types), final = "or")
            )
        ))
    }
    type <- build_ListOfFAERS_type(x)
    if (is.null(type)) abort_ListOfFAERS()
    if (type != actual_type) {
        cli::cli_abort(sprintf(
            "For %s, only {.cls FAERS%s} object are allowed",
            actual_type, actual_type
        ))
    }
}

abort_ListOfFAERS <- function() {
    cli::cli_abort("values in {.cls ListOfFAERS} must be all {.cls FAERSascii} or {.cls FAERSxml}")
}
