###########################################################
#' ListOfFAERS class
#'
#' Packed all [FAERSascii] or [FAERSxml] objects into a single object
#' @param x A list of [FAERSascii] or [FAERSxml] objects. Only one of `x` or
#' `type` should be provided. If you want to create a empty `ListOfFAERS`
#' object, just provide `type` argument.
#' @inheritParams faers_download
#' @slot container A list as the container to accommodate all
#' [FAERSascii] or [FAERSxml] objects.
#' @slot type A string of "ascii" or "xml" indicates the file type used.
#' @note For `ListOfFAERS` object, the type slot will determine the class of
#' objects in `container` slot, and can only be set when create `ListOfFAERS`
#' object using [ListOfFAERS]. You shouldn't change it manually with
#' [slot<-][methods::slot].
#' @seealso [ListOfFAERS-class]
#' @export
ListOfFAERS <- function(x, type) {
    with_value <- rlang::check_exclusive(x, type)
    if (with_value == "x") {
        assert_(x, is.list, "a list")
        type <- NULL
        for (allowed_type in c("ascii", "xml")) {
            if (all(isMatchedFAERS(x, allowed_type))) {
                type <- allowed_type
            }
        }
        if (is.null(type)) {
            cli::cli_abort(
                "All elements of {.arg x} must be of the same class, {.cls FAERSascii} or {.cls FAERSxml}"
            )
        }
        names(x) <- vapply(x, faers_period, character(1L))
    } else {
        type <- match.arg(type, faers_file_types)
    }
    methods::new("ListOfFAERS", container = x, type = type)
}

isMatchedFAERS <- function(x, type) {
    vapply(x, methods::is, logical(1L), class2 = paste0("FAERS", type))
}

#' @export
#' @aliases ListOfFAERS-class
#' @rdname ListOfFAERS
methods::setClass(
    "ListOfFAERS",
    slots = list(container = "list", type = "character"),
    prototype = list(container = list())
)

methods::setValidity("ListOfFAERS", function(object) {
    if (!rlang::is_named2(object@container)) {
        return("all elements in `@container` must be a named")
    }
    nms <- names(object@container)
    if (length(nms)) {
        mismatch_idx <- nms != vapply(
            object@container,
            faers_period, character(1L)
        )
        if (any(mismatch_idx)) {
            return(sprintf(
                "the names of `@container` must match the corresponded `period`, please check items %s",
                oxford_comma(nms[mismatch_idx])
            ))
        }
        if (!all(isMatchedFAERS(object@container, type = object@type))) {
            return(sprintf("all elements in `@container` must be of `FAERS%s`", object@type))
        }
    }
    TRUE
})

######################################################
#' @param object A `ListOfFAERS` object.
#' @importFrom methods show
#' @export
#' @method show ListOfFAERS
#' @rdname ListOfFAERS
methods::setMethod("show", "ListOfFAERS", function(object) {
    l <- length(object@container)
    cat(sprintf(
        "A total of %s FAERS Quarterly %s Data file%s",
        l, object@type, if (l > 1L) "s" else ""
    ), sep = "\n")
})

#######################################################
#' @param i Indices specifying elements to extract.
#' @export
#' @rdname ListOfFAERS
`[[.ListOfFAERS` <- function(object, i) {
    object@container[[use_indices(i, names(object@container))]]
}

#' @export
#' @rdname ListOfFAERS
`[.ListOfFAERS` <- function(object, i) {
    methods::new("ListOfFAERS",
        container = object@container[use_indices(i, names(object@container))],
        type = object@type
    )
}
