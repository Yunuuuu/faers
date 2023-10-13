###########################################################
#' Create ListOfFAERS class
#'
#' Packed all [FAERSascii] or [FAERSxml] objects into a single object
#' @param x A list of [FAERSascii] or [FAERSxml] objects. Only one of `x` or
#' `type` should be provided. If you want to create a empty `ListOfFAERS`
#' object, just provide `type` argument.
#' @inheritParams faers_download
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

#' @slot container: An list as the container to accommodate all
#' [FAERSascii] or [FAERSxml] objects.
#' @slot type: A string of "ascii" or "xml" indicates the file type used.
#' @note For `ListOfFAERS` object, the type slot will determine the class of
#' objects in `container` slot, and can only be set when create `ListOfFAERS`
#' object using [ListOfFAERS]. You shouldn't change it manually with
#' [@@][methods::getSlots].
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
        mismatch_idx <- nms != vapply(x, faers_period, character(1L))
        if (any(mismatch_idx)) {
            return(sprintf(
                "the names of `@container` must match the corresponded `period`, please check items %s",
                oxford_comma(nms[mismatch_idx])
            ))
        }
        if (!all(isMatchedFAERS(object@containers, type = object@type))) {
            return(sprintf("all elements in `@containers` must be of `FAERS%s`", object@type))
        }
    }
    TRUE
})

######################################################
#' @param object A [ListOfFAERS] object.
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

##############################################################
#' @param year,years A number or an atomic numeric coerced to integer indicates
#' the years of FAERS data to used.
#' @param quarter,quarters A string or an atomic character indicates the
#' quarters of FAERS data to used.
#' @param period,periods A string or an atomic character indicates the periods
#' of FAERS data to used.
#' @export
#' @rdname ListOfFAERS
`[[.ListOfFAERS` <- function(object, year = NULL, quarter = NULL, period = NULL) {
    assert_length(year, 1L)
    assert_string(quarter, empty_ok = FALSE, null_ok = TRUE)
    assert_string(period, empty_ok = FALSE, null_ok = TRUE)
    period <- build_periods(period, year, quarter)
    if (!any(period == faers_period(object))) {
        cli::cli_abort("Cannot find {period}")
    }
    object@container[[period]]
}

#' @export
#' @rdname ListOfFAERS
`[.ListOfFAERS` <- function(object, years = NULL, quarters = NULL, periods = NULL) {
    periods <- build_periods(periods, years, quarters)
    missed_periods <- setdiff(periods, faers_period(object))
    if (length(missed_periods)) {
        cli::cli_abort("Cannot find {missed_periods}")
    }
    methods::new("ListOfFAERS",
        container = object@container[periods], type = object@type
    )
}

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
