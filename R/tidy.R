#' Tidy up faers datatable
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods. For [ListOfFAERS]
#' method, other arguments passed to [FAERSascii] or [FAERSxml] method.
#' @return A [data.table][data.table::data.table] object.
#' @export
#' @name faers_tidy
methods::setGeneric("faers_tidy", function(object, ...) {
    methods::makeStandardGeneric("faers_tidy")
})

#' @param use A character or integer vector specifying the fields to use. If
#' `NULL`, all fields will be used.
#' @param all A scalar logical. If `TRUE` rows from all `infos` will be
#' included, if `FALSE`, no matching row with other infos will be excluded.
#' [merge][data.table::merge].
#' @export
#' @method faers_tidy FAERSascii
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSascii", function(object, use = NULL, all = TRUE) {
    use <- use_names_to_integer_indices(use, names(object@datatable))
    lst <- object@datatable[use]
    lapply(lst, function(x) {
        data.table::setnames(
            x,
            c("isr", "case"), c("primaryid", "caseid"),
            skip_absent = TRUE
        )
    })
    # check if drug_seq should matched
    if (sum(names(lst) %in% c("indi", "ther", "drug")) >= 2L) {
        lapply(lst, function(x) {
            data.table::setnames(
                x, c("indi_drug_seq", "dsg_drug_seq"),
                c("drug_seq", "drug_seq"),
                skip_absent = TRUE
            )
        })
    }
    Reduce(function(x, y) {
        merge(x, y, allow.cartesian = TRUE, all = all)
    }, lst)
})

use_names_to_integer_indices <- function(use, names, arg = rlang::caller_arg(use), call = rlang::caller_env()) {
    if (is.null(use)) {
        return(seq_along(names))
    }
    if (anyNA(use)) {
        rlang::abort(
            sprintf("%s cannot contain `NA`", format_arg(arg)),
            call = call
        )
    }
    if (is.character(use)) {
        use <- match(use, names)
        if (anyNA(use)) {
            rlang::abort(sprintf(
                "%s contains invalid values", format_arg(arg)
            ), call = call)
        }
    } else if (is.numeric(use)) {
        if (any(use < 1L) || any(use > length(names))) {
            rlang::abort(sprintf(
                "%s contains out-of-bounds indices", format_arg(arg)
            ), call = call)
        }
    } else {
        rlang::abort(
            sprintf(
                "%s must be an atomic numeic/character or `NULL`",
                format_arg(arg)
            ),
            call = call
        )
    }
    use
}

# define_common_by <- function(x) {
#     out <- NULL
#     nms <- names(x)
#     out <- c(out, choose_one(nms, c("primaryid", "isr")))
#     out <- c(out, choose_one(nms, c("caseid", "case")))
#     out
# }
# choose_one <- function(all, choices) {
#     for (choice in choices) {
#         if (any(choice == all)) {
#             return(choice)
#         }
#     }
#     cli::cli_abort("One of {.val {choices}} must exist")
# }

#' @export
#' @method faers_tidy FAERSxml
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSxml", function(object) {
    object@datatable
})

methods::setOldClass("ListOfFAERS")

#' @export
#' @method faers_tidy ListOfFAERS
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "ListOfFAERS", function(object, ...) {
    data.table::rbindlist(lapply(object, faers_tidy, ...),
        use.names = TRUE, fill = TRUE
    )
})
