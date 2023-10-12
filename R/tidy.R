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
#' `NULL`, all fields will be used. Note: use all fields will consume a lot of
#' memory.
#' @param all A scalar logical. If `TRUE` rows from all fields specified in
#' `use` will be included, if `FALSE`, no matching row with other fields will be
#' excluded. See [merge][data.table::merge] for details.
#' @export
#' @method faers_tidy FAERSascii
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSascii", function(object, use = NULL, all = TRUE) {
    use <- use_names_to_integer_indices(use, names(object@datatable))
    lst <- object@datatable[use]
    old_names <- c("isr", "case")
    new_names <- c("primaryid", "caseid")
    # check if drug_seq should be matched
    if (sum(names(lst) %in% c("indi", "ther", "drug")) >= 2L) {
        old_names <- c(old_names, c("indi_drug_seq", "dsg_drug_seq"))
        new_names <- c(new_names, c("drug_seq", "drug_seq"))
    }
    lapply(lst, function(x) {
        data.table::setnames(x, old_names, new_names, skip_absent = TRUE)
    })
    Reduce(function(x, y) {
        #  This defaults to the shared key columns between x and y 
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
    validate_ListOfFAERS(object)
    data.table::rbindlist(lapply(object, faers_tidy, ...),
        use.names = TRUE, fill = TRUE
    )
})
