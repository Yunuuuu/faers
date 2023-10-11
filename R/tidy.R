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

#' @param infos Index (integer, logistical, character all will be okay) to
#' select informations included in the final object. If `NULL`, all infomations
#' will be used, Note: this will take much memory.
#' @param all A scalar logical. If `TRUE` rows from all `infos` will be
#' included, if `FALSE`, no matching row with other infos will be excluded.
#' [merge][data.table::merge].
#' @export
#' @method faers_tidy FAERSascii
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSascii", function(object, infos = NULL, all = TRUE) {
    infos <- infos %||% seq_along(object@datatable)
    lst <- object@datatable[infos]
    lapply(lst, function(x) {
        data.table::setnames(
            x,
            c("isr", "case"), c("primaryid", "caseid"),
            skip_absent = TRUE
        )
    })
    Reduce(function(x, y) {
        merge(x, y, allow.cartesian = TRUE, all = all)
    }, lst)
})

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
