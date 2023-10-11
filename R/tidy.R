#' Tidy up faers datatable
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods.
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
    Reduce(function(x, y) {
        merge(x, y, allow.cartesian = TRUE, all = all)
    }, object@datatable[infos])
})

#' @export
#' @method faers_tidy FAERSxml
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSxml", function(object) {
    object@datatable
})
