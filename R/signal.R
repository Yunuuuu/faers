#' Create contingency table and run disproportionality analysis
#' @details
#'  - `faers_phv_table`: build a contingency table for all events in
#'    `interested_event`.
#'  - `faers_phv_signal`: Pharmacovigilance Analysis used contingency table
#'    constructed with `faers_phv_table`.
#' @param object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods.
#'  - `faers_phv_table`: other arguments passed to [faers_counts].
#'  - `faers_phv_signal`: other arguments passed to `faers_phv_table`.
#' @return A [data.table][data.table::data.table] object.
#' @examples
#' # you must change `dir`, as the files included in the package are sampled
#' data <- faers(c(2004, 2017), c("q1", "q2"),
#'     dir = system.file("extdata", package = "faers")
#' )
#' \dontrun{
#' # you must standardize and deduplication before disproportionality analysis
#' # you should replace `meddra_path` with yours
#' data <- faers_standardize(data, meddra_path)
#' data <- faers_dedup(data)
#' faers_phv_table(data,
#'     filter_params = list(field = "demo", .fn = function(x) {
#'         sample(x$primaryid, 100L)
#'     })
#' )
#' faers_phv_signal(data,
#'     filter_params = list(field = "demo", .fn = function(x) {
#'         sample(x$primaryid, 100L)
#'     })
#' )
#' }
#' @export
#' @aliases faers_phv_table
#' @name faers_phv_signal
methods::setGeneric(
    "faers_phv_table",
    function(object, ..., interested, object2) {
        methods::makeStandardGeneric("faers_phv_table")
    }
)


#' @param filter_params Other arguments passed to [faers_filter], solely used
#' when `interested` and `object2` are both `missing`
#' @param interested A [FAERSascii] object, containing information pertaining to
#' the relevant exposure, typically involving pharmaceutical substances, should
#' constitute a subset of the overarching "object". In the event that both
#' "interested" and "object2" are absent, the [faers_filter] function will be
#' invoked to selectively procure data concerning the interested exposure from
#' the "object". Subsequently, the acquired "interested" subset will be once
#' again channeled into `faers_phv_table`. The parameters "n11" or "a" will then
#' be computed based on the contents of "interested".
#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "missing", object2 = "missing"),
    function(object, ..., filter_params = list(), interested, object2) {
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        interested <- do.call(
            faers_filter,
            c(list(object = object), filter_params)
        )
        faers_phv_table(object = object, ..., interested = interested)
    }
)

#' @inheritParams faers_counts
#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "missing"),
    function(object, interested_field = "reac", interested_event = "soc_name", ..., interested, object2) {
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        if (!interested@standardization) {
            cli::cli_abort("{.arg interested} must be standardized using {.fn faers_standardize}")
        }
        full_primaryids <- faers_primaryid(object)
        interested_primaryids <- faers_primaryid(interested)
        if (!all(interested_primaryids %in% full_primaryids)) {
            cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg object}")
        }
        full_counts <- faers_counts(
            object,
            interested_field = interested_field,
            interested_event = interested_event, ...
        )
        interested_counts <- faers_counts(
            interested,
            interested_field = interested_field,
            interested_event = interested_event, ...
        )
        n <- sum(full_counts$N) # scalar
        n1. <- sum(interested_counts$N) # scalar
        data.table::setnames(full_counts, "N", "n.1")
        data.table::setnames(interested_counts, "N", "a")
        out <- merge(full_counts, interested_counts,
            by = interested_event, all = TRUE, allow.cartesian = TRUE
        )
        out[, a := data.table::fifelse(is.na(a), 0L, a)] # nolint
        out[, b := n1. - a] # nolint
        out[, c := n.1 - a] # nolint
        out[, d := n - (n1. + n.1 - a)] # nolint
        out <- out[, !"n.1"]
        data.table::setcolorder(out, c(interested_event, "a", "b", "c", "d"))[]
    }
)

#' @param object2 A [FAERSascii] object with data from another interested drug,
#' In this way, `object` and `object2` should be not overlapped. The value `n11`
#' or `a` will be calculated from `object`
#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "missing", object2 = "FAERSascii"),
    function(object, interested_field = "reac", interested_event = "soc_name", ..., interested, object2) {
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        if (!object2@standardization) {
            cli::cli_abort("{.arg object2} must be standardized using {.fn faers_standardize}")
        }
        primaryids <- faers_primaryid(object)
        primaryids2 <- faers_primaryid(object2)
        overlapped_idx <- primaryids %in% primaryids2
        if (any(overlapped_idx)) {
            cli::cli_warn("{.val {overlapped_idx}} report{?s} are overlapped between {.arg object} and {.arg object2}")
        }
        interested_counts <- faers_counts(object,
            interested_field = interested_field,
            interested_event = interested_event, ...
        )
        interested_counts2 <- faers_counts(object2,
            interested_field = interested_field,
            interested_event = interested_event, ...
        )
        n1. <- sum(interested_counts$N)
        n0. <- sum(interested_counts2$N)
        data.table::setnames(interested_counts, "N", "a")
        data.table::setnames(interested_counts2, "N", "c")
        out <- merge(interested_counts, interested_counts2,
            by = interested_event, all = TRUE, allow.cartesian = TRUE
        )
        out[, c("a", "c") := lapply(.SD, function(x) {
            data.table::fifelse(is.na(x), 0L, x)
        }), .SDcols = c("a", "c")]
        out[, b := n1. - a] # nolint
        out[, d := n0. - c] # nolint
        data.table::setcolorder(out, c(interested_event, "a", "b", "c", "d"))[]
    }
)

utils::globalVariables(c("a", "b", "d", "n.1"))

##############################################################
#' @export
#' @rdname faers_phv_signal
methods::setGeneric("faers_phv_signal", function(object, ...) {
    methods::makeStandardGeneric("faers_phv_signal")
})

#' @param phv_signal_params Other arguments passed to [phv_signal].
#' @seealso [phv_signal]
#' @method faers_phv_signal FAERSascii
#' @rdname faers_phv_signal
methods::setMethod("faers_phv_signal", "FAERSascii", function(object, ..., phv_signal_params = list()) {
    assert_(phv_signal_params, is.list, "a list")
    out <- faers_phv_table(object, ...)
    cbind(out, do.call(
        phv_signal,
        c(out[, .SD, .SDcols = c("a", "b", "c", "d")], phv_signal_params)
    ))
})
