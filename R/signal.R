#' Create contingency table and run disproportionality analysis
#' @details
#'  - `faers_phv_table`: build a contingency table for all events in
#'    `.events`.
#'  - `faers_phv_signal`: Pharmacovigilance Analysis used contingency table
#'    constructed with `faers_phv_table`. You must pass `.full` or `.object2`
#'    into `faers_phv_table`.
#' @param .object A [FAERSascii] object. The unique number of `primaryids` from
#' `.object` will be regarded as `n1.`.
#' @param ... Other arguments passed to specific methods.
#'  - `faers_phv_table`: other arguments passed to [faers_counts].
#'  - `faers_phv_signal`: other arguments passed to `faers_phv_table`.
#' @return A [data.table][data.table::data.table] object.
#' @examples
#' # you must change `dir`, as the files included in the package are sampled
#' data <- faers(c(2004, 2017), c("q1", "q2"),
#'     dir = system.file("extdata", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' \dontrun{
#' # you must standardize and deduplication before disproportionality analysis
#' # you should replace `meddra_path` with yours
#' data <- faers_standardize(data, meddra_path)
#' data <- faers_dedup(data)
#' # we use faers_filter() to extract data we are interested
#' # here, we just sample 100 reports. You should do it based on your purpose.
#' faers_phv_signal(
#'     faers_filter(data, .fn = ~ sample(faers_primaryid(.x), 100L)),
#'     .full = data
#' )
#' }
#' @export
#' @aliases faers_phv_table
#' @name faers_phv_signal
methods::setGeneric(
    "faers_phv_table",
    function(.object, ..., .full, .object2) {
        rlang::check_exclusive(.full, .object2)
        methods::makeStandardGeneric("faers_phv_table")
    }
)

#' @param .full A [FAERSascii] object with data from full data. In this way,
#' `.object` must be a subset of `.full`. The unique number of `primaryids` from
#' `.full` will be regarded as `n`.
#' @inheritParams faers_counts
#' @export
#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(.object = "FAERSascii", .full = "FAERSascii", .object2 = "missing"),
    function(.object, .events = "soc_name", ..., .full, .object2) {
        if (!.object@standardization) {
            cli::cli_abort("{.arg .object} must be standardized using {.fn faers_standardize}")
        }
        if (!.full@standardization) {
            cli::cli_abort("{.arg .full} must be standardized using {.fn faers_standardize}")
        }
        full_primaryids <- faers_primaryid(.full)
        interested_primaryids <- faers_primaryid(.object)
        if (!all(interested_primaryids %chin% full_primaryids)) {
            cli::cli_abort("Provided {.arg interested} data must be a subset of {.arg .object}")
        }
        full_counts <- faers_counts(.full, .events = .events, ...)
        interested_counts <- faers_counts(.object, .events = .events, ...)
        # It's not necessary to call unique for de-duplicated data
        # Don't use `sum(full_counts$N)` or `sum(interested_counts$N)`, since
        # they will include duplicated primaryids (patients)
        n <- length(unique(full_primaryids))
        n1. <- length(unique(interested_primaryids))
        data.table::setnames(full_counts, "N", "n.1")
        data.table::setnames(interested_counts, "N", "a")
        out <- merge(full_counts, interested_counts,
            by = .events, all = TRUE, allow.cartesian = TRUE
        )
        out[, a := data.table::fifelse(is.na(a), 0L, a)] # nolint
        out[, b := n1. - a] # nolint
        out[, c := n.1 - a] # nolint
        out[, d := n - (n1. + n.1 - a)] # nolint
        out <- out[, !"n.1"]
        data.table::setcolorder(out, c(.events, "a", "b", "c", "d"))[]
    }
)

#' @param .object2 A [FAERSascii] object with data from another interested drug,
#' In this way, `.object` and `.object2` should not be overlapped. The unique
#' number of `primaryids` from `object2` will be regarded as `n0.`.
#' @rdname faers_phv_signal
#' @export
methods::setMethod(
    "faers_phv_table",
    c(.object = "FAERSascii", .full = "missing", .object2 = "FAERSascii"),
    function(.object, .events = "soc_name", ..., .full, .object2) {
        if (!.object@standardization) {
            cli::cli_abort("{.arg .object} must be standardized using {.fn faers_standardize}")
        }
        if (!.object2@standardization) {
            cli::cli_abort("{.arg .object2} must be standardized using {.fn faers_standardize}")
        }
        primaryids <- faers_primaryid(.object)
        primaryids2 <- faers_primaryid(.object2)
        overlapped_idx <- primaryids %chin% primaryids2
        if (any(overlapped_idx)) {
            cli::cli_warn("{.val {sum(overlapped_idx)}} report{?s} are overlapped between {.arg .object} and {.arg .object2}")
        }
        interested_counts <- faers_counts(.object, .events = .events, ...)
        interested_counts2 <- faers_counts(.object2, .events = .events, ...)
        # It's not necessary to call unique for de-duplicated data
        n1. <- length(unique(primaryids))
        n0. <- length(unique(primaryids2))
        data.table::setnames(interested_counts, "N", "a")
        data.table::setnames(interested_counts2, "N", "c")
        out <- merge(interested_counts, interested_counts2,
            by = .events, all = TRUE, allow.cartesian = TRUE
        )
        out[, c("a", "c") := lapply(.SD, function(x) {
            data.table::fifelse(is.na(x), 0L, x)
        }), .SDcols = c("a", "c")]
        out[, b := n1. - a] # nolint
        out[, d := n0. - c] # nolint
        data.table::setcolorder(out, c(.events, "a", "b", "c", "d"))[]
    }
)

utils::globalVariables(c("a", "b", "d", "n.1"))

##############################################################
#' @export
#' @rdname faers_phv_signal
methods::setGeneric("faers_phv_signal", function(.object, ...) {
    methods::makeStandardGeneric("faers_phv_signal")
})

#' @param .methods Just an alias of `method` in [phv_signal].
#' @param .phv_signal_params Other arguments passed to [phv_signal].
#' @inheritParams phv_signal
#' @seealso [phv_signal]
#' @export
#' @method faers_phv_signal FAERSascii
#' @rdname faers_phv_signal
methods::setMethod("faers_phv_signal", "FAERSascii", function(.object, .methods = NULL, ..., .phv_signal_params = list(), BPPARAM = SerialParam()) {
    assert_(.phv_signal_params, is.list, "a list")
    out <- faers_phv_table(.object = .object, ...)
    .__signal__. <- do.call(
        phv_signal, c(
            out[, c("a", "b", "c", "d")],
            list(methods = .methods, BPPARAM = BPPARAM),
            .phv_signal_params
        )
    )
    out[, names(.__signal__.) := .__signal__.][]
})
