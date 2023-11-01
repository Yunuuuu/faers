#' Create contingency table and run disproportionality analysis
#' @details
#'  - `faers_phv_table`: build a contingency table for all events in
#'    `interested_event`.
#'  - `faers_phv_signal`: Pharmacovigilance Analysis used contingency table
#'    constructed with `faers_phv_table`. Details see [phv_signal].
#' @param object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods.
#'  - `faers_phv_table`: other arguments passed to `interested_fn`.
#'  - `faers_phv_signal`: other arguments passed to `faers_phv_table`.
#' @return A [data.table][data.table::data.table] object.
#' @examples
#' \dontrun{
#' # you must change `dir`, as the files included in the package are sampled
#' data <- faers(c(2004, 2017), c("q1", "q2"),
#'     dir = system.file("extdata", package = "faers")
#' )
#' # you must standardize and deduplication before disproportionality analysis
#' # you should replace `meddra_path` with yours
#' data <- faers_standardize(data, meddra_path)
#' data <- faers_dedup(data)
#' faers_phv_table(data,
#'     filter_params = list(field = "demo", .fn = function(x) {
#'        sample(x$primaryid, 100L)
#'     })
#' )
#' faers_phv_signal(data,
#'     filter_params = list(field = "demo", .fn = function(x) {
#'        sample(x$primaryid, 100L)
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


#' @param interested_field A string indicates the interested FAERS fields to
#' use. Only values "demo", "drug", "indi", "ther", "reac", "rpsr", and "outc"
#' can be used.
#' @param interested_event A character specify the events column(s?) in field of
#' object specified in `interested_field`. If multiple columns were selected,
#' the unique combination will define the interested events.
#' @param filter_params Other arguments passed to [faers_filter], solely used
#' when `interested` and `object2` are both `missing`
#' @param interested_fn A function or formula defined the preprocessing function
#' before creating contingency table, with the `interested_field` data as the
#' input and return a [data.table][data.table::data.table].
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs.
#'
#'   If a **string**, the function is looked up in `globalenv()`.
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

#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "missing"),
    function(object, interested_field = "reac", interested_event = "soc_name", interested_fn = NULL, ..., interested, object2) {
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
        full_data <- faers_get(object, field = interested_field)
        interested_data <- faers_get(interested, field = interested_field)
        if (!is.null(interested_fn)) {
            interested_fn <- rlang::as_function(interested_fn)
            full_data <- interested_fn(full_data, ...)
            interested_data <- interested_fn(interested_data, ...)
            if (!(data.table::is.data.table(interested_data) ||
                data.table::is.data.table(full_data))) {
                cli::cli_abort("{.fn interested_fn} must return an {.cls data.table}")
            }
        }
        n <- nrow(full_data) # scalar
        n1. <- nrow(interested_data) # scalar
        out <- merge(
            eval(substitute(
                full_data[, list(n.1 = .N), by = interested_event],
                list(interested_event = interested_event)
            )),
            eval(substitute(
                interested_data[, list(a = .N), by = interested_event],
                list(interested_event = interested_event)
            )),
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
    function(object, interested_field = "reac", interested_event = "soc_name", interested_fn = NULL, ..., interested, object2) {
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
        interested_reac <- faers_get(object, field = interested_field)
        interested_reac2 <- faers_get(object2, field = interested_field)
        if (!is.null(interested_fn)) {
            interested_fn <- rlang::as_function(interested_fn)
            interested_reac <- interested_fn(interested_reac, ...)
            interested_reac2 <- interested_fn(interested_reac2, ...)
            if (!(data.table::is.data.table(interested_reac) ||
                data.table::is.data.table(interested_reac2))) {
                cli::cli_abort("{.arg interested_fn} must return an {.cls data.table}")
            }
        }
        n1. <- nrow(interested_reac)
        n0. <- nrow(interested_reac2)
        out <- merge(
            eval(substitute(
                interested_reac[, list(a = .N), by = interested_event],
                list(interested_event = interested_event)
            )),
            eval(substitute(
                interested_reac2[, list(c = .N), by = interested_event],
                list(interested_event = interested_event)
            )),
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

#' @rdname faers_phv_signal
methods::setMethod(
    "faers_phv_table",
    c(object = "FAERSascii", interested = "FAERSascii", object2 = "FAERSascii"),
    function(object, interested, object2) {
        cli::cli_abort("{.arg interested} and {.arg object2} are both exclusive, must be provided only one or none")
    }
)

##############################################################
#' @export
#' @rdname faers_phv_signal
methods::setGeneric("faers_phv_signal", function(object, ...) {
    methods::makeStandardGeneric("faers_phv_signal")
})

#' @inheritParams phv_signal
#' @seealso [phv_signal]
#' @method faers_phv_signal FAERSascii
#' @rdname faers_phv_signal
methods::setMethod("faers_phv_signal", "FAERSascii", function(object, ..., methods = NULL, alpha = 0.05, correct = TRUE, n_mcmc = 1e5L, alpha1 = 0.5, alpha2 = 0.5) {
    out <- faers_phv_table(object, ...)
    cbind(
        out,
        do.call(
            phv_signal,
            c(out[, .SD, .SDcols = c("a", "b", "c", "d")], list(
                methods = methods, alpha = alpha, correct = correct,
                n_mcmc = n_mcmc, alpha1 = alpha1, alpha2 = alpha2
            ))
        )
    )
})
