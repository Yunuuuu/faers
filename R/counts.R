#' Counting the number of unique case for each event
#'
#' @param object A [FAERSascii] object.
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
#' faers_counts(data)
#' }
#' @export
#' @name faers_counts
methods::setGeneric("faers_counts", function(object, ...) {
    methods::makeStandardGeneric("faers_counts")
})

#' @param ... Other arguments passed to specific methods, for `FAERSascii`
#' method, other arguments passed to `interested_fn()`.
#' @param interested_field A string indicates the interested FAERS fields to
#' use. Only values "demo", "drug", "indi", "ther", "reac", "rpsr", and "outc"
#' can be used.
#' @param interested_event A character specify the events column(s?) in the
#' `interested_field` of object to count the unique `primaryid`. If multiple
#' columns were selected, the combination for all columns will define the
#' interested events.
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
#' @rdname faers_counts
#' @export
methods::setMethod(
    "faers_counts", c(object = "FAERSascii"),
    function(object, interested_field = "reac", interested_event = "soc_name", interested_fn = NULL, ...) {
        if (!object@standardization) {
            cli::cli_abort("{.arg object} must be standardized using {.fn faers_standardize}")
        }
        data <- faers_get(object, field = interested_field)
        if (!is.null(interested_fn)) {
            interested_fn <- rlang::as_function(interested_fn)
            data <- interested_fn(data, ...)
            if (!data.table::is.data.table(data)) {
                cli::cli_abort("{.fn interested_fn} must return an {.cls data.table}")
            }
        }
        groups <- c("primaryid", interested_event)
        data <- unique(data, by = groups, cols = character())
        eval(substitute(
            data[, list(N = .N), by = interested_event],
            list(interested_event = interested_event)
        ))
    }
)
