#' Set Operations for FAERS objects
#'
#' Similar to the set functions in base R (`union` and `intersect`), the
#' `faers_union` and `faers_intersect` functions are specifically designed for
#' use with the [FAERS] objects. It is important to note that all data passed to
#' these functions via the `...` argument must belong to the same FAERS object,
#' indicating that they have the same period data (as defined by faers_period).
#' In many cases, this data is obtained by applying the [faers_filter] or
#' [faers_keep] function within the same FAERS object.
#'
#' @param ... A collection of multiple [FAERSxml] or [FAERSascii] objects, or a
#' list containing [FAERSxml] or [FAERSascii] objects. These objects can be
#' either completely standardized using [faers_standardize] or not standardized
#' at all. Additionally, they can be fully de-duplicated using [faers_dedup] or
#' not de-duplicated at all.
#' @return A [FAERSxml] or [FAERSascii] object.
#' @examples
#' # the files included in the package are sampled
#' data1 <- faers_parse(
#'     system.file("extdata", "aers_ascii_2004q1.zip", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' data2 <- faers_parse(
#'     system.file("extdata", "faers_ascii_2017q2.zip", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' # It is not possible to use Set Operations on `data1` and `data2` as they are
#' # derived from different time periods.  in this way, it would be more
#' # appropriate to employ the function `faers_combine()` to combine them
#' data <- faers_combine(data1, data2)
#' interested_data1 <- faers_filter(data, function(drug) {
#'     # just for example, no meanning
#'     drug[grepl("ALCOHOL", drugname, ignore.case = TRUE) |
#'         grepl("ALCOHOL", prod_ai, ignore.case = TRUE), primaryid]
#' }, .field = "drug")
#' interested_data2 <- faers_filter(data, function(drug) {
#'     # just for example, no meanning
#'     drug[grepl("HUMIRA", drugname, ignore.case = TRUE) |
#'         grepl("HUMIRA", prod_ai, ignore.case = TRUE), primaryid]
#' }, .field = "drug")
#' # I don't known whether to union primaryid or just rows identity
#' faers_union(interested_data1, interested_data2)
#' @export 
#' @aliases faers_union
#' @name FAERS-Set
faers_union <- function(...) {
    x <- wrap_faers(...)
    cli::cli_alert("Take the union of {length(x)} {.cls FAERS} Data{?s}")
    faers_set_opts(x, set_fn = data.table::funion)
}

#' @export 
#' @rdname FAERS-Set
faers_intersetct <- function(...) {
    x <- wrap_faers(...)
    cli::cli_alert("Take the intersect of {length(x)} {.cls FAERS} Data{?s}")
    faers_set_opts(x, set_fn = data.table::fintersect)
}

faers_set_opts <- function(x, set_fn, call = rlang::caller_env()) {
    type <- combine_faers_list_type(x, call = call)
    period <- set_opts_faers_period(x)
    is_dedup <- set_opts_faers_deduplication(x, call = call)
    is_stand <- combine_faers_standardization(x, call = call)
    meddra <- combine_faers_meddra(x, standardization = is_stand, call = call)
    data <- set_opts_faers_data(x, type = type, set_fn = set_fn)
    switch(type,
        ascii = methods::new("FAERSascii",
            data = data,
            deletedCases = unique(
                unlist(lapply(x, faers_deleted_cases), use.names = FALSE)
            ),
            year = period$year, quarter = period$quarter,
            standardization = is_stand, deduplication = is_dedup,
            meddra = meddra
        ),
        xml = methods::new("FAERSxml",
            data = data,
            year = period$year, quarter = period$quarter,
            standardization = is_stand, deduplication = is_dedup,
            meddra = meddra
        )
    )
}

set_opts_faers_period <- function(lst, call = rlang::caller_env()) {
    period_lst <- lapply(lst, faers_period)
    ref <- period_lst[[1L]]
    is_right <- vapply(period_lst, function(x) identical(ref, x), logical(1L))
    if (all(is_right)) {
        return(ref)
    }
    cli::cli_abort(
        "All elements in {.arg ...} must be from the same {.cls FAERS} data",
        call = call
    )
}

set_opts_faers_deduplication <- function(lst, call = rlang::caller_env()) {
    dedup_vec <- vapply(lst, function(object) object@deduplication, logical(1L))
    if (all(dedup_vec)) {
        TRUE
    } else if (!any(dedup_vec)) {
        FALSE
    } else {
        cli::cli_abort(
            "All elements in {.arg ...} must be either fully deduplicated or not at all.",
            call = call
        )
    }
}

set_opts_faers_data <- function(x, type, set_fn) {
    switch(type,
        ascii = set_opts_faers_ascii_data(x, set_fn = set_fn),
        xml = set_opts_faers_xml_data(x, set_fn = set_fn)
    )
}

set_opts_faers_ascii_data <- function(x, set_fn) {
    data_list <- lapply(faers_ascii_file_fields, function(field) {
        Reduce(set_fn, lapply(x, function(obj) obj@data[[field]]))
    })
    data.table::setattr(data_list, "names", faers_ascii_file_fields)
    data_list
}

set_opts_faers_xml_data <- function(x, set_fn) {
    Reduce(set_fn, lapply(x, function(obj) obj@data))
}
