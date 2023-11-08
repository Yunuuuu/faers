#' Download and parse FAERS Quarterly Data files
#' @inheritParams faers_download
#' @inheritParams faers_parse
#' @param handle_opts Extra handle options passed to each request
#' [new_handle][curl::new_handle].
#' @return A [FAERSxml] or [FAERSascii] object.
#' @examples
#' # you must change `dir`, as the file included in the package is sampled
#' data <- faers(2004, "q1", dir = system.file("extdata", package = "faers"))
#' @export
faers <- function(years, quarters, format = NULL, dir = getwd(), compress_dir = dir, handle_opts = list()) {
    format <- match.arg(format, faers_file_format)
    yq <- recycle_scalar(years = years, quarters = quarters)
    data.table::setDT(yq)
    yq <- unique(yq)
    faers_files <- do.call(faers_download, c(
        yq, list(format = format, dir = dir), handle_opts
    ))
    bar_id <- cli::cli_progress_bar(
        "Parsing FAER",
        type = "iterator", total = length(faers_files),
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
        format_done = sprintf(
            "Parsing {.val {cli::pb_total}} %s Quarterly Data file{?s} in {cli::pb_elapsed}",
            format
        ),
        clear = FALSE
    )
    out <- .mapply(
        function(path, year, quarter, format, compress_dir) {
            out <- faers_parse(
                path = path,
                format = format, year = year, quarter = quarter,
                compress_dir = compress_dir
            )
            cli::cli_progress_update(id = bar_id)
            out
        },
        list(path = faers_files, year = yq$years, quarter = yq$quarters),
        MoreArgs = list(format = format, compress_dir = compress_dir)
    )
    l <- length(out)
    if (l == 1L) {
        return(out[[1L]])
    }
    cli::cli_alert("Combining all {l} {.cls FAERS} Data{?s}")
    combine_faers(out)
}

recycle_scalar <- function(..., length = NULL, args = NULL) {
    args <- args %||% unlist(lapply(substitute(...()), as.character),
        use.names = FALSE
    )
    lst <- list(...)
    l <- lengths(lst)
    expected_len <- length %||% max(l)
    if (!all(l == 1L | l == expected_len)) {
        cli::cli_abort(c(
            "{.arg {args}} must have compatible sizes",
            i = "Only values of size one are recycled."
        ))
    }
    lapply(lst, rep_len, length.out = expected_len)
}
