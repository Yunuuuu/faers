#' Download and parse FAERS Quarterly Data files
#' @inheritParams faers_download
#' @inheritParams faers_parse
#' @param handle_opts Extra handle options passed to each request
#' [new_handle][curl::new_handle].
#' @return A [FAERSxml] or [FAERSascii] object.
#' @export
faers <- function(years, quarters, type = NULL, dir = getwd(), compress_dir = dir, handle_opts = list()) {
    type <- match.arg(type, faers_file_types)
    yq <- recycle_scalar(years = years, quarters = quarters)
    faers_files <- do.call(faers_download, c(
        yq, list(type = type, dir = dir), handle_opts
    ))
    bar_id <- cli::cli_progress_bar(sprintf("Parsing FAERS %s file", type),
        type = "iterator", total = length(faers_files),
        format_done = sprintf(
            "Parsing {.val {cli::pb_total}} %s Quarterly Data file{?s} in {cli::pb_elapsed}",
            type
        ),
        clear = FALSE
    )
    out <- .mapply(
        function(path, year, quarter, type, compress_dir) {
            cli::cli_progress_update(id = bar_id)
            faers_parse(
                path = path,
                type = type, year = year, quarter = quarter,
                compress_dir = compress_dir
            )
        },
        list(path = faers_files, year = yq$years, quarter = yq$quarters),
        MoreArgs = list(type = type, compress_dir = compress_dir)
    )
    faers_combine(out)
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
