#' Download and parse FAERS Quarterly Data files
#' @inheritParams faers_download
#' @inheritParams faers_parse
#' @param handle_opts Extra handle options passed to each request
#' [new_handle][curl::new_handle].
#' @return A [ListOfFAERS] object if multiple `years` and `quarters` are
#'  supplied, otherwise, a [FAERSxml] or [FAERSascii] object.
#' @export
faers <- function(years, quarters, type = c("ascii", "xml"), dir = getwd(), compress_dir = dir, handle_opts = list()) {
    type <- match.arg(type)
    yq <- refine_length(years = years, quarters = quarters)
    faers_files <- do.call(faers_download, c(
        yq, list(type = type, dir = dir), handle_opts
    ))
    out <- .mapply(faers_parse,
        list(path = faers_files, year = yq$years, quarter = yq$quarters),
        MoreArgs = list(type = type, compress_dir = compress_dir)
    )
    if (length(out) == 1L) {
        out[[1L]]
    } else {
        structure(out, class = c("ListOfFAERS", paste0("ListOfFAERS", type)))
    }
}

#' @param x A [ListOfFAERS] object.
#' @param ... Not used currently.
#' @export
#' @aliases ListOfFAERS
#' @rdname faers
print.ListOfFAERS <- function(x, ...) {
    cat(sprintf(
        "A total of %s FDA Adverse Event Reporting System Quarterly Data file%s",
        length(x), if (length(x) > 1L) "s" else ""
    ), sep = "\n")
}

refine_length <- function(..., args = NULL) {
    args <- args %||% unlist(lapply(substitute(...()), as.character),
        use.names = FALSE
    )
    lst <- list(...)
    l <- lengths(lst)
    ml <- max(l)
    if (!all(l == 1L | l == ml)) {
        cli::cli_abort(c(
            "{.arg {args}} must have compatible sizes",
            i = "Only values of size one are recycled."
        ))
    }
    lapply(lst, rep_len, length.out = ml)
}