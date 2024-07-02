#' Download FAERS data
#'
#' This function downloads the FAERS data for selected years and quarters.
#'
#' @inheritParams faers_available
#' @param format File format to used, only "ascii" and "xml" are availabe.
#'  Default: "ascii".
#' @param dir The destination directory for any downloads. Defaults to
#'  current working dir.
#' @param ... Extra handle options passed to each request
#'  [multi_download][curl::multi_download].
#' @return An atomic character for the path of downloaded files.
#' @examples
#' # you must change `dir`, as the file included in the package is sampled
#' # in this way, the file will downloaded from FAERS
#' faers_download(
#'     year = 2004, quarter = "q1",
#'     dir = system.file("extdata", package = "faers")
#' )
#' @export
faers_download <- function(years, quarters, format = NULL, dir = getwd(), ...) {
    format <- match.arg(format, faers_file_format)
    assert_string(dir, empty_ok = FALSE)
    if (format == "xml") {
        # only faers database has xml data files
        is_aers_pairs <- is_from_laers(years, quarters)
        if (any(is_aers_pairs)) {
            aers_pairs <- paste0(years, quarters)[is_aers_pairs] # nolint
            cli::cli_abort(c(
                "Only FAERS (from 2012q4) has {.field xml} files",
                x = "Legacy AERS (before 2012q3) pair{?s}: {.val {aers_pairs}}"
            ))
        }
    }
    urls <- build_faers_url(format, years, quarters)
    dest_files <- file.path(dir_create2(dir), basename(urls))
    download_inform(urls, dest_files, ...)
}

#' Download utils function with good message.
#' @return A character path if downloading successed, otherwise, stop with error
#'   message.
#' @noRd
download_inform <- function(urls, file_paths, ...) {
    out <- list(
        urls = urls, destfiles = file_paths,
        is_success = rep_len(TRUE, length(urls))
    )
    if (any(is_existed <- file.exists(file_paths))) {
        cli::cli_inform(paste(
            "Finding {.val {sum(is_existed)}} file{?s} already",
            "downloaded: {.file {basename(file_paths[is_existed])}}"
        ))
        urls <- urls[!is_existed]
        file_paths <- file_paths[!is_existed]
    }
    if (l <- length(urls)) {
        assert_internet()
        if (l == 1L) {
            cli::cli_inform("Downloading 1 file from: {.url {urls}}")
        } else {
            cli::cli_inform("Downloading {.val {l}} files")
        }
        arg_list <- c(
            list(
                urls = urls, destfiles = file_paths, resume = FALSE,
                progress = interactive(), timeout = Inf
            ),
            rlang::list2(...)
        )
        status <- do.call(curl::multi_download, arg_list)
        is_success <- is_download_success(status)
        is_need_deleted <- !is_success & file.exists(file_paths)
        if (any(is_need_deleted)) {
            file.remove(file_paths[is_need_deleted])
        }
        if (!all(is_success)) {
            n_failed_files <- sum(!is_success) # nolint
            cli::cli_abort(c(
                "Cannot download {.val {n_failed_files}} file{?s}",
                "i" = "url{?s}: {.url {urls[!is_success]}}",
                "!" = paste(
                    "status {cli::qty(n_failed_files)} code{?s}:",
                    "{.val {status[!is_success]}}"
                ),
                x = "error {cli::qty(n_failed_files)} message{?s}: {.val {status$error[!is_success]}}"
            ))
        }
    }
    out$destfiles
}

#' @param status A data frame returned by [multi_download][curl::multi_download]
#' @noRd
is_download_success <- function(status, successful_code = c(200L, 206L, 416L)) {
    status$success & !is.na(status$success) &
        status$status_code %in% successful_code
}

base_download_inform <- function(urls, file_paths, ...) {
    out <- file_paths
    if (any(is_existed <- file.exists(file_paths))) {
        cli::cli_inform(paste(
            "Finding {.val {sum(is_existed)}} file{?s} already",
            "downloaded: {.file {basename(file_paths[is_existed])}}"
        )) # nolint
        urls <- urls[!is_existed]
        file_paths <- file_paths[!is_existed]
    }
    if (l <- length(urls)) {
        assert_internet()
        if (l == 1L) {
            cli::cli_inform("Downloading 1 file from: {.url {urls}}")
        } else {
            cli::cli_inform("Downloading {.val {l}} files")
        }
        status <- utils::download.file(urls,
            destfile = file_paths, ..., method = "libcurl"
        )
        is_success <- status == 0L
        is_need_deleted <- !is_success & file.exists(file_paths)
        if (any(is_need_deleted)) file.remove(file_paths[is_need_deleted])
        if (!all(is_success)) {
            n_failed_files <- sum(!is_success) # nolint
            cli::cli_abort(c(
                "Cannot download {.val {n_failed_files}} file{?s}",
                "i" = "url{?s}: {.url {urls[!is_success]}}",
                "!" = paste(
                    "status {cli::qty(n_failed_files)} code{?s}:",
                    "{.val {status[!is_success]}}"
                )
            ))
        }
    }
    out
}

build_faers_url <- function(type, years, quarters) {
    laers_period <- is_from_laers(years, quarters)
    sprintf(
        "%s/content/Exports/%s_%s_%s%s.zip",
        fda_host("fis"),
        ifelse(laers_period, "aers", "faers"),
        ifelse(type == "ascii" | !laers_period, type, "sgml"),
        years, quarters
    )
}
