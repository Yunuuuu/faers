#' Download FAERS data
#'
#' This function downloads the FAERS data for selected years and quarters.
#'
#' @inheritParams faers_available
#' @param type File type to used, only "ascii" and "xml" are availabe.
#' @param dir The destination directory for any downloads. Defaults to
#'  current working dir. 
#' @param ... Extra handle options passed to each request
#' [new_handle][curl::new_handle].
#' @return An atomic character for the path of downloaded files.
#' @examples
#' \dontrun{
#'  faers_download(year = 2018, quarter = "q4", dir = tempdir())
#' }
#' @export
faers_download <- function(years, quarters, type = c("ascii", "xml"), dir = getwd(), ...) {
    type <- match.arg(type)
    assert_string(dir, empty_ok = FALSE)
    data_available <- faers_available(years, quarters)
    if (!all(data_available)) {
        missed_pairs <- paste(years, quarters, sep = ":")[!data_available] # nolint
        cli::cli_abort(c(
            "Not all data specified in {.arg years} and {.arg quarters} are available",
            x = "Missed pair{?s}: {.val {missed_pairs}}"
        ))
    }
    urls <- build_faers_url(type, years, quarters)
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
    dest_files <- file.path(dir, basename(urls))
    download_inform(urls, dest_files, handle_opts = list(...))
}

#' Download utils function with good message.
#' @return A character path if downloading successed, otherwise, stop with error
#'   message.
#' @noRd
download_inform <- function(urls, file_paths, handle_opts = list()) {
    out <- list(
        urls = urls, destfiles = file_paths,
        is_success = rep_len(TRUE, length(urls))
    )
    is_existed <- file.exists(file_paths)
    if (any(is_existed)) {
        cli::cli_inform("Finding {.val {sum(is_existed)}} file{?s} already downloaded: {.file {basename(file_paths[is_existed])}}") # nolint
        urls <- urls[!is_existed]
        file_paths <- file_paths[!is_existed]
    }
    if (length(urls)) {
        cli::cli_inform(
            "Downloading {.val {length(urls)}} %s file{?s} from {.url {fda_url}}"
        )
        arg_list <- c(
            list(
                urls = urls, destfiles = file_paths, resume = FALSE,
                progress = interactive(), timeout = Inf
            ),
            handle_opts
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
                "!" = "status {cli::qty(n_failed_files)} code{?s}: {.val {status$status_code[!is_success]}}",
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

build_faers_url <- function(type, years, quarters) {
    sprintf(
        "%s/content/Exports/%s_%s_%s%s.zip",
        fda_url,
        ifelse(years > 2012L | (years == 2012L & quarters == "q4"),
            "faers", "aers"
        ),
        type, years, quarters
    )
}
fda_url <- "https://fis.fda.gov"
