#' Read and Parse Drugs@@FDA data
#'
#' @param use File pattern to use. Must define a file exactly, you can set `list
#' = TRUE` to see what files can be used.
#' @param list A boolean value, should it only list files in the Drugs@@FDA
#' dataset?
#' @param force A boolean value. If set to `TRUE`, it indicates the retrieval of
#' `Drugs@@FDA` data in the FDA directly, bypassing the cache.
#' @return
#' - if `list = TRUE`, an atomic character.
#' - if `list = FALSE`, a [data.table][data.table::data.table]
#' @seealso
#' <https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files>
#' @examples
#' fda_drugs(list = TRUE)
#' fda_drugs()
#' @export
fda_drugs <- function(use = "Products", list = FALSE, force = FALSE) {
    assert_bool(list)
    assert_bool(force)
    if (force) {
        file <- fda_drugs_download(dir = faers_cache_dir("fdadrugs"))
    } else {
        file <- fda_drugs_file()
    }
    fda_drugs_load(file, use = use, list = list)
}

fda_drugs_load <- function(file, use = "Products", list = FALSE, dir = faers_cache_dir("fdadrugs")) {
    path <- unzip2(file, dir)
    if (list) {
        list.files(path)
    } else {
        file <- locate_files(path, use, ignore.case = TRUE)
        if (length(file) > 1L) {
            cli::cli_abort(
                "Multiple files matched, files: {.file {basename(file)}}"
            )
        }
        # Don't use data.table
        # Stopped early on line
        out <- vroom::vroom(file, show_col_types = FALSE)
        data.table::setDT(out)[]
    }
}

fda_drugs_file <- function(dir = faers_cache_dir("fdadrugs")) {
    file <- tryCatch(
        locate_files(dir, "^fda_drugs_data.*\\.zip", ignore.case = FALSE),
        no_file = function(cnd) {
            FALSE
        }
    )
    if (isFALSE(file)) {
        file <- fda_drugs_download(dir = dir)
    } else {
        date <- as.Date(
            str_extract(basename(file), "\\d+-\\d+-\\d+"),
            format = "%Y-%m-%d"
        )
        if (length(file) > 1L) {
            i <- order(date, decreasing = TRUE)[1L]
            file <- file[i]
            date <- date[i]
        }
        cli::cli_inform(c(
            ">" = "Using Drugs@FDA Data from cached {.file {file}}",
            " " = "Snapshot date: {date}"
        ))
    }
    file
}

fda_drugs_download <- function(dir = faers_cache_dir("fdadrugs"), call = rlang::caller_env()) {
    assert_internet(call = call)
    file <- file.path(dir, sprintf("fda_drugs_data_%s.zip", Sys.Date()))
    download_inform(fda_drugs_url(), file)
}

fda_drugs_url <- function() {
    url <- sprintf(
        "%s/drugs/drug-approvals-and-databases/drugsfda-data-files",
        fda_host("www")
    )
    cli::cli_inform(c(">" = "Reading html: {.url {url}}"))
    html <- xml2::read_html(url)
    node <- rvest::html_element(
        html, "[data-entity-substitution=media_download]"
    )
    cli::cat_line(rvest::html_text(node))
    paste0(fda_host("www"), rvest::html_attr(node, "href"))
}
