#' Read and Parse Drugs@@FDA data
#'
#' @param pattern File pattern to use. Must define a file exactly, you can set
#' `list = TRUE` to see what files can be used.
#' @param list A boolean value, should it only list files in the `Drugs@@FDA`
#' dataset?
#' @param force A boolean value. If set to `TRUE`, it indicates the retrieval of
#' `Drugs@@FDA` data in the FDA directly, bypassing the cache.
#' @param url A string of the url for `Drugs@@FDA` file. Try to get the link
#' from site:
#' <https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files>.
#' @return
#' - if `list = TRUE`, an atomic character.
#' - if `list = FALSE`, a [data.table][data.table::data.table].
#' @examples
#' fda_drugs(list = TRUE)
#' fda_drugs()
#' @export
fda_drugs <- function(pattern = "Products", url = NULL,
                      list = FALSE, force = FALSE) {
    assert_bool(list)
    assert_bool(force)
    assert_string(url, null_ok = TRUE)
    file <- fda_drugs_file(url, force)
    fda_drugs_load(file, pattern = pattern, list = list)
}

fda_drugs_load <- function(file, pattern = "Products",
                           list = FALSE, dir = faers_cache_dir("fdadrugs")) {
    path <- unzip2(file, dir)
    if (list) {
        list.files(path)
    } else {
        assert_string(pattern)
        file <- locate_file(path, pattern, ignore.case = TRUE)
        # Don't use data.table: error, Stopped early on line
        out <- vroom::vroom(file, show_col_types = FALSE)
        data.table::setDT(out)[]
    }
}

fda_drugs_file <- function(url = NULL, force,
                           dir = faers_cache_dir("fdadrugs"),
                           arg = rlang::caller_arg(url),
                           call = rlang::caller_env()) {
    cache_use_or_download(
        force = force,
        url = fda_drugs_url(url, arg = arg, call = call),
        prefix = "fda_drugs_data",
        ext = "zip",
        name = "Drugs@FDA data",
        dir = dir, 
        method = "base",
        arg = arg, call = call
    )
}

fda_drugs_url <- function(url = NULL,
                          arg = rlang::caller_arg(url),
                          call = rlang::caller_env()) {
    if (!is.null(url)) return(url) # styler: off
    assert_internet(call = call)
    url <- sprintf(
        "%s/drugs/drug-approvals-and-databases/drugsfda-data-files",
        fda_host("www")
    )
    cli::cli_inform(c(">" = "Reading html: {.url {url}}"))
    html <- xml2::read_html(url)
    node <- rvest::html_element(html, "[data-entity-substitution]")
    if (inherits(node, "xml_missing")) {
        # cli::cli_abort(c(
        #     "Cannot determine the url of {.field Drugs@FDA} file",
        #     i = "try to provide {.arg {arg}} manually"
        # ), call = call)
        href <- "/media/89850/download?attachment"
    } else {
        cli::cat_line(rvest::html_text(node))
        href <- rvest::html_attr(node, "href")
    }
    paste0(fda_host("www"), href)
}
