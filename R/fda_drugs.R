#' Read and Parse Drugs@@FDA data
#'
#' @param pattern File pattern to use. Must define a file exactly, you can set
#' `list = TRUE` to see what files can be used.
#' @param list A boolean value, should it only list files in the `Drugs@@FDA`
#' dataset?
#' @param force A boolean value. If set to `TRUE`, it indicates the retrieval of
#' `Drugs@@FDA` data in the FDA directly, bypassing the cache.
#' @return
#' - if `list = TRUE`, an atomic character.
#' - if `list = FALSE`, a [data.table][data.table::data.table].
#' @seealso
#' <https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files>
#' @examples
#' fda_drugs(list = TRUE)
#' fda_drugs()
#' @export
fda_drugs <- function(pattern = "Products", list = FALSE, force = FALSE) {
    assert_bool(list)
    assert_bool(force)
    file <- fda_drugs_file(force)
    fda_drugs_load(file, pattern = pattern, list = list)
}

fda_drugs_load <- function(file, pattern = "Products", list = FALSE, dir = faers_cache_dir("fdadrugs")) {
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

fda_drugs_file <- function(force, dir = faers_cache_dir("fdadrugs")) {
    cache_file(
        force = force,
        url = fda_drugs_url(),
        prefix = "fda_drugs_data",
        ext = "zip",
        name = "Drugs@FDA data",
        dir = dir
    )
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
