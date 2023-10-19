#' List of FAERS data
#'
#' The function lists the metadata for the FAERS databases currently
#' available to download.
#'
#' @return A [data.table][data.table::data.table] reporting years, period,
#' quarter, and file urls and file sizes.
#' @examples
#' faers_meta()
#' @seealso
#' <https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html>
#' @export
faers_meta <- function() {
    html <- faers_meta_doc()
    table_xml_list <- rvest::html_elements(html, ".panel.panel-default")
    table_list <- lapply(table_xml_list, parse_year_xml_table)
    out <- data.table::rbindlist(table_list)
    out[, quarter := period2quarter(period)] # nolint
    data.table::setcolorder(out, c(
        "year", "period", "quarter",
        "urls_ascii", "file_size_ascii", "urls_xml", "file_size_xml"
    ))
    data.table::setnames(out, c(
        "year", "period", "quarter",
        "ascii_urls", "ascii_file_size", "xml_urls", "xml_file_size"
    ))
    data.table::setorderv(out, c("year", "quarter"), order = c(-1L, -1L))[]
}

faers_meta_doc <- function() {
    if (!exists(".faers_meta_doc", where = faers_cache_env, inherits = FALSE)) {
        assert_internet()
        url <- sprintf(
            "%s/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html", fda_host
        )
        cli::cli_alert("Reading html: {.url {url}}")
        faers_cache_env[[".faers_meta_doc"]] <- xml2::read_html(url)
    }
    get(".faers_meta_doc", pos = faers_cache_env, inherits = FALSE)
}

utils::globalVariables(c("period", "quarter"))

parse_year_xml_table <- function(year_xml) {
    year <- rvest::html_text2(rvest::html_element(year_xml, ".panel-title"))
    if (!str_detect(year, "^20\\d+$")) {
        return(NULL)
    }
    file_table <- lapply(rvest::html_elements(year_xml, "tbody > tr"), function(quarter_xml) {
        period <- rvest::html_text2(rvest::html_elements(quarter_xml, "p"))
        file_xmls <- rvest::html_elements(quarter_xml, "td a")
        files <- rvest::html_text2(file_xmls)
        out <- data.table::data.table(
            period = str_remove(period, "\\s*\\d*(\\s*posted.*\\s*)?$"),
            urls = rvest::html_attr(file_xmls, "href"),
            file_format = tolower(str_extract(files, "XML|ASCII")),
            file_size = str_extract(files, "\\d[\\d.]*(MB)")
        )
        data.table::dcast(out, period ~ file_format,
            value.var = c("urls", "file_size")
        )
    })
    out_table <- data.table::rbindlist(file_table)
    out_table[, year := as.integer(year)] # nolint
}

period2quarter <- function(x) {
    data.table::fcase(
        str_detect(x, "^October"), "q4",
        str_detect(x, "^July"), "q3",
        str_detect(x, "^April"), "q2",
        str_detect(x, "^January"), "q1"
    )
}
