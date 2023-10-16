rxnorm_get_drugs <- function(term) {
    rxnorm_perform("drugs", name = term)
}

rxnorm_approximate_map <- function(term, max_entries = NULL, option = NULL) {
    rxnorm_perform("approximateTerm",
        term = term, maxEntries = max_entries, option = option
    )
}

#' @param allsrc 0: Active concepts; 1: Current concepts. Active: concepts in
#' the current RxNorm data set that have an atom with SAB=RXNORM and SUPPRESS=N;
#' Current: concepts in the current RxNorm data set that have an atom with
#' SUPPRESS=N.
#' @param srclist Source vocabularies to search (if allsrc=1)
#' @param search 0: Exact match only; 1: Normalized match; 2: Best match (exact
#' or normalized)
#' @noRd
rxnorm_exact_map <- function(term, allsrc = NULL, srclist = NULL, search = NULL) {
    rxnorm_perform("rxcui",
        name = term, allsrc = allsrc,
        srclist = srclist, search = search
    )
}

rxnorm_perform <- function(path, ..., type = "xml") {
    httr2::req_perform(rxnorm_api(path = path, ..., type = type))
}

rxnorm_api <- function(path, ..., type = "xml") {
    req <- httr2::request(rxnorm_host)
    req <- httr2::req_url_path(req, sprintf("REST/%s.%s", path, type))
    req <- httr2::req_url_query(req, ...)
    # https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html
    req <- httr2::req_headers(req, `max-age` = 43200L)
    httr2::req_cache(httr2::req_throttle(req, rate = 20L),
        path = faers_cache_dir("rxnorm")
    )
}
rxnorm_host <- "https://rxnav.nlm.nih.gov"

get_rxnorm_item <- function(resp, xpath) {
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}
