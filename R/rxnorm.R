rxnorm_get_drugs <- function(term) {
    rxnorm_perform("drugs", name = term)
}

rxnorm_approximate_map <- function(term, max_entries = NULL, option = NULL) {
    rxnorm_perform("approximateTerm",
        term = term, maxEntries = max_entries, option = option
    )
}

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
    httr2::req_url_query(req, ...)
}
rxnorm_host <- "https://rxnav.nlm.nih.gov"

get_rxnorm_item <- function(resp, xpath) {
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}
