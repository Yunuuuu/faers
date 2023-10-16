rxnorm_get_drugs <- function(terms, pool = 20L) {
    rxnorm_perform("drugs", query_api_id = "name", terms = terms, pool = pool)
}

rxnorm_approximate_map <- function(terms, max_entries = NULL, option = NULL, pool = 20L) {
    rxnorm_perform("approximateTerm",
        query_api_id = "term", terms = terms,
        maxEntries = max_entries, option = option,
        pool = pool
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
rxnorm_exact_map <- function(terms, allsrc = NULL, srclist = NULL, search = NULL, pool = 20L) {
    rxnorm_perform("rxcui",
        query_api_id = "name",
        terms = terms, allsrc = allsrc,
        srclist = srclist, search = search,
        pool = pool
    )
}

#' all values in dots should be named.
#' @return A list of resp objects
#' @noRd
rxnorm_perform <- function(path, query_api_id, terms, ..., pool = 20L, type = "xml") {
    pool <- max(1L, min(pool, 20L))
    resp_list <- vector("list", length(terms))
    group_list <- seq_along(resp_list)
    group_list <- split(group_list, ceiling(group_list / pool))
    bar_id <- cli::cli_progress_bar(
        name = "Querying RxNorm",
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
        format_done = sprintf(
            "Querying {.val {cli::pb_total}} run{?s} per %s in {cli::pb_elapsed}", pool
        ),
        total = length(group_list)
    )
    for (idx in group_list) {
        req_list <- lapply(terms[idx], rxnorm_api,
            path = path, query_api_id = query_api_id,
            ..., type = type
        )
        resp_list[idx] <- httr2::multi_req_perform(req_list)
        cli::cli_progress_update(id = bar_id)
    }
    resp_list
}

rxnorm_api <- function(path, query_api_id, query, ..., type = "xml") {
    req <- httr2::request(rxnorm_host)
    req <- httr2::req_url_path(req, sprintf("REST/%s.%s", path, type))
    req <- rlang::exec(
        httr2::req_url_query,
        .req = req, !!query_api_id := query, ...
    )
    # https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html
    req <- httr2::req_headers(req, `Cache-Control` = "max-age=43200")
    httr2::req_cache(httr2::req_throttle(req, rate = 20L),
        path = faers_cache_dir("rxnorm")
    )
}
rxnorm_host <- "https://rxnav.nlm.nih.gov"

get_rxnorm_item <- function(resp, xpath) {
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}
