rxnorm_getDrugs <- function(terms, pool = 10L, retry = 3L) {
    resps <- rxnorm_perform("drugs",
        query_api_id = "name",
        terms = terms, pool = pool, retry = retry
    )
    rxnorm_return_list(resps, "//conceptProperties")
}

rxnorm_getApproximateMatch <- function(terms, max_entries = NULL, option = NULL, pool = 10L, retry = 3L) {
    resps <- rxnorm_perform("approximateTerm",
        query_api_id = "term", terms = terms,
        maxEntries = max_entries, option = option,
        pool = pool, retry = retry
    )
    rxnorm_return_list(resps, "//candidate")
}

#' @param allsrc 0: Active concepts; 1: Current concepts. Active: concepts in
#' the current RxNorm data set that have an atom with SAB=RXNORM and SUPPRESS=N;
#' Current: concepts in the current RxNorm data set that have an atom with
#' SUPPRESS=N.
#' @param srclist Source vocabularies to search (if allsrc=1)
#' @param search 0: Exact match only; 1: Normalized match; 2: Best match (exact
#' or normalized)
#' @noRd
rxnorm_findRxcuiByString <- function(terms, allsrc = NULL, srclist = NULL, search = NULL, pool = 10L, retry = 3L) {
    resps <- rxnorm_perform("rxcui",
        query_api_id = "name",
        terms = terms, allsrc = allsrc,
        srclist = srclist, search = search,
        pool = pool, retry = retry
    )
    rxnorm_return_atomic(resps, "//rxnormId")
}

# parse rxnorm resp
rxnorm_return_list <- function(resps, xpath) {
    lapply(resps, rxnorm_parse_dt, xpath = xpath)
}

rxnorm_return_atomic <- function(resps, xpath) {
    vapply(resps, rxnorm_parse_scalar, character(1L), xpath = xpath)
}

rxnorm_parse_scalar <- function(resp, xpath) {
    if (rxnorm_is_fail(resp)) {
        return(NA)
    }
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}

rxnorm_parse_dt <- function(resp, xpath) {
    if (rxnorm_is_fail(resp)) {
        return(NULL)
    }
    xml <- httr2::resp_body_xml(resp)
    out <- xml2::xml_find_all(xml, xpath)
    out <- lapply(out, function(x) {
        data.table::setDT(make_length_one_list(xml2::as_list(x)))
    })
    out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
    simplify_list_cols(out)[]
}

#' all values in dots should be named.
#' @return A list of resp objects
#' @noRd
rxnorm_perform <- function(path, query_api_id, terms, ..., pool = 20L, retry = 3L, type = "xml") {
    pool <- max(1L, min(pool, 20L))
    resps <- vector("list", length(terms))
    groups <- seq_along(resps)
    groups <- split(groups, ceiling(groups / pool))
    if (!curl::has_internet()) {
        cli::cli_abort("No internet")
    }
    bar_id <- cli::cli_progress_bar(
        name = "Querying RxNorm",
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
        format_done = sprintf(
            "Querying RxNorm {.val {cli::pb_total}} run{?s} per %s in {cli::pb_elapsed}", pool
        ),
        total = length(groups)
    )
    for (idx in groups) {
        req_list <- lapply(terms[idx], rxnorm_api,
            path = path, query_api_id = query_api_id,
            ..., type = type
        )
        resps[idx] <- httr2::multi_req_perform(req_list)
        cli::cli_progress_update(id = bar_id)
    }
    if (retry > 0L) {
        fail <- vapply(resps, rxnorm_is_fail, logical(1))
        if (any(fail)) {
            cli::cli_alert("Retry for {sum(fail)} quer{?y/ies}")
            resps[fail] <- Recall(
                path = path,
                query_api_id = query_api_id,
                terms = terms[fail],
                ..., pool = pool, retry = retry - 1L
            )
        }
    }
    resps
}

rxnorm_is_fail <- function(resp) {
    inherits(resp, "error") || httr2::resp_status(resp) != 200L
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
