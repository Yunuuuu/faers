rxnorm_getRxNormName <- function(terms, pool = 5L, retry = 3L) {
    resps <- rxnorm_perform("rxcui/%s",
        rxnorm_ids = terms,
        rxnorm_api_name = NULL,
        pool = pool, retry = retry
    )
    rxnorm_return_atomic(resps, "//idGroup")
}

rxnorm_getDrugs <- function(terms, pool = 5L, retry = 3L) {
    resps <- rxnorm_perform("drugs",
        rxnorm_ids = terms, rxnorm_api_name = "name",
        pool = pool, retry = retry
    )
    rxnorm_return_list(resps, "//conceptProperties")
}

rxnorm_getApproximateMatch <- function(terms, max_entries = NULL, option = NULL, pool = 5L, retry = 3L) {
    resps <- rxnorm_perform("approximateTerm",
        rxnorm_ids = terms, rxnorm_api_name = "term",
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
rxnorm_findRxcuiByString <- function(terms, allsrc = NULL, srclist = NULL, search = NULL, pool = 5L, retry = 3L) {
    resps <- rxnorm_perform("rxcui",
        rxnorm_ids = terms,
        rxnorm_api_name = "name",
        allsrc = allsrc,
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
    if (rxnorm_fail(resp)) {
        return(NA_character_)
    }
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}

rxnorm_parse_dt <- function(resp, xpath) {
    if (rxnorm_fail(resp)) {
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
rxnorm_perform <- function(path, rxnorm_ids, rxnorm_api_name = NULL, ..., pool = 5L, retry = 3L, format = "xml") {
    pool <- max(1L, min(pool, 20L))
    resps <- vector("list", length(rxnorm_ids))
    groups <- seq_along(resps)
    groups <- split(groups, ceiling(groups / pool))
    if (!curl::has_internet()) {
        cli::cli_abort("No internet")
    }
    bar_id <- cli::cli_progress_bar(
        name = "Querying RxNorm",
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_rate}] | {cli::pb_eta_str}",
        format_done = "Querying RxNorm for {.val {cli::pb_total}} quer{?y/ies} in {cli::pb_elapsed}",
        total = length(resps)
    )
    for (idx in groups) {
        req_list <- lapply(rxnorm_ids[idx], rxnorm_api,
            path = path, rxnorm_api_name = rxnorm_api_name,
            ..., format = format
        )
        resps[idx] <- httr2::multi_req_perform(req_list)
        cli::cli_progress_update(inc = length(idx), id = bar_id)
    }
    if (retry > 0L) {
        fail <- vapply(resps, function(resp) {
            !rxnorm_non_exist(resp) && rxnorm_fail(resp)
        }, logical(1))
        if (any(fail)) {
            cli::cli_alert("Retry for {sum(fail)} quer{?y/ies}")
            resps[fail] <- Recall(
                path = path,
                rxnorm_ids = rxnorm_ids[fail],
                rxnorm_api_name = rxnorm_api_name,
                ..., pool = pool, retry = retry - 1L,
                format = format
            )
        }
    }
    resps
}

rxnorm_non_exist <- function(resp) {
    inherits(resp, "httr2_http_404")
}

rxnorm_fail <- function(resp) {
    inherits(resp, "error") || httr2::resp_status(resp) != 200L
}

rxnorm_api <- function(path, rxnorm_id, rxnorm_api_name = NULL, ..., format = "xml") {
    req <- httr2::request(rxnorm_host)
    if (is.null(rxnorm_api_name)) {
        req <- httr2::req_url_path(
            req = req, "REST",
            sprintf("%s.%s", sprintf(path, rxnorm_id), format)
        )
        req <- httr2::req_url_query(.req = req, ...)
    } else {
        req <- httr2::req_url_path(
            req = req, "REST",
            sprintf("%s.%s", path, format)
        )
        req <- rlang::exec(
            httr2::req_url_query,
            .req = req, !!rxnorm_api_name := rxnorm_id, ...
        )
    }
    rxnorm_set_headers(req)
}

rxnorm_set_headers <- function(req) {
    # https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html
    req <- httr2::req_headers(req, `Cache-Control` = "max-age=43200")
    httr2::req_cache(httr2::req_throttle(req, rate = 20L),
        path = faers_cache_dir("rxnorm")
    )
}

rxnorm_host <- "https://rxnav.nlm.nih.gov"
