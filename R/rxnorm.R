rxnorm_standardize_drug <- function(terms, exact = TRUE, approximate = TRUE, search = 2, pool_size = 5L) {
    assert_bool(exact)
    assert_bool(approximate)
    rxnorm_map_to_rxcui(terms,
        exact = exact, approximate = approximate,
        search = search, pool_size = pool_size
    )
    # get other drug details from rxnorm
}

rxnorm_map_to_rxcui <- function(terms, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL, pool_size = 5L, retry = 0L) {
    if (!(exact || approximate)) {
        cli::cli_abort(
            "One of {.arg exact} or {.arg approximate} must be {.code TRUE}"
        )
    }
    if (exact) {
        cli::cli_alert("Running Exact Match")
        out <- rxnorm_findRxcuiByString(terms,
            allsrc = allsrc,
            srclist = srclist, search = search,
            pool_size = pool_size, retry = retry
        )
    } else {
        out <- rep_len(NA_character_, length(terms))
    }
    if (anyNA(out) && approximate) {
        cli::cli_alert("Running Approximate Match")
        out2 <- rxnorm_getApproximateMatch(terms[is.na(out)],
            max_entries = 1L, pool_size = pool_size, retry = retry
        )
        out[is.na(out)] <- vapply(out2, function(x) {
            if (is.null(x)) {
                NA_character_
            } else {
                x$rxcui[[1L]] %||% NA_character_
            }
        }, character(1L))
    }
    out
}

###########################################################
# rxnorm_getRxNormName("131725")
rxnorm_getRxNormName <- function(rxcuis, pool_size = 5L, retry = 0L) {
    reqs <- lapply(rxcuis, function(term) {
        rxnorm_api(.path = sprintf("rxcui/%s", term), name = term)
    })
    resps <- rxnorm_perform_parallel(
        reqs = reqs, pool_size = pool_size, retry = retry
    )
    rxnorm_return_atomic(resps, "//name")
}
# rxnorm_getDrugs("cymbalta")
rxnorm_getDrugs <- function(names, pool_size = 5L, retry = 0L) {
    reqs <- lapply(names, function(term) {
        rxnorm_api(.path = "drugs", name = term)
    })
    resps <- rxnorm_perform_parallel(
        reqs = reqs, pool_size = pool_size, retry = retry
    )
    rxnorm_return_list(resps, "//conceptProperties", names = names)
}
# rxnorm_getApproximateMatch("zocor 10 mg")
rxnorm_getApproximateMatch <- function(terms, max_entries = NULL, option = NULL, pool_size = 5L, retry = 0L) {
    reqs <- lapply(terms, function(term) {
        rxnorm_api(
            .path = "approximateTerm",
            term = term, maxEntries = max_entries, option = option
        )
    })
    resps <- rxnorm_perform_parallel(
        reqs = reqs, pool_size = pool_size, retry = retry
    )
    rxnorm_return_list(resps, "//candidate", names = terms)
}

#' @param allsrc 0: Active concepts; 1: Current concepts. Active: concepts in
#' the current RxNorm data set that have an atom with SAB=RXNORM and SUPPRESS=N;
#' Current: concepts in the current RxNorm data set that have an atom with
#' SUPPRESS=N.
#' @param srclist Source vocabularies to search (if allsrc=1)
#' @param search 0: Exact match only; 1: Normalized match; 2: Best match (exact
#' or normalized)
#' @noRd
# rxnorm_findRxcuiByString(I("Lipitor+10+mg+Tab"), search = 1L)
rxnorm_findRxcuiByString <- function(names, allsrc = NULL, srclist = NULL, search = NULL, pool_size = 5L, retry = 0L) {
    reqs <- lapply(names, function(term) {
        rxnorm_api(.path = "rxcui",
            name = term,
            allsrc = allsrc,
            srclist = srclist, search = search
        )
    })
    resps <- rxnorm_perform_parallel(
        reqs = reqs,
        pool_size = pool_size, retry = retry
    )
    rxnorm_return_atomic(resps, "//rxnormId")
}

########################################################
# rxnorm_getTermTypes()
rxnorm_getTermTypes <- function() {
    resp <- rxnorm_perform(rxnorm_api(.path = "termtypes"))
    xml <- httr2::resp_body_xml(resp, check_type = FALSE)
    xml2::xml_text(xml2::xml_find_all(xml, "//termType"))
}
# rxnorm_getRxNormVersion()
rxnorm_getRxNormVersion <- function() {
    resp <- rxnorm_perform(rxnorm_api(.path = "version"))
    xml <- xml2::xml_find_all(
        httr2::resp_body_xml(resp, check_type = FALSE),
        "//rxnormdata"
    )
    xml <- xml2::xml_children(xml)
    structure(xml2::xml_text(xml), names = xml2::xml_name(xml))
}

#########################################################
# Parse rxnorm resp list --------------------------------
rxnorm_return_list <- function(resps, xpath, names = NULL) {
    if (!is.null(names)) names(resps) <- names
    lapply(resps, rxnorm_parse_dt, xpath = xpath)
}

rxnorm_return_atomic <- function(resps, xpath) {
    vapply(resps, rxnorm_parse_scalar, character(1L), xpath = xpath)
}

rxnorm_parse_scalar <- function(resp, xpath) {
    if (resp_fail(resp)) {
        return(NA_character_)
    }
    xml <- httr2::resp_body_xml(resp)
    xml2::xml_text(xml2::xml_find_first(xml, xpath))
}

rxnorm_parse_dt <- function(resp, xpath) {
    if (resp_fail(resp)) {
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


#' @return A list of response object of the query results from RxNorm
#' @noRd
rxnorm_perform_parallel <- function(reqs, pool_size = 5L, retry = 0L, pool = NULL) {
    assert_internet()
    pool_size <- max(1L, min(pool_size, 20L))
    resps <- vector("list", length(reqs))
    groups <- seq_along(resps)
    groups <- split(groups, ceiling(groups / pool_size))
    bar_id <- cli::cli_progress_bar(
        name = "Querying RxNorm",
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_rate}] | {cli::pb_eta_str}",
        format_done = "Querying RxNorm for {.val {cli::pb_total}} quer{?y/ies} in {cli::pb_elapsed}",
        total = length(reqs)
    )
    for (idx in groups) {
        resps[idx] <- httr2::req_perform_parallel(
            reqs = reqs[idx], pool = pool,
            on_error = "continue", progress = FALSE
        )
        cli::cli_progress_update(inc = length(idx), id = bar_id)
    }
    if (retry > 0L) {
        fail <- vapply(resps, resp_fail, logical(1L))
        if (any(fail)) {
            cli::cli_alert("Retry for {sum(fail)} quer{?y/ies}")
            resps[fail] <- Recall(
                reqs = reqs[fail], pool_size = pool_size,
                retry = retry - 1L, pool = pool
            )
        }
    }
    resps
}

#########################################################
#' @noRd
rxnorm_perform <- function(req) {
    assert_internet()
    httr2::resp_check_status(httr2::req_perform(req))
}

###########################################################
#' All values in dots should be named. If rxnorm_api_name is `NULL`, `path` must
#' contain format strings used by sprintf.
#' @noRd 
rxnorm_api <- function(.path, ..., .format = "xml") {
    req <- httr2::req_url_path_append(
        req = httr2::req_url_path(req = httr2::request(rxnorm_host), "REST"),
        sprintf("%s.%s", .path, .format)
    )
    if (...length()) {
        req <- httr2::req_url_query(.req = req, ...)
    }
    rxnorm_set_headers(req)
}

rxnorm_set_headers <- function(req) {
    # https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html
    req <- httr2::req_headers(req, `Cache-Control` = "max-age=43200")
    httr2::req_cache(
        httr2::req_throttle(req, rate = 20L),
        path = faers_cache_dir("rxnorm")
    )
}

rxnorm_host <- "https://rxnav.nlm.nih.gov"

# check response --------------------
resp_non_exist <- function(resp) {
    inherits(resp, "httr2_http_404")
}

# the requst exist but failed
resp_fail <- function(resp) {
    inherits(resp, "httr2_failure")
}
