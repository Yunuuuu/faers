faers_drug_normalize <- function(terms, exact = TRUE, approximate = TRUE, search = 2L) {
    assert_bool(exact)
    assert_bool(approximate)
    vapply(terms, drug_normalize_by_network, character(1L),
        exact = exact, approximate = approximate,
        search = search, USE.NAMES = FALSE
    )
}

drug_normalize_by_network <- function(term, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL) {
    if (exact) {
        out <- rxnorm_exact_map(term,
            allsrc = allsrc,
            srclist = srclist, search = search
        )
        out <- get_rxnorm_item(out, "//rxnormId")
    }
    if (is.na(out) && approximate) {
        out <- rxnorm_approximate_map(term, max_entries = 1L)
        out <- get_rxnorm_item(out, "//rxcui")
    }
    out
}
