faers_drug_normalize <- function(terms, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = 2L) {
    assert_bool(exact)
    assert_bool(approximate)
    vapply(terms, drug_normalize, character(1L),
        exact = exact, approximate = approximate,
        allsrc = allsrc, srclist = srclist, search = search,
        USE.NAMES = FALSE
    )
}

drug_normalize <- function(term, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = 2L) {
    if (exact) {
        out <- rxnorm_exact_map(term,
            allsrc = allsrc,
            srclist = srclist, search = search
        )
        out <- xml2::xml_text(xml2::xml_find_first(
            httr2::resp_body_xml(out),
            "//rxnormId"
        ))
    }
    if (is.na(out) && approximate) {
        out <- rxnorm_approximate_map(term, max_entries = 1L)
        out <- xml2::xml_text(xml2::xml_find_first(
            httr2::resp_body_xml(out),
            "//rxcui"
        ))
    }
    out
}
