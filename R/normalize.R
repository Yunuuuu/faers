faers_drug_normalize <- function(terms, exact = TRUE, approximate = TRUE, search = 2L) {
    assert_bool(exact)
    assert_bool(approximate)
    vapply(
        cli::cli_progress_along(terms,
            name = "Parsing CUI",
            format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
            format_done = sprintf(
                "Parsing {.val {cli::pb_total}} term{?s} in {cli::pb_elapsed}"
            ),
            clear = FALSE
        ),
        function(i) {
            drug_normalize_by_network(terms[[i]],
                exact = exact, approximate = approximate,
                search = search
            )
        }, character(1L),
        USE.NAMES = FALSE
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
