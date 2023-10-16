faers_drug_normalize <- function(terms, athena = NULL, force = FALSE, exact = TRUE, approximate = TRUE, search = 2L) {
    drug_normalize_by_athena(terms = terms, path = athena, force = force)
}

drug_normalize_by_rxnorm <- function(terms, exact, approximate, search) {
    assert_bool(exact)
    assert_bool(approximate)
    vapply(
        cli::cli_progress_along(terms,
            name = "Parsing CUI",
            format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
            format_done = "Parsing {.val {cli::pb_total}} term{?s} in {cli::pb_elapsed}",
            clear = FALSE
        ),
        function(i) {
            rxnorm_map_to_rxcui(terms[[i]],
                exact = exact, approximate = approximate,
                search = search
            )
        }, character(1L),
        USE.NAMES = FALSE
    )
    # get other drug details from rxnorm
}

rxnorm_map_to_rxcui <- function(term, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL) {
    if (exact) {
        out <- rxnorm_exact_map(term,
            allsrc = allsrc,
            srclist = srclist, search = search
        )
        out <- get_rxnorm_item(out, "//rxnormId")
    } else {
        out <- NA_character_
    }
    if (is.na(out) && approximate) {
        out <- rxnorm_approximate_map(term, max_entries = 1L)
        out <- get_rxnorm_item(out, "//rxcui")
    }
    out
}

drug_normalize_by_athena <- function(terms, path = NULL, force = FALSE) {
    data <- athena_parse(
        c("concept", "concept_synonym"),
        path = path, force = force
    )
    data$concept <- data$concept[domain_id == "Drug"] # nolint
    data$concept_synonym <- data$concept_synonym[
        concept_id %in% data$concept$concept_id # nolint
    ]
    mapped_concept_ids <- c(
        data$concept_synonym$concept_id,
        data$concept$concept_id
    )
    ..__mapped_concept_ids__.. <- mapped_concept_ids[data.table::chmatch(
        tolower(terms), tolower(c(
            data$concept_synonym$concept_synonym_name,
            data$concept$concept_name
        ))
    )]
    out <- data$concept[match(..__mapped_concept_ids__.., concept_id)] # nolint
    out[, drug_names := terms] # nolint
    data.table::setcolorder(out, "drug_names", before = 1L)
}
utils::globalVariables(c("domain_id", "concept_id"))
