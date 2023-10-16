faers_drug_normalize <- function(terms, athena = NULL, force = FALSE, exact = TRUE, approximate = TRUE, search = 2L) {
    drug_normalize_by_athena(terms = terms, path = athena, force = force)
}

drug_normalize_by_rxnorm <- function(terms, exact = TRUE, approximate = TRUE, search = 2, pool = 20L) {
    assert_bool(exact)
    assert_bool(approximate)
    rxnorm_map_to_rxcui(terms,
        exact = exact, approximate = approximate,
        search = search, pool = pool
    )
    # get other drug details from rxnorm
}

rxnorm_map_to_rxcui <- function(terms, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL, pool = 20L) {
    if (exact) {
        out <- rxnorm_exact_map(terms,
            allsrc = allsrc,
            srclist = srclist, search = search,
            pool = pool
        )
        out <- vapply(out, get_rxnorm_item, character(1L), xpath = "//rxnormId")
    } else {
        out <- rep_len(NA_character_, length(terms))
    }
    if (anyNA(out) && approximate) {
        out2 <- rxnorm_approximate_map(terms[is.na(out)],
            max_entries = 1L, pool = pool
        )
        out[is.na(out)] <- vapply(out2, get_rxnorm_item, character(1L),
            xpath = "//rxcui"
        )
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
        data$concept$concept_id,
        data$concept_synonym$concept_id
    )
    ..__mapped_concept_ids__.. <- mapped_concept_ids[data.table::chmatch(
        str_trim(tolower(terms)), str_trim(tolower(c(
            data$concept$concept_name,
            data$concept_synonym$concept_synonym_name
        )))
    )]
    out <- data$concept[match(..__mapped_concept_ids__.., concept_id)] # nolint
    out[, athena_drug_names := terms] # nolint
    data.table::setcolorder(out, "athena_drug_names", before = 1L)
}
utils::globalVariables(c("domain_id", "concept_id", "athena_drug_names"))
