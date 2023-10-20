#' Standardize FAERS Quarterly Data for PT and drug names
#' @param object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods.
#' @return A [data.table][data.table::data.table] object.
#' @note For `FAERSascii` method, this function will modify object in place.
#' @export
#' @name faers_standardize
methods::setGeneric("faers_standardize", function(object, ...) {
    methods::makeStandardGeneric("faers_standardize")
})

#' @param meddra_path A string, define the path of MedDRA directory.
#' @export
#' @method faers_standardize FAERSascii
#' @rdname faers_standardize
methods::setMethod("faers_standardize", "FAERSascii", function(object, meddra_path) {
    # standardize PT terms
    assert_string(meddra_path)
    meddra_data <- load_meddra(meddra_path, use = c("pt", "llt"))
    object@data$indi[, meddra_code := faers_standardize_pt(
        object@data$indi$indi_pt, meddra_data
    )]
    object@data$reac[, meddra_code := faers_standardize_pt(pt, meddra_data)]
    invisible(object)
})

faers_standardize_pt <- function(terms, meddra_data) {
    idx <- data.table::chmatch(toupper(terms), toupper(meddra_data$pt$pt_name))
    out <- meddra_data$pt$pt_code[idx]
    if (anyNA(out)) {
        idx <- data.table::chmatch(
            toupper(terms[is.na(out)]),
            toupper(meddra_data$llt$llt_name)
        )
        out[is.na(out)] <- meddra_data$llt$llt_code[idx]
    }
    out
}

faers_standardize_drug <- function(terms, athena = NULL, force = FALSE, exact = TRUE, approximate = TRUE, search = 2L) {
    standardize_drug_by_athena(terms = terms, path = athena, force = force)
}

standardize_drug_by_rxnorm <- function(terms, exact = TRUE, approximate = TRUE, search = 2, pool = 5L) {
    assert_bool(exact)
    assert_bool(approximate)
    rxnorm_map_to_rxcui(terms,
        exact = exact, approximate = approximate,
        search = search, pool = pool
    )
    # get other drug details from rxnorm
}

rxnorm_map_to_rxcui <- function(terms, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL, pool = 5L) {
    if (exact) {
        cli::cli_alert("Running Exact Match")
        out <- rxnorm_findRxcuiByString(terms,
            allsrc = allsrc,
            srclist = srclist, search = search,
            pool = pool
        )
    } else {
        out <- rep_len(NA_character_, length(terms))
    }
    if (anyNA(out) && approximate) {
        cli::cli_alert("Running Approximate Match")
        out2 <- rxnorm_getApproximateMatch(terms[is.na(out)],
            max_entries = 1L, pool = pool
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

standardize_drug_by_athena <- function(terms, path = NULL, force = FALSE) {
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
utils::globalVariables(c(
    "domain_id", "concept_id", "athena_drug_names", "meddra_code"
))
