#' Read and Parse ATHENA VOCABULARIES data
#' @param use An atomic character specifying the files to use with values in
#' `r quote_strings(use_athena)`.
#' @param list A boolean value, should it only list files in the ATHENA
#' VOCABULARIES data?
#' @param force A boolean value. If set to `TRUE`, it indicates the retrieval of
#' VOCABULARIES data in the `url` directly, bypassing the cache.
#' @param url A string of url for ATHENA VOCABULARIES data. You must provide it
#' to cache the file when you firstly run this function.
#' @return
#' - if `list = TRUE`, an atomic character.
#' - if `list = FALSE`, a [data.table][data.table::data.table] if `use` is a
#'   string or otherwise a list of [data.table][data.table::data.table].
#' @export
athena <- function(use = NULL, list = FALSE, force = FALSE, url = NULL) {
    assert_bool(list)
    assert_bool(force)
    assert_string(url, null_ok = TRUE)
    if (!is.null(url) && !startsWith(url, "https")) {
        cli::cli_abort("{.arg url} must start with {.val https}")
    }
    file <- athena_file(url, force)
    path <- unzip2(file, dir)
    if (list) {
        list.files(path)
    } else {
        assert_inclusive(use, use_athena, null_ok = TRUE)
        use <- use %||% use_athena
        out <- athena_loads(path, use)
        if (length(use) == 1L) {
            out[[1L]]
        } else {
            out
        }
    }
}

#' @inheritDotParams athena -use -list
#' @noRd
athena_standardize_drug <- function(terms, ...) {
    data <- athena(use = c("concept", "concept_synonym"), ..., list = FALSE)
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
    data.table::setcolorder(out, "athena_drug_names")
}

utils::globalVariables(c("domain_id", "concept_id", "athena_drug_names"))

athena_loads <- function(file, use, dir = faers_cache_dir("athena")) {
    files <- use_files(file, use = use, ext = "csv")
    lapply(files, athena_read)
}

athena_read <- function(file) {
    data.table::fread(file = file, quote = "")
}

athena_file <- function(
    url, force, dir = faers_cache_dir("athena"),
    arg = rlang::caller_arg(url)) {
    cache_file(
        force = force,
        url = url,
        prefix = "athena_vocabularies",
        ext = "zip",
        name = "ATHENA VOCABULARIES",
        dir = dir,
        arg = arg
    )
}

use_athena <- c(
    "concept", "domain", "concept_class",
    "concept_relationship", "concept_ancestor", "concept_synonym", "drug_strength", "relationship", "vocabulary"
)
