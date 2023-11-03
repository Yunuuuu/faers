athena_standardize_drug <- function(terms, path = NULL, force = FALSE) {
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
    data.table::setcolorder(out, "athena_drug_names")
}

utils::globalVariables(c("domain_id", "concept_id", "athena_drug_names"))

#' @param use An atomic character specifying the files to use with values in
#' "concept", "domain", "concept_class", "concept_relationship",
#' "concept_ancestor", "concept_synonym", "drug_strength", "relationship",
#' "vocabulary".
#' @noRd
athena_parse <- function(use = NULL, path = NULL, dir = faers_cache_dir("athena"), force = FALSE) {
    path <- path %||% "https://athena.ohdsi.org/api/v1/vocabularies/zip/1ff93442-eb50-4dac-a38c-a374ac383da7"
    if (startsWith(path, "https")) {
        path <- athena_download(path, force = force, dir = dir)
    }
    path <- dir_or_unzip(path,
        compress_dir = dir,
        pattern = "\\.zip$",
        none_msg = "Athena vocabularies Data should be a zip file"
    )
    files <- locate_files(path, "\\.csv$")
    ids <- str_remove(tolower(basename(files)), "\\.csv$")
    use <- use %||% athena_files
    idx <- match(use, ids)
    if (anyNA(idx)) {
        cli::cli_abort(sprintf(
            "Cannot find %s",
            oxford_comma(style_file(paste0(toupper(use[is.na(idx)]), ".csv")))
        ))
    }
    files <- files[idx]
    ids <- ids[idx]
    data_list <- lapply(files, function(file) {
        data.table::fread(file = file, quote = "")
    })
    data.table::setattr(data_list, "names", ids)
    data_list
}

athena_download <- function(url, force = FALSE, dir = faers_cache_dir("athena")) {
    rxnorm_file <- file.path(dir, "athena_rxnorm.zip")
    if (isTRUE(force) && file.exists(rxnorm_file)) {
        file.remove(rxnorm_file)
    }
    download_inform(url, rxnorm_file)
}

athena_files <- c(
    "concept", "domain", "concept_class",
    "concept_relationship", "concept_ancestor", "concept_synonym", "drug_strength", "relationship", "vocabulary"
)
