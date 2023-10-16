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
        data.table::fread(file = file)
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
