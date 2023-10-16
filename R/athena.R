athena_parse_rxnorm <- function(path = NULL, dir = faers_cache_dir("athena"), force = FALSE) {
    path <- path %||% "https://athena.ohdsi.org/api/v1/vocabularies/zip/1ff93442-eb50-4dac-a38c-a374ac383da7"
    if (startsWith(path, "https")) {
        path <- athena_download_rxnorm(path, force = force, dir = dir)
    }
    path <- dir_or_unzip(path,
        compress_dir = dir,
        pattern = "\\.zip$",
        none_msg = "Athena vocabularies Data should be a zip file"
    )
    athena_files <- locate_files(path, "\\.csv$")
    data_list <- lapply(athena_files, function(file) {
        data.table::fread(file = file)
    })
    data.table::setattr(data_list, "names", str_remove(
        tolower(basename(athena_files)), "\\.csv$"
    ))
    data_list
}

athena_download_rxnorm <- function(url, force = FALSE, dir = faers_cache_dir("athena")) {
    rxnorm_file <- file.path(dir, "athena_rxnorm.zip")
    if (isTRUE(force) && file.exists(rxnorm_file)) {
        file.remove(rxnorm_file)
    }
    download_inform(url, rxnorm_file)
}
