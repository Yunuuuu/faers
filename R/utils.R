`%||%` <- function(x, y) if (is.null(x)) y else x

# name used: metadata, rxnorm, athena
faers_cache_dir <- function(name) {
    path <- faers_cache_env[[name]]
    if (is.null(path)) {
        path <- file.path(faers_user_cache_dir(), name)
        faers_cache_env[[name]] <- dir_create2(path)
    }
    path
}

faers_user_cache_dir <- function() {
    dir_create2(rappdirs::user_cache_dir(pkg_nm()), recursive = TRUE)
}

#' Used by `faers_cache_dir` and `faers_meta_doc`
#' @noRd
faers_cache_env <- new.env()

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

assert_internet <- function(call = rlang::caller_env()) {
    if (!curl::has_internet()) {
        cli::cli_abort("No internet", call = call)
    }
}

has_name <- function(x, name) {
    any(name == rlang::names2(x))
}

fda_host <- "https://fis.fda.gov"
faers_file_format <- c("ascii", "xml")
faers_file_quarters <- c("q1", "q2", "q3", "q4")
faers_ascii_file_fields <- c("demo", "drug", "indi", "reac", "ther", "rpsr", "outc")
