`%||%` <- function(x, y) if (is.null(x)) y else x

#' test whether from legacy aers, before 2012q3
#' @noRd
is_from_laers <- function(years, quarters) {
    is_before_period(years, quarters, 2012L, "q3")
}

#' included the specified one
#' @noRd
is_before_period <- function(years, quarters, y, q) {
    years < y | (years == y & quarters <= q)
}

# file or path utils function --------------
faers_cache_dir <- function(name) {
    path <- faers_cache_env[[name]]
    if (is.null(path)) {
        path <- file.path(faers_user_cache_dir(), name)
        if (!dir.exists(path)) {
            dir.create(path)
        }
        faers_cache_env[[name]] <- path
    }
    path
}

faers_user_cache_dir <- function() {
    path <- rappdirs::user_cache_dir("faers")
    if (!dir.exists(path)) {
        dir.create(path)
    }
    path
}

#' Used by `is_installed` and `faers_meta_doc`
#' @noRd
faers_cache_env <- new.env()


dir_or_unzip <- function(path, compress_dir, pattern, none_msg, ignore.case = TRUE) {
    if (dir.exists(path)) {
        return(path)
    } else if (file.exists(path)) {
        if (str_detect(path, pattern, ignore.case = ignore.case)) {
            assert_string(compress_dir, empty_ok = FALSE)
            return(unzip2(path, compress_dir, ignore.case = ignore.case))
        } else {
            cli::cli_abort(none_msg)
        }
    } else {
        cli::cli_abort("{.path {path}} doesn't exist")
    }
}

#' Will always add the basename into the compress_dir
#' @noRd
unzip2 <- function(path, compress_dir, ignore.case = TRUE) {
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    compress_dir <- file.path(compress_dir, str_remove(
        basename(path), "\\.zip$",
        ignore.case = ignore.case
    ))
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    utils::unzip(path, exdir = compress_dir, overwrite = TRUE)
    compress_dir
}

locate_dir <- function(path, pattern, ignore.case = TRUE) {
    path <- list.dirs(path, recursive = FALSE)
    path <- path[str_detect(basename(path), pattern, ignore.case = ignore.case)]
    if (!dir.exists(path)) {
        cli::cli_abort("Cannot locate {.field {pattern}} directory in {.path {path}}")
    }
    path
}

locate_files <- function(path, pattern, ignore.case = TRUE) {
    files <- list.files(path,
        pattern = pattern, full.names = TRUE,
        ignore.case = ignore.case
    )
    if (!length(files)) {
        cli::cli_abort("Cannot locate {.field {pattern}} file in {.path {path}}")
    }
    files
}

fda_host <- "https://fis.fda.gov"
faers_file_types <- c("ascii", "xml")
faers_file_quarters <- c("q1", "q2", "q3", "q4")
faers_ascii_file_fields <- c("demo", "drug", "indi", "ther", "reac", "rpsr", "outc")
