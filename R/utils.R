`%||%` <- function(x, y) if (is.null(x)) y else x

#' test whether from legacy aers, before 2012q3
#' @noRd
is_from_laers <- function(years, quarters) {
    is_before_period(years, quarters, 2012L, "q3")
}

#' Test whether years and quarters are before specified period
#'
#' @param years An atomic integer indicates years to test.
#' @param quarters An atomic character indicates quarters to test, only "q1",
#' "q2", "q3", and "q4" are allowed.
#' @param y An integer, specified period year.
#' @param q An string, specified period quarter.
#' @param inclusive A bool, whether to include the period specifid.
#' @return An atomic logical with the same length of the max length of `years`
#' and `quarters`.
#' @export
faers_before_period <- function(years, quarters, y, q, inclusive = TRUE) {
    periods <- recycle_scalar(years = years, quarters = quarters)
    assert_inclusive(quarters, faers_file_quarters)
    assert_length(y, 1L)
    assert_length(q, 1L)
    assert_bool(inclusive, 1L)
    do.call(
        is_before_period,
        c(periods, list(y = y, q = q, inclusive = inclusive))
    )
}

is_before_period <- function(years, quarters, y, q, inclusive = TRUE) {
    if (isTRUE(inclusive)) {
        years < y | (years == y & quarters <= q)
    } else {
        years < y | (years == y & quarters < q)
    }
}

# file or path utils function --------------
faers_cache_dir <- function(name) {
    path <- faers_cache_env[[name]]
    if (is.null(path)) {
        path <- file.path(faers_user_cache_dir(), name)
        faers_cache_env[[name]] <- dir_create2(path)
    }
    path
}

faers_user_cache_dir <- function() {
    dir_create2(rappdirs::user_cache_dir("faers"))
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
    compress_dir <- file.path(dir_create2(compress_dir), str_remove(
        basename(path), "\\.zip$",
        ignore.case = ignore.case
    ))
    if (is.null(utils::unzip(path, exdir = dir_create2(compress_dir), overwrite = TRUE))) {
        cli::cli_abort("Cannot uncompress {.file {path}}")
    }
    compress_dir
}

locate_dir <- function(path, pattern, ignore.case = TRUE) {
    path <- list.dirs(path, recursive = FALSE)
    path <- path[str_detect(basename(path), pattern, ignore.case = ignore.case)]
    if (!length(path) || !dir.exists(path)) {
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

dir_create2 <- function(dir) {
    if (!dir.exists(dir)) {
        if (!dir.create(dir, showWarnings = FALSE)) {
            cli::cli_abort("Cannot create directory {.path {dir}}")
        }
    }
    dir
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
