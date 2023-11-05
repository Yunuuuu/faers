#' Remove caches
#'
#' @param caches An atomic character, indicates what caches to remove? Only
#' `"metadata"`, `"fdadrugs"`, `"rxnorm"`, and `"athena"` can be used. If
#' `NULL`, all caches will be removed.
#' @inheritParams base::unlink
#' @return Path of the deleted directory invisiblely
#' @examples
#' faers_clearcache()
#' @export
faers_clearcache <- function(caches = NULL, force = FALSE) {
    if (is.null(caches)) {
        paths <- faers_user_cache_dir(create = FALSE)
    } else {
        assert_inclusive(caches, c("metadata", "fdadrugs", "rxnorm", "athena"))
        paths <- faers_cache_dir(caches, create = FALSE)
    }
    for (path in paths) {
        dir_delete(path, force = force)
    }
    invisible(paths)
}

dir_delete <- function(path, ...) {
    # Not deleting a non-existent file is not a failure
    if (unlink(path, recursive = TRUE, ...)) {
        cli::cli_warn("Cannot remove {.path {path}}")
    } else {
        cli::cli_alert_success("Removing {.path {path}} successfully")
    }
}

# name used: metadata, fdadrugs, rxnorm, athena
faers_cache_dir <- function(name, create = TRUE) {
    path <- file.path(faers_user_cache_dir(create = create), name)
    if (create) {
        dir_create2(path)
    } else {
        path
    }
}

faers_user_cache_dir <- function(create = TRUE) {
    path <- file.path(rappdirs::user_cache_dir("R"), pkg_nm())
    if (create) {
        dir_create2(path, recursive = TRUE)
    } else {
        path
    }
}

# file or path utils function --------------
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

# Path root in archive starts at current dir, so if /a/b/c/file and current
#    dir is /a/b, 'zip -r archive .' puts c/file in archive
zip2 <- function(zipfile, files, ..., root = getwd()) {
    old_dir <- getwd()
    on.exit(setwd(old_dir))
    setwd(root)
    utils::zip(zipfile, files, ...)
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

locate_dir <- function(path, pattern = NULL, ignore.case = TRUE) {
    path <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    if (!is.null(pattern)) {
        path <- path[
            str_detect(basename(path), pattern, ignore.case = ignore.case)
        ]
    } else {
        pattern <- "any"
    }
    if (length(path) > 1L) {
        cli::cli_abort(
            "Multiple directories matched, dir: {.path {basename(path)}}"
        )
    }
    if (!length(path) || !dir.exists(path)) {
        cli::cli_abort(
            "Cannot locate {.field {pattern}} directory in {.path {path}}",
            class = "no_dir"
        )
    }
    path
}

locate_file <- function(path, pattern = NULL, ignore.case = TRUE) {
    file <- locate_files(path, pattern = pattern, ignore.case = ignore.case)
    if (length(file) > 1L) {
        cli::cli_abort(
            "Multiple files matched, files: {.file {basename(file)}}"
        )
    }
    file
}

locate_files <- function(path, pattern = NULL, ignore.case = TRUE) {
    files <- list.files(path, full.names = TRUE)
    if (!is.null(pattern)) {
        files <- files[
            str_detect(basename(files), pattern, ignore.case = ignore.case)
        ]
    } else {
        pattern <- "any"
    }
    if (!length(files)) {
        cli::cli_abort(
            "Cannot locate {.field {pattern}} file in {.path {path}}",
            class = "no_file"
        )
    }
    files
}

dir_create2 <- function(dir, ...) {
    if (!dir.exists(dir)) {
        if (!dir.create(dir, showWarnings = FALSE, ...)) {
            cli::cli_abort("Cannot create directory {.path {dir}}")
        }
    }
    dir
}

internal_file <- function(...) {
    system.file(..., package = pkg_nm(), mustWork = TRUE)
}
