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

locate_dir <- function(path, pattern, ignore.case = TRUE) {
    path <- list.dirs(path, recursive = FALSE)
    path <- path[str_detect(basename(path), pattern, ignore.case = ignore.case)]
    if (!length(path) || !dir.exists(path)) {
        cli::cli_abort(
            "Cannot locate {.field {pattern}} directory in {.path {path}}",
            class = "no_dir"
        )
    }
    path
}

locate_files <- function(path, pattern, ignore.case = TRUE) {
    files <- list.files(path, full.names = TRUE)
    files <- files[
        str_detect(basename(files), pattern, ignore.case = ignore.case)
    ]
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
