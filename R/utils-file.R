#' Remove caches
#'
#' @param caches An atomic character, indicates what caches to remove? Only
#' `r quote_strings(cached_nms)` can be used. If `NULL`, all caches will be
#' removed.
#' @inheritParams base::unlink
#' @return Path of the deleted directory invisiblely
#' @examples
#' faers_clearcache()
#' @export
faers_clearcache <- function(caches = NULL, force = FALSE) {
    if (is.null(caches)) {
        paths <- faers_user_cache_dir(create = FALSE)
    } else {
        assert_inclusive(caches, cached_nms)
        paths <- faers_cache_dir(caches, create = FALSE)
    }
    for (path in paths) {
        delete_cache(path, force = force)
    }
    invisible(paths)
}

cached_nms <- c("metadata", "fdadrugs", "athena")

delete_cache <- function(path, ..., name = NULL) {
    if (dir.exists(path)) {
        if (unlink(path, recursive = TRUE, ...)) {
            cli::cli_warn("Cannot remove {.path {path}}")
        } else {
            cli::cli_alert_success("Removing {.path {path}} successfully")
        }
    } else {
        if (is.null(name)) {
            msg <- "No cache"
        } else {
            msg <- "No cache found for {.field {name}}"
        }
        cli::cli_alert_info(msg)
    }
}

cache_file <- function(
    force, url, prefix,
    ext = NULL, name, dir,
    arg = rlang::caller_arg(url),
    call = rlang::caller_env()) {
    if (!force) {
        pattern <- sprintf("^%s_\\d+-\\d+-\\d+", prefix)
        if (!is.null(ext)) {
            pattern <- paste0(pattern, "\\.", ext, "$")
        } else {
            pattern <- paste0(pattern, "$")
        }
        file <- tryCatch(
            locate_files(dir, pattern, ignore.case = FALSE),
            no_file = function(cnd) {
                force <<- TRUE
            }
        )
    }
    if (force) {
        if (is.null(url)) {
            cli::cli_abort("You must provide {.arg {arg}}", call = call)
        }
        file <- cache_download(url, prefix = prefix, ext = ext, dir = dir)
    } else {
        date <- as.Date(
            str_extract(basename(file), "\\d+-\\d+-\\d+"),
            format = "%Y-%m-%d"
        )
        if (length(file) > 1L) {
            i <- order(date, decreasing = TRUE)[1L]
            file <- file[i]
            date <- date[i]
        }
        cli::cli_inform(c(
            ">" = "Using {name} from cached {.file {file}}",
            " " = "Snapshot date: {date}"
        ))
    }
    file
}

cache_download <- function(url, prefix, ext, dir, call = rlang::caller_env()) {
    assert_internet(call = call)
    file <- file.path(dir, paste(prefix, Sys.Date(), sep = "_"))
    if (!is.null(ext)) {
        file <- paste(file, ext, sep = ".")
    }
    download_inform(url, file)
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

use_files <- function(dir, use, ext = NULL) {
    if (!is.null(ext)) {
        pattern <- sprintf("\\.%s$", ext)
    } else {
        pattern <- NULL
    }
    files <- locate_files(dir, pattern)
    ids <- tolower(path_ext_remove(basename(files)))
    use <- as.character(use)
    idx <- data.table::chmatch(use, ids)
    if (anyNA(idx)) {
        cli::cli_abort(sprintf("Cannot find %s", oxford_comma(use[is.na(idx)])))
    }
    files <- files[idx]
    names(files) <- ids[idx]
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

path_ext_remove <- function(x) {
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}
