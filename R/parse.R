#' Parse FAERS Quarterly Data
#' @param path A string specifies the path of FAERS Quarterly Data. You can pass
#' the FAERS zip file directly, In this way, all files in the zip file will be
#' extracted in `compress_dir`. Or, you can also uncompressed youself, and
#' passed the directory contained the uncompressed files.
#' @inheritParams faers_download
#' @param year Year of the FAERS Quarterly Data. Coerced into integer, if
#' `NULL`, this will be extracted from path.
#' @param quarter String specifies quarter of the FAERS data, if `NULL`, this
#' will be extracted from path.
#' @param compress_dir A string specifies the directory to extract files to. It
#' will be created if necessary.
#' @return A [FAERSxml] or [FAERSascii] object.
#' @export
faers_parse <- function(path, type = NULL, year = NULL, quarter = NULL, compress_dir = getwd()) {
    assert_string(path, empty_ok = FALSE)
    if (is.null(type)) {
        type <- str_extract(basename(path), "xml|ascii", ignore.case = TRUE)
        if (!any(type == c("xml", "ascii"))) {
            cli::cli_abort(c(
                "Cannot parse file type from {.arg path}",
                i = "Try to set {.arg type} manually"
            ))
        }
    } else {
        type <- match.arg(type, c("xml", "ascii"))
    }
    year <- year %||% str_extract(path, "20\\d+(?=q[1-4])")
    year <- as.integer(year)
    quarter <- quarter %||% str_extract(path, "(?<=20\\d{2})(q[1-4])")
    quarter <- as.character(quarter)
    if (dir.exists(path)) {
        path <- file.path(path, type)
        if (!dir.exists(path)) {
            cli::cli_abort("Cannot find {.path {type}} in {.path {path}}")
        }
    } else if (file.exists(path)) {
        if (endsWith(path, ".zip")) {
            assert_string(compress_dir, empty_ok = FALSE)
            path <- faers_unzip(path, compress_dir)
            path <- file.path(path, type)
        } else {
            cli::cli_abort("Only compressed zip files from FAERS Quarterly Data can work")
        }
    } else {
        cli::cli_abort("{.path {path}} doesn't exist")
    }
    raw_files <- faers_list_files(path, type)
    datalist <- switch(type,
        xml = parse_xml(raw_files),
        ascii = parse_ascii(raw_files)
    )
    do.call(methods::new, c(datalist,
        Class = paste0("FAERS", type), year = year, quarter = quarter
    ))
}

faers_unzip <- function(path, compress_dir) {
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    compress_dir <- file.path(
        compress_dir,
        str_replace(basename(path), "\\.zip$", "", ignore.case = TRUE)
    )
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    utils::unzip(path, exdir = compress_dir, overwrite = TRUE)
    compress_dir
}

faers_list_files <- function(path, type) {
    pattern <- switch(type,
        xml = "\\.xml$",
        ascii = "\\.txt$"
    )
    list.files(path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
}

parse_ascii <- function(files) {
    ids <- str_replace(
        basename(files), "\\d+q\\d\\.txt$", "",
        ignore.case = TRUE
    )
    data_list <- lapply(files, function(file) {
        out <- tryCatch(
            data.table::fread(
                file,
                sep = "$", quote = "", fill = TRUE,
                blank.lines.skip = TRUE,
                na.strings = c("", "NA"),
                integer64 = "double"
            ),
            # data.table will stop early for some files, leave a lot of rows not
            # read in 
            warning = function(cnd) {
                out <- vroom::vroom(file, delim = "$", show_col_types = FALSE)
                # vroom will leave the separator "$" in the last column
                # and treat it as the character column. We just transformed it 
                # manually
                data.table::setDT(out)
                last_col <- rev(names(out))[1L]
                out[, (last_col) := utils::type.convert(
                    str_replace(.SD[[1L]], "\\$$", ""), as.is = TRUE
                ), .SDcols = last_col]
            }
        )
        data.table::setnames(out, tolower)
    })
    list(datatable = data.table::setattr(data_list, "names", tolower(ids)))
}

parse_xml <- function(file) {
    xml_doc <- xml2::read_xml(file)
    full_content <- xml2::xml_contents(xml_doc)
    header <- xml2::as_list(full_content[[1L]])
    reports_nodesets <- full_content[-1L]
    l <- length(reports_nodesets)
    bar_id <- cli::cli_progress_bar("Parsing reports",
        type = "iterator", total = l,
        format = "{cli::pb_spin} Parsing reports | {cli::pb_current}/{cli::pb_total}",
        format_done = "Parsing {.val {cli::pb_total}} report{?s} in {cli::pb_elapsed}",
        clear = FALSE
    )
    reports_list <- lapply(seq_len(l), function(i) {
        report <- xml2::as_list(reports_nodesets[[i]])
        report <- make_length_one_list(list_flatten(report))
        for (item in c("patient.drug", "patient.reaction")) {
            idx <- names(report) == item
            item_list <- report[idx]
            report <- report[!idx]
            lapply(item_list, function(x) data.table::setDT(x[[1L]]))
            item_dt <- data.table::rbindlist(
                unlist(item_list, recursive = FALSE, use.names = FALSE),
                use.names = TRUE, fill = TRUE
            )
            report[[item]] <- list(simplify_list_cols(item_dt))
        }
        cli::cli_progress_update(id = bar_id)
        data.table::setDT(report)
    })
    datatable <- data.table::rbindlist(reports_list,
        use.names = TRUE, fill = TRUE
    )
    simplify_list_cols(datatable)
    data.table::setnames(datatable, tolower)
    list(datatable = datatable, header = header)
}

list_flatten <- function(x, use.names = TRUE, sep = ".") {
    lst <- unlist(unname(x), recursive = FALSE)
    if (!use.names) {
        return(lst)
    }
    top_names <- rep(rlang::names2(x), times = lengths(x))
    is_top_names_good <- top_names != ""
    cur_names <- rlang::names2(lst)
    is_cur_names_good <- cur_names != ""
    new_names <- data.table::fcase(
        is_top_names_good & is_cur_names_good,
        paste(top_names, cur_names, sep = sep),
        is_top_names_good, top_names,
        is_cur_names_good, cur_names,
        default = NA_character_
    )
    data.table::setattr(lst, "names", new_names)
}

make_length_one_list <- function(lst) {
    lapply(lst, function(x) {
        if (length(x) == 1L) x else list(x)
    })
}

simplify_list_cols <- function(dt) {
    dt_names <- names(dt)
    dt[, (dt_names) := lapply(.SD, function(x) { # nolint
        x[lengths(x) == 0L] <- NA
        x[lengths(x) == 1L] <- unlist(x[lengths(x) == 1L])
        x
    }), .SDcols = dt_names]
    flatten_cols <- dt_names[
        vapply(dt, function(x) all(lengths(x) == 1L), logical(1L))
    ]
    dt[, (flatten_cols) := lapply(.SD, unlist, recursive = TRUE), # nolint
        .SDcols = flatten_cols
    ]
}
