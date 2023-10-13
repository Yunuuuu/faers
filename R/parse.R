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
        if (!any(type == faers_file_types)) {
            cli::cli_abort(c(
                "Cannot parse file type from {.arg path}",
                i = "Try to set {.arg type} manually"
            ))
        }
    } else {
        type <- match.arg(type, faers_file_types)
    }
    year <- year %||% str_extract(path, "20\\d+(?=q[1-4])")
    year <- as.integer(year)
    quarter <- quarter %||% str_extract(path, "(?<=20\\d{2})(q[1-4])")
    quarter <- as.character(quarter)
    if (dir.exists(path)) {
        path0 <- path
        path <- faers_list_zip_dir(path0, type)
        if (!dir.exists(path)) {
            cli::cli_abort("Cannot find {.path {type}} in {.path {path0}}")
        }
    } else if (file.exists(path)) {
        if (endsWith(path, ".zip")) {
            assert_string(compress_dir, empty_ok = FALSE)
            path0 <- faers_unzip(path, compress_dir)
            path <- faers_list_zip_dir(path0, type)
        } else {
            cli::cli_abort("Only compressed zip files from FAERS Quarterly Data can work")
        }
    } else {
        cli::cli_abort("{.path {path}} doesn't exist")
    }
    raw_files <- faers_list_files(path, type)
    switch(type,
        xml = parse_xml(raw_files, year, quarter),
        ascii = parse_ascii(raw_files, year, quarter)
    )
}

faers_unzip <- function(path, compress_dir) {
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    compress_dir <- file.path(
        compress_dir,
        str_remove(basename(path), "\\.zip$", ignore.case = TRUE)
    )
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    utils::unzip(path, exdir = compress_dir, overwrite = TRUE)
    compress_dir
}

faers_list_zip_dir <- function(path, type) {
    path <- list.dirs(path, recursive = FALSE)
    path[str_detect(basename(path), sprintf("^%s$", type), ignore.case = TRUE)]
}

faers_list_files <- function(path, type) {
    pattern <- switch(type,
        xml = "\\.xml$",
        ascii = "\\.txt$"
    )
    list.files(path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
}

# parse ascii files -----------------------------------
parse_ascii <- function(files, year, quarter) {
    fields <- str_remove(basename(files), "\\d+q\\d\\.txt$", ignore.case = TRUE)
    fields <- tolower(fields)
    idx <- match(faers_ascii_file_fields, fields)
    files <- files[idx]
    fields <- fields[idx]
    data_list <- .mapply(function(file, field) {
        tryCatch(
            read_ascii(file, verbose = FALSE),
            warning = function(cnd) {
                read_ascii_safe(file)
            }
        )
    }, list(file = files, field = fields), NULL)
    data.table::setattr(data_list, "names", fields)
    methods::new("FAERSascii",
        datatable = data_list,
        year = year, quarter = quarter
    )
}

read_ascii <- function(file, ...) {
    out <- data.table::fread(
        file = file,
        sep = "$", quote = "", fill = TRUE,
        blank.lines.skip = TRUE,
        na.strings = na_string,
        ...
    )
    # the last columns often messed by the presence OF '$'.
    # AS PART OF THE ROWTERMINATOR IN DATA ROWS BUT NOT IN THE HEADER ROW
    # (LAERS)
    vcolumns <- str_subset(names(out), "^V\\d+$")
    out[, (vcolumns) := lapply(.SD, function(x) {
        if (all(is.na(x))) NULL else x
    }), .SDcols = vcolumns]
}

read_ascii_safe <- function(file) {
    # data.table will stop early for some files
    # leave a lot of rows not read in
    # this is mainly due to the presence of collapsed lines (two, or more lines
    # collapsed as one line)
    file_text <- read_lines(file)
    n_seps <- str_count(file_text, "$", fixed = TRUE)
    collapsed_lines <- floor(n_seps / n_seps[2L]) > 1L
    # collapsed_lines <- collapsed_lines[collapsed_lines > 1L]
    if (any(collapsed_lines)) {
        cli::cli_warn("omiting collapsed lines: {which(collapsed_lines)}")
        file_text <- file_text[!collapsed_lines]
    }
    file_text <- str_remove(file_text[!collapsed_lines], "\\$$")
    read_text(file_text,
        sep = "$", quote = "", fill = TRUE,
        blank.lines.skip = TRUE,
        integer64 = "double"
    )
}

read_text <- function(text, ...) {
    if (!length(text)) {
        return(data.table::data.table())
    }
    file <- tempfile()
    data.table::fwrite(list(text),
        file = file,
        quote = FALSE,
        na = "NA",
        col.names = FALSE,
        logical01 = FALSE,
        showProgress = FALSE,
        compress = "none",
        verbose = FALSE
    )
    # brio::write_lines(text, file)
    on.exit(file.remove(file))
    data.table::fread(
        file = file, ...,
        na.strings = na_string,
        showProgress = FALSE
    )
}
na_string <- c("NA", "null", "NULL", "Null")
read_lines <- function(file) {
    data.table::fread(
        file = file, sep = "", header = FALSE,
        blank.lines.skip = TRUE,
        colClasses = "character",
        showProgress = FALSE
    )[[1L]]
}

# parse xml ----------------------------------------------
parse_xml <- function(file, year, quarter) {
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
    methods::new("FAERSxml",
        datatable = datatable, header = header,
        year = year, quarter = quarter
    )
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
