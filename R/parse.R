faers_parse <- function(path, type = NULL, year = NULL, quarter = NULL, compress_dir = getwd()) {
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
    if (endsWith(path, ".zip")) {
        path <- faers_unzip(path, compress_dir)
        path <- file.path(path, type)
    } else {
        path <- file.path(path, type)
        if (!dir.exists(path)) {
            cli::cli_abort("Cannot find {.path {type}} in {.path {path}}")
        }
    }
    raw_files <- faers_list_files(path, type)
    datatable <- switch(type,
        xml = parse_xml(raw_files),
        ascii = parse_ascii(raw_files)
    )
    methods::new(paste0("FAERS", type),
        year = year, quarter = quarter, 
        datatable = datatable
    )
}

faers_unzip <- function(path, compress_dir) {
    if (!dir.exists(compress_dir)) {
        dir.create(compress_dir)
    }
    compress_dir <- file.path(
        compress_dir,
        str_replace(basename(path), "\\.zip$", "")
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
    list.files(path, pattern = pattern, full.names = TRUE)
}

parse_ascii <- function(files) {
    ids <- str_replace(basename(files), "\\d+q\\d\\.txt$", "")
    data_list <- lapply(files, function(file) {
        data.table::fread(file, sep = "$", quote = "")
    })
    data.table::setattr(data_list, "names", ids)
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
        i <- 1L
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
    out <- data.table::rbindlist(reports_list, use.names = TRUE, fill = TRUE)
    simplify_list_cols(out)
    data.table::setattr(out, "header", header)
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
