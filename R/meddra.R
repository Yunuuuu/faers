load_meddra <- function(path, use = NULL) {
    files <- locate_files(locate_dir(path, "^MedAscii$"), "\\.asc$")
    fields <- tolower(str_remove(
        basename(files), "\\.asc$",
        ignore.case = TRUE
    ))
    use <- use %||% meddra_full_fields
    idx <- match(use, fields)
    if (anyNA(idx)) {
        cli::cli_abort(sprintf(
            "Cannot find %s", oxford_comma(style_file(use[is.na(idx)]))
        ))
    }
    files <- files[idx]
    out <- lapply(files, read_ascii)
    data.table::setattr(out, "names", use)
    for (field in use) {
        data.table::setnames(out[[field]], meddra_names(field))
    }
    out
}

meddra_hierarchy_data <- function(path) {
    meddra_data <- load_meddra(path, use = c(
        "llt", "pt", "hlt_pt", "hlt", "hlgt_hlt",
        "hlgt", "soc_hlgt", "soc"
    ))
    out <- Reduce(function(x, y) {
        merge(x, y,
            by = intersect(names(x), names(y)),
            allow.cartesian = TRUE, all = TRUE
        )
    }, meddra_data)
    out[, .SD, .SDcols = meddra_hierarchy_infos(meddra_hierarchy_fields)]
}

meddra_map_code_into_names <- function(meddra_data, terms, use = c("llt", "pt")) {
    out <- rep_len(NA_character_, length(terms))
    for (i in use) {
        operated_idx <- is.na(out)
        idx <- match(
            terms[operated_idx],
            meddra_data[[paste(i, "code", sep = "_")]]
        )
        out[operated_idx] <- meddra_data[[
            paste(i, "name", sep = "_")
        ]][idx]
        if (!anyNA(out)) break
    }
    out
}

meddra_standardize_pt <- function(terms, meddra_data, use = c("llt", "pt")) {
    # ignore letter case
    terms <- toupper(terms)
    # prepare data
    pt_from <- rep_len(NA_character_, length(terms))
    out_code <- rep_len(NA_integer_, length(terms))
    idx <- rep_len(NA_integer_, length(terms))
    # order `use` based on the order in `meddra_hierarchy_fields`
    use <- intersect(meddra_hierarchy_fields, use)
    for (i in use) {
        operated_idx <- is.na(out_code)
        mapped_idx <- data.table::chmatch(
            terms[operated_idx],
            toupper(meddra_data[[paste(i, "name", sep = "_")]])
        )
        pt_from[operated_idx] <- i
        out_code[operated_idx] <- meddra_data[[
            paste(i, "code", sep = "_")
        ]][mapped_idx]
        idx[operated_idx] <- mapped_idx
        if (!anyNA(idx)) break
    }
    out <- meddra_data[idx]
    # nolint start
    out[, meddra_hierarchy := pt_from]
    out[, meddra_code := as.character(out_code)]
    out[, meddra_pt := meddra_map_code_into_names(meddra_data, meddra_code)]
    # nolint end

    # remove the low meddra hierarchy fields
    deleted_columns <- meddra_hierarchy_infos(
        use[-length(use)],
        add_soc_abbrev = FALSE
    )
    out[, .SD, .SDcols = !deleted_columns]
}

meddra_hierarchy_infos <- function(use, add_soc_abbrev = TRUE) {
    out <- paste(rep(use, each = 2L),
        rep(c("code", "name"), times = 5L),
        sep = "_"
    )
    if (add_soc_abbrev) {
        c(out, "soc_abbrev")
    } else {
        out
    }
}

meddra_hierarchy_fields <- c("llt", "pt", "hlt", "hlgt", "soc")
meddra_full_fields <- c(
    "llt", "pt", "hlt", "hlt_pt", "hlgt", "hlgt_hlt",
    "soc", "soc_hlgt", "mdhier", "intl_ord", "smq_content", "smq_list"
)

meddra_names <- function(field) {
    switch(field,
        llt = c(
            "llt_code", "llt_name", "pt_code", "llt_whoart_code",
            "llt_harts_code", "llt_costart_sym", "llt_icd9_code",
            "llt_icd9cm_code", "llt_icd10_code", "llt_currency",
            "llt_jart_code"
        ),
        pt = c(
            "pt_code", "pt_name", "null_field", "pt_soc_code",
            "pt_whoart_code", "pt_harts_code", "pt_costart_sym",
            "pt_icd9_code", "pt_icd9cm_code", "pt_icd10_code", "pt_jart_code"
        ),
        hlt = c(
            "hlt_code", "hlt_name", "hlt_whoart_code", "hlt_harts_code",
            "hlt_costart_sym", "hlt_icd9_code", "hlt_icd9cm_code",
            "hlt_icd10_code", "hlt_jart_code"
        ),
        hlt_pt = c("hlt_code", "pt_code"),
        hlgt = c(
            "hlgt_code", "hlgt_name", "hlgt_whoart_code",
            "hlgt_harts_code", "hlgt_costart_sym", "hlgt_icd9_code",
            "hlgt_icd9cm_code", "hlgt_icd10_code", "hlgt_jart_code"
        ),
        hlgt_hlt = c("hlgt_code", "hlt_code"),
        soc = c(
            "soc_code", "soc_name", "soc_abbrev", "soc_whoart_code",
            "soc_harts_code", "soc_costart_sym", "soc_icd9_code",
            "soc_icd9cm_code", "soc_icd10_code", "soc_jart_code"
        ),
        soc_hlgt = c("soc_code", "hlgt_code"),
        mdhier = c(
            "pt_code", "hlt_code", "hlgt_code", "soc_code", "pt_name",
            "hlt_name", "hlgt_name", "soc_name", "soc_abbrev",
            "null_field", "pt_soc_code", "primary_soc_fg"
        ),
        intl_ord = c("intl_ord_code", "soc_code"),
        smq_list = c(
            "SMQ_code", "SMQ_name", "SMQ_level", "SMQ_description",
            "SMQ_source", "SMQ_note", "MedDRA_version", "status",
            "SMQ_Algorithm"
        ),
        smq_content = c(
            "SMQ_code", "Term_code", "Term_level", "Term_scope",
            "Term_category", "Term_weight", "Term_status",
            "Term_addition_version", "Term_last_modified"
        )
    )
}

utils::globalVariables(c("meddra_hierarchy", "meddra_pt"))
