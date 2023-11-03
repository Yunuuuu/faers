#' MedDRA class
#'
#' @description Provide a container for MedDRA Data file
#' @slot hierarchy A [data.table][data.table::data.table] or `NULL` representing
#' the meddra hierarchy data. There are five levels to the MedDRA hierarchy,
#' arranged from very specific to very general.
#' @slot smq A [data.table][data.table::data.table] or `NULL` representing the
#' meddra smq data. Standardised MedDRA Queries (SMQs) are used to support
#' signal detection and monitoring. SMQs are validated, standard sets of MedDRA
#' terms. These sets of terms have undergone extensive review, testing, analysis
#' and expert discussion. SMQs represent a variety of safety topics of
#' regulatory interest (e.g., SMQ Severe cutaneous adverse reactions, SMQ
#' Anaphylactic reaction).
#' @return 
#' - `meddra_data`: A `MedDRA` object.
#' - `meddra_hierarchy`: Extract the `hierarchy` slot.
#' - `meddra_smq`: Extract the `smq` slot.
#' @seealso
#' - <https://www.meddra.org/>
#' - <https://www.meddra.org/how-to-use/basics/hierarchy>
#' - <https://www.meddra.org/how-to-use/tools/smqs>
#' @aliases MedDRA
#' @name MedDRA-class
NULL

#' @param path A string, define the path of MedDRA directory.
#' @param add_smq A bool, indicates whether Standardised MedDRA Queries (SMQ)
#' should be added. If `TRUE`, "smq_content.asc", and "smq_list.asc" must exist.
#' @export
#' @rdname MedDRA-class
meddra_data <- function(path, add_smq = FALSE) {
    hierarchy <- meddra_load_hierarchy(path, primary_soc = TRUE)
    if (add_smq) {
        smq_data <- meddra_load_smq(path)
        smq_code <- unique(smq_data$smq_code)
        smq_code <- smq_code[
            meddra_hierarchy_match(
                hierarchy, smq_code,
                c("llt_code", "pt_code")
            )
        ]
        hierarchy[, smq_code := smq_code]
    } else {
        smq_data <- NULL
    }
    methods::new("MedDRA", hierarchy = hierarchy, smq = smq_data)
}

#' @importClassesFrom data.table data.table
methods::setClassUnion("DTOrNull", c("NULL", "data.table"))

#' @export
#' @rdname MedDRA-class
methods::setClass(
    "MedDRA",
    slots = list(hierarchy = "DTOrNull", smq = "DTOrNull"),
    prototype = list(hierarchy = NULL, smq = NULL)
)


#######################################################
#' @param object A `MedDRA` object.
#' @param ... Other arguments passed to specific methods.
#' @export
#' @aliases meddra_hierarchy
#' @rdname MedDRA-class
methods::setGeneric("meddra_hierarchy", function(object, ...) {
    methods::makeStandardGeneric("meddra_hierarchy")
})

#' @export
#' @method meddra_hierarchy MedDRA
#' @rdname MedDRA-class
methods::setMethod("meddra_hierarchy", "MedDRA", function(object) {
    object@hierarchy
})

#######################################################
#' @export
#' @aliases meddra_smq
#' @rdname MedDRA-class
methods::setGeneric("meddra_smq", function(object, ...) {
    methods::makeStandardGeneric("meddra_smq")
})

#' @export
#' @method meddra_smq MedDRA
#' @rdname MedDRA-class
methods::setMethod("meddra_smq", "MedDRA", function(object) {
    object@smq
})

##########################################################
meddra_load_smq <- function(path) {
    data <- meddra_load(path, use = c("smq_content", "smq_list"))
    out <- data$smq_content[, c("smq_code", "term_code")]
    data$smq_list[out, on = "smq_code", allow.cartesian = TRUE]
}

meddra_load_hierarchy <- function(path, primary_soc = TRUE) {
    use <- c("llt", "mdhier")
    data <- meddra_load(path, use = use)
    if (primary_soc) {
        # one PT can linked more than one hlt, we can choose the primary SOC
        is_primary <- data$mdhier[
            , list(is_primary = sum(primary_soc_fg == "Y", na.rm = TRUE)),
            by = "pt_code"
        ]
        if (all(is_primary$is_primary == 1L)) {

        } else {
            cli::cli_warn(
                "{sum(!is_primary$is_primary)} PT item{?s} do{?es}n't ha{?s/ve} a primary SOC, will choose the first one" # nolint
            )
        }
        out <- data$mdhier[, .SD[{
            .is_primary <- primary_soc_fg == "Y"
            if (any(.is_primary, na.rm = TRUE)) {
                which(.is_primary)[1L]
            } else {
                1L
            }
        }], by = "pt_code"]
    } else {
        out <- data$mdhier
    }
    cols <- meddra_columns(meddra_hierarchy_fields)
    out[data$llt, on = "pt_code", allow.cartesian = TRUE][
        , .SD,
        .SDcols = cols
    ]
}

meddra_load <- function(path, use = NULL) {
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

#' @return An integer index of terms to match meddra data
#' @noRd
meddra_hierarchy_match <- function(data, terms, use = c("llt_code", "pt_code", "hlt_code", "hlgt_code", "soc_code")) {
    out <- rep_len(NA_integer_, nrow(data))
    for (i in use) {
        operated_idx <- is.na(out)
        out[operated_idx] <- match(data[[i]][operated_idx], terms)
        if (!anyNA(out)) break
    }
    out
}

meddra_map_code_into_names <- function(
    codes, data, code_col = c("llt_code", "pt_code"),
    name_cols = c("llt_name", "pt_name")) {
    out <- rep_len(NA_character_, length(codes))
    for (i in seq_along(code_col)) {
        na_idx <- is.na(out)
        idx <- match(codes[na_idx], data[[code_col[i]]])
        out[na_idx] <- data[[name_cols[i]]][idx]
        if (!anyNA(out)) break
    }
    out
}

meddra_standardize_pt <- function(terms, data, use = c("llt", "pt")) {
    # ignore letter case
    terms <- toupper(terms)
    # prepare data
    pt_from <- rep_len(NA_character_, length(terms))
    out_code <- rep_len(NA_integer_, length(terms))
    idx <- rep_len(NA_integer_, length(terms))
    # order `use` based on the order in `meddra_hierarchy_fields`
    # from lowest to highest
    use <- intersect(meddra_hierarchy_fields, use)
    for (i in use) {
        operated_idx <- which(is.na(out_code))
        mapped_idx <- data.table::chmatch(
            terms[operated_idx],
            toupper(data[[paste(i, "name", sep = "_")]])
        )
        pt_from[operated_idx[!is.na(mapped_idx)]] <- i
        out_code[operated_idx] <- data[[
            paste(i, "code", sep = "_")
        ]][mapped_idx]
        idx[operated_idx] <- mapped_idx
        if (!anyNA(idx)) break
    }
    data.table(
        meddra_hierarchy_idx = idx,
        meddra_hierarchy_from = pt_from,
        meddra_code = as.character(out_code),
        meddra_pt = meddra_map_code_into_names(
            out_code, data,
            code_col = paste(use, "_code", sep = "_"),
            name_cols = paste(use, "name", sep = "_")
        )
    )
}

meddra_columns <- function(use, add_soc_abbrev = TRUE, add_primary_soc = TRUE) {
    out <- paste(rep(use, each = 2L),
        rep(c("code", "name"), times = length(use)),
        sep = "_"
    )
    if (add_soc_abbrev) {
        out <- c(out, "soc_abbrev")
    }
    if (add_primary_soc) {
        out <- c(out, "primary_soc_fg")
    }
    out
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
            "smq_code", "smq_name", "smq_level", "smq_description",
            "smq_source", "smq_note", "MedDRA_version", "smq_status",
            "smq_algorithm"
        ),
        smq_content = c(
            "smq_code", "term_code", "term_level", "term_scope",
            "term_category", "term_weight", "term_status",
            "term_addition_version", "term_last_modified"
        )
    )
}

utils::globalVariables(c("primary_soc_fg"))
