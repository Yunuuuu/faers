#' Tidy up faers datatable with duplicate records removed
#' @param object A [FAERSascii] or [ListOfFAERS] object.
#' @param ... Other arguments passed to specific methods.
#' @return A [data.table][data.table::data.table] object.
#' @export
#' @name faers_tidy
methods::setGeneric("faers_tidy", function(object, ...) {
    methods::makeStandardGeneric("faers_tidy")
})

#' @param fields An atomic characters specifying the fields to use. If `NULL`,
#' the de-duplicated dataset will be returned.
#' @param remove_deleted_cases If `TRUE`, will remove all
#' [deletedCases][FAERS-class] from the final result.
#' @export
#' @method faers_tidy FAERSascii
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSascii", function(object, fields = NULL, remove_deleted_cases = TRUE) {
    tidy_faers_ascii_list(object,
        fields = fields,
        remove_deleted_cases = remove_deleted_cases
    )
})

#' @export
#' @method faers_tidy ListOfFAERS
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "ListOfFAERS", function(object, fields = NULL, remove_deleted_cases = TRUE) {
    tidy_faers_ascii_list(object,
        fields = fields,
        remove_deleted_cases = remove_deleted_cases
    )
})

tidy_faers_ascii_list <- function(object, fields, remove_deleted_cases) {
    assert_inclusive(fields, faers_ascii_file_fields, null_ok = TRUE)
    lst <- faers_fields(object)
    out <- do.call(
        dedup_faers_ascii,
        lst[c("demo", "drug", "indi", "ther", "reac")]
    )
    if (isTRUE(remove_deleted_cases)) {
        deleted_cases <- faers_deleted_cases(object)
        if (length(deleted_cases)) {
            out <- out[!caseid %in% deleted_cases]
        }
    }
    if (is.null(fields)) {
        return(out)
    }
    match_id <- out[, "primaryid"]
    out <- lapply(lst[fields], function(data) {
        data[match_id, on = "primaryid"]
    })
    if (length(out) == 1L) {
        out[[1L]]
    } else {
        out
    }
}

#' @export
#' @method faers_tidy FAERSxml
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSxml", function(object) {
    cli::cli_abort("Don't implement currently")
})

#' @method faers_tidy ANY
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "ANY", function(object) {
    cli::cli_abort("Only {.cls FAERSascii}, and {.cls ListOfFAERS} can work")
})

# define_common_by <- function(x) {
#     out <- NULL
#     nms <- names(x)
#     out <- c(out, choose_one(nms, c("primaryid", "isr")))
#     out <- c(out, choose_one(nms, c("caseid", "case")))
#     out
# }
# choose_one <- function(all, choices) {
#     for (choice in choices) {
#         if (any(choice == all)) {
#             return(choice)
#         }
#     }
#     cli::cli_abort("One of {.val {choices}} must exist")
# }

# Combine data from various source tables
# demo
# drug
# indi
# ther
# reac
# Perform string aggregation operations

dedup_faers_ascii <- function(demo, drug, indi, ther, reac) {
    # As recommended by the FDA, a deduplication step was performed to retain
    # the most recent report for each case with the same case identifier
    # nolint start
    cli::cli_alert_info("deduplication from the same source by retain the most recent report")
    out <- demo[
        order(
            -primaryid, -year, -quarter,
            -caseversion, -fda_dt, -i_f_code, -event_dt
        ), .SD[1L],
        by = "caseid"
    ]
    # collapse all used drugs, indi, ther states, use it as a whole to identify
    # same cases.
    # match drug, indi, and ther data.
    cli::cli_alert_info("merging `drug`, `indi`, `ther`, and `reac` data")
    out <- drug[order(drug_seq),
        list(aligned_drugs = paste0(drugname, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    # meddra_code: indi_pt
    # [!meddra_code %in% c(10070592L, 10057097L)]
    out <- indi[
        order(indi_drug_seq, indi_pt),
        list(aligned_indi = paste0(indi_pt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    out <- ther[order(dsg_drug_seq, start_dt),
        list(aligned_start_dt = paste0(start_dt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    # meddra_code: pt
    out <- reac[order(pt),
        list(aligned_reac = paste0(pt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]

    #  consider two cases to be the same if they had a complete match of the
    #  eight criteria which are gender, age, reporting country, event date,
    #  start date, drug indications, drugs administered, and adverse reactions.
    #  Two records were also considered duplicated if they mismatch in only one
    #  of the gender, age, reporting country, event date, start date, or drug
    #  indications fields, but not the drug or adverse event fields.

    # Notes: always remember NA value in data.table, will be regarded as equal.
    # but they won't really be the same, so we should convert NA value in
    # columns used into other differentiated values, like ..__na_null__..1,
    # ..__na_null__..2, and so on.
    cli::cli_alert_info("deduplication from multiple sources by matching gender, age, reporting country, event date, start date, drug indications, drugs administered, and adverse reactions")
    can_be_ignored_columns <- c(
        "event_dt", "age_in_years", "gender", "country_code",
        "aligned_start_dt", "aligned_indi"
    )
    must_matched_columns <- c("aligned_drugs", "aligned_reac")
    all_columns <- c(must_matched_columns, can_be_ignored_columns)
    out[, (all_columns) := lapply(.SD, function(x) {
        # above `paste0` will coerced NA into "NA"
        idx <- is.na(x) | x == "NA"
        x[idx] <- paste0("..__na_null__..", seq_len(sum(idx)))
        x
    }), .SDcols = all_columns]
    for (i in seq_along(can_be_ignored_columns)) {
        out <- out[order(-primaryid, -year, -quarter), .SD[1L],
            by = c(must_matched_columns, can_be_ignored_columns[-i])
        ]
    }
    # nolint end
    out[, (all_columns) := lapply(.SD, function(x) {
        x[startsWith(x, "..__na_null__..")] <- NA
        x
    }), .SDcols = all_columns][]
}

utils::globalVariables(c(
    "drug_seq", "drugname", "indi_pt", "start_dt",
    "indi_drug_seq", "dsg_drug_seq", "pt", "primaryid", "caseversion", "fda_dt", "i_f_code", "event_dt", "year", "caseid"
))
