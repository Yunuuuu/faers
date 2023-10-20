#' Tidy up FAERS Quarterly Data with duplicate records removed
#' @param object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods.
#' @return A [FAERSascii] object.
#' @export
#' @name faers_dedup
methods::setGeneric("faers_dedup", function(object, ...) {
    methods::makeStandardGeneric("faers_dedup")
})

#' @param remove_deleted_cases If `TRUE`, will remove all
#' [deletedCases][FAERS-class] from the final result.
#' @export
#' @method faers_dedup FAERSascii
#' @rdname faers_dedup
methods::setMethod("faers_dedup", "FAERSascii", function(object, remove_deleted_cases = TRUE) {
    if (!is.null(object@dedup)) {
        cli::cli_abort("You must not run {.fn faers_dedup} twice")
    }
    object@dedup <- do.call(
        dedup_faers_ascii,
        faers_data(object)[c("demo", "drug", "indi", "ther", "reac")]
    )
    if (isTRUE(remove_deleted_cases)) {
        deleted_cases <- faers_deleted_cases(object)
        if (length(deleted_cases)) {
            object@dedup <- object@dedup[!caseid %in% deleted_cases]
        }
    }
    faers_keep(object, primaryid = object@dedup$primaryid)
})

#' @export
#' @method faers_dedup FAERSxml
#' @rdname faers_dedup
methods::setMethod("faers_dedup", "FAERSxml", function(object) {
    cli::cli_abort("Don't implement currently")
})

#' @method faers_dedup ANY
#' @rdname faers_dedup
methods::setMethod("faers_dedup", "ANY", function(object) {
    cli::cli_abort("Only {.cls FAERSascii} can work")
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
    if (!any("meddra_code" == names(indi)) ||
        !any("meddra_code" == names(indi))) {
            cli::cli_abort("{.cls FAERS} object must be standardized firstly using {.fn faers_standardize}")
    }
    # As recommended by the FDA, a deduplication step was performed to retain
    # the most recent report for each case with the same case identifier
    # nolint start
    cli::cli_alert("deduplication from the same source by retain the most recent report")
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
    cli::cli_alert("merging `drug`, `indi`, `ther`, and `reac` data")
    out <- drug[order(drug_seq),
        list(aligned_drugs = paste0(drugname, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    # meddra_code: indi_pt
    # 10070592	Product used for unknown indication
    # 10057097	Drug use for unknown indication
    out <- indi[!meddra_code %in% c("10070592", "10057097")][
        order(indi_drug_seq, meddra_code),
        list(aligned_indi = paste0(meddra_code, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    out <- ther[order(dsg_drug_seq, start_dt),
        list(aligned_start_dt = paste0(start_dt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    # meddra_code: pt
    out <- reac[order(meddra_code),
        list(aligned_reac = paste0(meddra_code, collapse = "/")),
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
    # round age_in_years to prevent minimal differences in age
    cli::cli_alert("deduplication from multiple sources by matching gender, age, reporting country, event date, start date, drug indications, drugs administered, and adverse reactions")
    out[, age_in_years_round := round(age_in_years, 2L)]
    can_be_ignored_columns <- c(
        "event_dt", "gender", "country_code",
        "aligned_start_dt", "aligned_indi"
    )
    must_matched_columns <- c("aligned_drugs", "aligned_reac")
    all_columns <- c(must_matched_columns, can_be_ignored_columns)
    out[, c(all_columns, "age_in_years_round") := lapply(.SD, function(x) {
        # above `paste0` will coerced NA into "NA"
        idx <- is.na(x) | x == "NA"
        if (any(idx)) {
            x[idx] <- paste0("..__na_null__..", seq_len(sum(idx)))
        }
        x
    }), .SDcols = c(all_columns, "age_in_years_round")]
    for (i in seq_along(can_be_ignored_columns)) {
        out <- out[order(-primaryid, -year, -quarter), .SD[1L],
            by = c(must_matched_columns, can_be_ignored_columns[-i])
        ]
    }
    out[, age_in_years_round := NULL]
    # nolint end
    out[, (all_columns) := lapply(.SD, function(x) {
        x[startsWith(x, "..__na_null__..")] <- NA
        x
    }), .SDcols = all_columns]
    out[, c(
        "year", "quarter", "primaryid",
        "caseid", "caseversion", "fda_dt", "i_f_code",
        "event_dt", "gender", "age_in_years", "country_code",
        "aligned_drugs", "aligned_reac", "aligned_start_dt", "aligned_indi"
    )]
}

utils::globalVariables(c(
    "drug_seq", "drugname", "indi_meddra_code", "start_dt",
    "indi_drug_seq", "dsg_drug_seq", "primaryid", "caseversion", "fda_dt", "i_f_code", "event_dt", "year", "caseid", "age_in_years_round"
))
