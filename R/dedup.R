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
    if (object@deduplication) {
        cli::cli_abort("You must not run {.fn faers_dedup} twice")
    }
    if (!object@standardization) {
        cli::cli_abort("{.cls FAERS} object must be standardized using {.fn faers_standardize} firstly")
    }
    deduplicated_data <- do.call(
        dedup_faers_ascii,
        object[c("demo", "drug", "indi", "ther", "reac")]
    )
    if (isTRUE(remove_deleted_cases)) {
        deleted_cases <- faers_deleted_cases(object)
        if (length(deleted_cases)) {
            deduplicated_data <- deduplicated_data[!caseid %in% deleted_cases]
        }
    }
    deduplicated_data <- deduplicated_data[, c("year", "quarter", "primaryid")]
    object@data <- lapply(object@data, function(x) {
        x[deduplicated_data, on = c("year", "quarter", "primaryid")]
    })
    object@deduplication <- TRUE
    # ..__matched_ids__.. <- unique(deduplicated_data$primaryid)
    # # we keep the latest demographic information for the patients
    # object@data$demo <- object@data$demo[
    #     deduplicated_data,
    #     on = c("year", "quarter", "primaryid")
    # ]
    # # we keep all other information for the patients
    # for (i in setdiff(faers_ascii_file_fields, "demo")) {
    #     object@data[[i]] <- object@data[[i]][primaryid %in% ..__matched_ids__..]
    # }
    object
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
    # As recommended by the FDA, a deduplication step was performed to retain
    # the most recent report for each case with the same case identifier
    # nolint start
    cli::cli_alert("deduplication from the same source by retain the most recent report")

    # While the previous Legacy AERS (LAERS) database was Individual Safety
    # Report (ISR) based, the new FAERS database is Case/Version based. In
    # LAERS, a Case consisted of one or more ISRs (Initial and Follow-up
    # reports), and each ISR number represented a separate version of a case. A
    # case could contain multiple ISRs, and the latest ISR represented the most
    # current information about a particular case. (For example, Follow-up 1
    # would have the most up-to-date information about a case containing both an
    # Initial ISR and a Follow-up 1 ISR).
    # dt <- data.table::as.data.table(mtcars)
    # bench::mark(
    #   head = dt[order(mpg, gear), head(.SD, 1L), cyl],
    #   SD = dt[order(mpg, gear), .SD[1L], cyl],
    #   I = dt[dt[, .I[1L], cyl]$V1], # should be order firstly
    #   unique = unique(dt[order(mpg, gear)], by = "cyl"),
    #   unique2 = dt[order(mpg, gear), unique(.SD, by = "cyl")],
    #   check = FALSE, iterations = 100L
    # )
    #  expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
    #   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
    # 1 head        645.4µs  763.7µs      903.    66.6KB     9.12    99     1
    # 2 SD          648.8µs  721.7µs     1344.    66.6KB    27.4     98
    # 3 I             408µs    476µs     2012.      50KB     0      100
    # 4 unique       64.1µs   76.2µs    12337.    36.1KB   125.      99
    # 5 unique2     165.8µs  177.6µs     5363.    52.6KB     0      100

    # There are also duplicate reports where the same report was submitted by a
    # consumer and by the sponsor. we have found some duplicated `primaryid`
    # with different caseid in `demo`, so we remove this by keeping the latest
    # one
    out <- unique(
        demo[order(-year, -quarter, -fda_dt, -i_f_code, -event_dt)],
        by = "primaryid"
    )
    # then we keep the latest informations for the patients
    # Such as caseid "11232882" in 2017q2 2019q2, 2019q3
    data.table::setorderv(out,
        cols = c(
            "year", "quarter", "caseversion", "fda_dt", "i_f_code", "event_dt"
        ),
        order = -1L
    )
    out <- unique(out, by = "caseid")

    # collapse all used drugs, indi, ther states, use it as a whole to identify
    # same cases.
    # match drug, indi, and ther data.
    common_keys <- c("year", "quarter", "primaryid")
    cli::cli_alert("merging `drug`, `indi`, `ther`, and `reac` data")
    out <- drug[order(drug_seq),
        list(aligned_drugs = paste0(drugname, collapse = "/")),
        by = common_keys
    ][out, on = common_keys]
    # meddra_code: indi_pt
    # 10070592	Product used for unknown indication
    # 10057097	Drug use for unknown indication
    out <- indi[!meddra_code %in% c("10070592", "10057097")][
        order(indi_drug_seq, meddra_code),
        list(aligned_indi = paste0(meddra_code, collapse = "/")),
        by = common_keys
    ][out, on = common_keys]
    out <- ther[order(dsg_drug_seq, start_dt),
        list(aligned_start_dt = paste0(start_dt, collapse = "/")),
        by = common_keys
    ][out, on = common_keys]
    # meddra_code: pt
    out <- reac[order(meddra_code),
        list(aligned_reac = paste0(meddra_code, collapse = "/")),
        by = common_keys
    ][out, on = common_keys]

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
        data.table::setorderv(out,
            cols = c("primaryid", "year", "quarter"),
            order = -1L
        )
        out <- unique(out,
            by = c(must_matched_columns, can_be_ignored_columns[-i])
        )
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
