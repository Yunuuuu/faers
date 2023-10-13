#' Tidy up faers datatable
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods. For [ListOfFAERS]
#' method, other arguments passed to [FAERSascii] or [FAERSxml] method.
#' @return A [data.table][data.table::data.table] object.
#' @export
#' @name faers_tidy
methods::setGeneric("faers_tidy", function(object, ...) {
    methods::makeStandardGeneric("faers_tidy")
})

#' @param use A character specifying the field to use. If `NULL`, the
#' deduplicated data will be returned.
#' @export
#' @method faers_tidy FAERSascii
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSascii", function(object, use = NULL) {
    data_list <- faers_fields(object)
    faers_tidy_faers_ascii(data_list, use = use)
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

faers_dedup_ascii <- function(demo, drug, indi, ther, reac) {
    # nolint start
    out <- drug[order(drug_seq),
        list(aligned_drugs = paste0(drugname, collapse = "/")),
        by = "primaryid"
    ][demo, on = "primaryid"]
    # meddra_code: indi_pt
    # [!meddra_code %in% c(10070592L, 10057097L)]
    out <- indi[
        order(indi_pt, indi_drug_seq),
        list(aligned_indi = paste0(indi_pt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    out <- ther[order(start_dt, dsg_drug_seq),
        list(aligned_start_dt = paste0(start_dt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    # meddra_code: pt
    out <- reac[order(pt),
        list(aligned_reac = paste0(pt, collapse = "/")),
        by = "primaryid"
    ][out, on = "primaryid"]
    out <- out[
        order(
            -primaryid, -year, -quarter,
            -caseversion, -fda_dt, -i_f_cod, -event_dt
        ), .SD[1L],
        by = "caseid"
    ]
    can_be_ignored_columns <- c(
        "event_dt", "age_in_years", "gender", "country_code",
        "aligned_start_dt", "aligned_indi"
    )
    must_matched_columns <- c("aligned_drugs", "aligned_reac")
    for (i in seq_along(can_be_ignored_columns)) {
        out <- out[
            order(
                -primaryid, -year, -quarter,
            ), .SD[1L],
            by = c(must_matched_columns, can_be_ignored_columns[-i])
        ]
    }
    # nolint end
    out
}

#' @export
#' @method faers_tidy FAERSxml
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "FAERSxml", function(object) {
    object@data
})

#' @export
#' @method faers_tidy ListOfFAERS
#' @rdname faers_tidy
methods::setMethod("faers_tidy", "ListOfFAERS", function(object, use) {
    data_list <- lapply(faers_ascii_file_fields, function(field) {
        data.table::rbindlist(faers_field(object, field),
            fill = TRUE, use.names = TRUE
        )
    })
    data.table::setattr(data_list, "names", faers_ascii_file_fields)
    faers_tidy_faers_ascii(data_list, use = use)
})

faers_tidy_faers_ascii <- function(lst, use) {
    dedup_out <- do.call(
        faers_dedup_ascii,
        lst[c("demo", "drug", "indi", "ther", "reac")]
    )
    if (is.null(use)) {
        dedup_out
    } else {
        assert_inclusive(use, faers_ascii_file_fields)
        match_id <- dedup_out[, "primaryid"]
        lst[[use]][match_id, on = "primaryid"]
    }
}

utils::globalVariables(c("drug_seq", "drugname", "indi_pt", "start_dt", "indi_drug_seq", "dsg_drug_seq", "pt", "primaryid", "caseversion", "fda_dt", "i_f_cod", "event_dt", "year"))
