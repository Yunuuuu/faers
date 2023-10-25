#' Merge all FAERS field data into one
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods.
#' @details
#' Each pair of field data are merged based on "year", "quarter" and
#' "primaryid". In cases where any pair of data contains information related to
#' "drug_seq" elements, such as "drug_seq", "indi_drug_seq", or "dsg_drug_seq",
#' "drug_seq" will be aligned as well. `use` shall be organized in the
#' subsequent sequence: 'demo', 'drug', 'indi', 'reac', 'ther', 'rpsr', and
#' 'outc' and the merging sequence will correspondingly adhere to this order.
#' Only the initial instance, of the "caseid" column will be preserved.
#'
#' @return A [data.table][data.table::data.table] object.
#' @export
#' @name faers_merge
methods::setGeneric("faers_merge", function(object, ...) {
    methods::makeStandardGeneric("faers_merge")
})

#' @param use A character vector specifying the fields to use. If `NULL`, all
#' fields will be used. Note: You'd better only merge necessary data, otherwise
#' all fields will consume a lot of memory.
#' @inheritParams data.table::merge.data.table
#' @export
#' @method faers_merge FAERSascii
#' @rdname faers_merge
methods::setMethod("faers_merge", "FAERSascii", function(object, use = NULL, all = TRUE, all.x = all, all.y = all) {
    assert_inclusive(use, faers_ascii_file_fields, null_ok = TRUE)
    if (is.null(use)) {
        use <- faers_ascii_file_fields
    } else {
        use <- intersect(faers_ascii_file_fields, use)
    }

    # for LAERS, caseid only exist in `demo` data.
    # So we just keep the caseid of `demo`
    if (length(use) == 1L) {
        return(object@data[[use]])
    }
    lst <- object@data[use]
    # check if we need copy indi
    # to prevent modify in place (change the input object)
    indi_reference <- TRUE
    if (object@standardization && all(c("indi", "reac") %in% use)) {
        meddra_columns <- c(
            meddra_hierarchy_infos(meddra_hierarchy_fields),
            "primary_soc_fg", "meddra_hierarchy",
            "meddra_code", "meddra_pt"
        )
        lst$indi <- data.table::copy(lst$indi)
        indi_reference <- FALSE
        data.table::setnames(
            lst$indi, meddra_columns,
            function(x) paste("indi", x, sep = "_"),
            skip_absent = TRUE
        )
        lst$reac <- data.table::copy(lst$reac)
        data.table::setnames(
            lst$reac, meddra_columns,
            function(x) paste("reac", x, sep = "_"),
            skip_absent = TRUE
        )
    }

    # check if drug_seq should be matched
    if (sum(use %in% c("indi", "ther", "drug")) >= 2L) {
        if (any(use == "indi")) {
            if (indi_reference) lst$indi <- data.table::copy(lst$indi)
            data.table::setnames(lst$indi, "indi_drug_seq", "drug_seq")
        }
        if (any(use == "ther")) {
            lst$ther <- data.table::copy(lst$ther)
            data.table::setnames(lst$ther, "dsg_drug_seq", "drug_seq")
        }
    }

    Reduce(function(x, y) {
        if (has_name(y, "caseid") && has_name(x, "caseid")) {
            y <- y[, .SD, .SDcols = !"caseid"]
        }
        # y[x, on = intersect(names(x), names(y)),
        #     allow.cartesian = TRUE
        # ]
        merge(x, y,
            by = intersect(names(x), names(y)),
            allow.cartesian = TRUE, sort = FALSE,
            all.x = all.x, all.y = all.y
        )
    }, lst)
})

#' @export
#' @method faers_merge FAERSxml
#' @rdname faers_merge
methods::setMethod("faers_merge", "FAERSxml", function(object) {
    cli::cli_abort("Don't implement currently")
})

#' @method faers_merge ANY
#' @rdname faers_merge
methods::setMethod("faers_merge", "ANY", function(object) {
    cli::cli_abort("Only {.cls FAERSascii} can work")
})
