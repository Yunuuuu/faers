#' Merge all FAERS field data into one
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods.
#' @return A [data.table][data.table::data.table] object.
#' @export
#' @name faers_merge
methods::setGeneric("faers_merge", function(object, ...) {
    methods::makeStandardGeneric("faers_merge")
})

#' @param use A character vector specifying the fields to use. If `NULL`, all
#' fields will be used. Note: You'd better only merge necessary data, otherwise
#' all fields will consume a lot of memory.
#' @export
#' @method faers_merge FAERSascii
#' @rdname faers_merge
methods::setMethod("faers_merge", "FAERSascii", function(object, use = NULL) {
    assert_inclusive(use, faers_ascii_file_fields, null_ok = TRUE)
    if (is.null(use)) {
        use <- faers_ascii_file_fields
    } else {
        use <- intersect(faers_ascii_file_fields, use)
    }
    lst <- object@data[use]

    # check if drug_seq should be matched
    if (any("indi" == use) && any(use %in% c("reac", "ther", "drug"))) {
        lst$indi <- data.table::copy(lst$indi)
    }
    if (all(c("indi", "reac") %in% use)) {
        data.table::setnames(
            lst$indi,
            meddra_hierarchy_infos(meddra_hierarchy_fields),
            function(x) paste("indi", x, sep = "_"),
            skip_absent = TRUE
        )
        lst$reac <- data.table::copy(lst$reac)
        data.table::setnames(
            lst$reac,
            meddra_hierarchy_infos(meddra_hierarchy_fields),
            function(x) paste("reac", x, sep = "_"),
            skip_absent = TRUE
        )
    }

    # check if drug_seq should be matched
    if (sum(use %in% c("indi", "ther", "drug")) >= 2L) {
        if (any(use == "indi")) {
            data.table::setnames(lst$indi, "indi_drug_seq", "drug_seq")
        }
        if (any(use == "ther")) {
            lst$ther <- data.table::copy(lst$ther)
            data.table::setnames(lst$ther, "dsg_drug_seq", "drug_seq")
        }
    }
    Reduce(function(x, y) {
        merge(x, y,
            by = setdiff(intersect(names(x), names(y)), "caseid"),
            allow.cartesian = TRUE, all = TRUE
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
