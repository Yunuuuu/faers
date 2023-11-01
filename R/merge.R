#' Merge all FAERS field data into one
#' @param object A [FAERSascii] or [FAERSxml] object.
#' @param ... Other arguments passed to specific methods.
#' @details
#' Each pair of field data are merged based on "year", "quarter" and
#' "primaryid". In cases where any pair of data contains information related to
#' "drug_seq" elements, such as "drug_seq", "indi_drug_seq", or "dsg_drug_seq",
#' "drug_seq" will be aligned as well. `fields` shall be organized in the
#' subsequent sequence: 'demo', 'drug', 'indi', 'reac', 'ther', 'rpsr', and
#' 'outc' and the merging sequence will correspondingly adhere to this order.
#' Only the initial instance, of the "caseid" column will be preserved.
#'
#' @return A [data.table][data.table::data.table] object.
#' @examples 
#'  # you must change `dir`, as the file included in the package is sampled
#'  data <- faers(2004, "q1", dir = system.file("extdata", package = "faers"))
#'  faers_merge(data, "indi") # only one field is just like faers_get()
#'  faers_merge(data, c("demo", "indi"))
#' @export
#' @name faers_merge
methods::setGeneric("faers_merge", function(object, ...) {
    methods::makeStandardGeneric("faers_merge")
})

#' @inheritParams faers_mget
#' @note You'd better only merge necessary data, otherwise a lot of memory will
#' be consumed to merge all fields data.
#' @inheritParams data.table::merge.data.table
#' @export
#' @method faers_merge FAERSascii
#' @rdname faers_merge
methods::setMethod("faers_merge", "FAERSascii", function(object, fields = NULL, all = TRUE, all.x = all, all.y = all) {
    assert_inclusive(fields, faers_ascii_file_fields, null_ok = TRUE)
    if (is.null(fields)) {
        fields <- faers_ascii_file_fields
    } else {
        fields <- intersect(faers_ascii_file_fields, fields)
    }

    # for LAERS, caseid only exist in `demo` data.
    # So we just keep the caseid of `demo`
    if (length(fields) == 1L) {
        return(faers_get(object, field = fields))
    }
    lst <- faers_mget(object, fields = fields)
    # check if we need copy indi
    # to prevent modify in place (change the input object)
    indi_reference <- TRUE
    if (object@standardization && all(c("indi", "reac") %in% fields)) {
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
    if (sum(fields %in% c("indi", "ther", "drug")) >= 2L) {
        if (any(fields == "indi")) {
            if (indi_reference) lst$indi <- data.table::copy(lst$indi)
            data.table::setnames(lst$indi, "indi_drug_seq", "drug_seq")
        }
        if (any(fields == "ther")) {
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
