testthat::skip_on_ci()

data <- faers(c(2004, 2017),
    c("q1", "q2"), "ascii",
    dir = internal_file("extdata"),
    compress_dir = tempdir()
)
data_std <- faers_standardize(data, "~/Data/MedDRA/MedDRA_26_1_English")

testthat::test_that("standardize FAERS ascii data works well", {
    testthat::expect_true(data_std@standardization)
    testthat::expect_s3_class(data_std@meddra, "data.table")
    testthat::expect_true(all(
        names(data_std@meddra) == c(meddra_hierarchy_infos(meddra_hierarchy_fields), "primary_soc_fg")
    ))
    testthat::expect_true(all(
        setdiff(names(data_std@data$indi), names(data@data$indi)) ==
            c("meddra_idx", "meddra_hierarchy", "meddra_code", "meddra_pt")
    ))
    testthat::expect_true(all(
        setdiff(names(data_std@data$reac), names(data@data$reac)) ==
            c("meddra_idx", "meddra_hierarchy", "meddra_code", "meddra_pt")
    ))
})

testthat::test_that("de-duplicating FAERS ascii data works well", {
    testthat::expect_error(faers_dedup(data))
    testthat::expect_no_error(data_dedup <- faers_dedup(data_std))
    testthat::expect_true(data_dedup@deduplication)
    testthat::expect_equal(anyDuplicated(faers_primaryid(data_dedup)), 0L)
    # don't introduce absent primaryid
    testthat::expect_in(data_dedup$indi$primaryid, data_std$indi$primaryid)
    testthat::expect_in(data_dedup$ther$primaryid, data_std$ther$primaryid)
    testthat::expect_in(data_dedup$drug$primaryid, data_std$drug$primaryid)
    testthat::expect_in(data_dedup$demo$primaryid, data_std$demo$primaryid)
    testthat::expect_in(data_dedup$reac$primaryid, data_std$reac$primaryid)
    testthat::expect_in(data_dedup$rpsr$primaryid, data_std$rpsr$primaryid)
    testthat::expect_in(data_dedup$outc$primaryid, data_std$outc$primaryid)
})

testthat::test_that("`faers_get` for standardizated data works well", {
    meddra_cols <- names(data_std@meddra)
    testthat::expect_s3_class(faers_get(data_std, "indi"), "data.table")
    testthat::expect_s3_class(faers_get(data_std, "reac"), "data.table")
    testthat::expect_identical(
        faers_get(data_std, "indi")[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$indi$meddra_idx]
    )
    testthat::expect_identical(
        faers_get(data_std, "reac")[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$reac$meddra_idx]
    )
})

testthat::test_that("`faers_mget` for standardizated data works well", {
    meddra_cols <- names(data_std@meddra)
    data_list <- faers_mget(data_std, c("indi", "reac", "demo", "drug"))
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
    testthat::expect_identical(
        data_list$indi[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$indi$meddra_idx]
    )
    testthat::expect_identical(
        data_list$reac[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$reac$meddra_idx]
    )
})

testthat::test_that("`$` for standardizated data works well", {
    testthat::expect_s3_class(data_std$indi, "data.table")
    testthat::expect_s3_class(data_std$reac, "data.table")
    testthat::expect_in("meddra_idx", names(data_std$indi))
    testthat::expect_in("meddra_idx", names(data_std$reac))
    data_std$indi[, .temp := 1L]
    testthat::expect_in(".temp", names(data_std$indi))
    data_std$indi[, .temp := NULL]
    data_std$drug[, .temp := 1L]
    testthat::expect_in(".temp", names(data_std$drug))
    data_std$drug[, .temp := NULL]
})

testthat::test_that("`[[` for standardizated data works well", {
    testthat::expect_s3_class(data_std[["indi"]], "data.table")
    testthat::expect_s3_class(data_std[["reac"]], "data.table")
    testthat::expect_in("meddra_idx", names(data_std[["indi"]]))
    testthat::expect_in("meddra_idx", names(data_std[["reac"]]))
    data_std[["indi"]][, .temp := 1L]
    testthat::expect_in(".temp", names(data_std[["indi"]]))
    data_std[["indi"]][, .temp := NULL]
    data_std[["drug"]][, .temp := 1L]
    testthat::expect_in(".temp", names(data_std[["drug"]]))
    data_std[["drug"]][, .temp := NULL]
})

testthat::test_that("`[` for standardizated data works well", {
    meddra_cols <- names(data_std@meddra)
    data_list <- data_std[c("indi", "reac", "demo", "drug")]
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
})

testthat::test_that("`faers_merge` for standardizated data works well", {
    meddra_cols <- c(
        meddra_hierarchy_infos(meddra_hierarchy_fields),
        "primary_soc_fg", "meddra_hierarchy",
        "meddra_code", "meddra_pt"
    )
    # indi and reac included meddra columns
    indi_data <- faers_merge(data_std, "indi")
    testthat::expect_in(meddra_cols, names(indi_data))
    reac_data <- faers_merge(data_std, "reac")
    testthat::expect_in(meddra_cols, names(reac_data))

    # internal don't modify data by reference and drug_seq match well
    raw_indi <- data.table::copy(data_std$indi)
    raw_ther <- data.table::copy(data_std$ther)
    raw_drug <- data.table::copy(data_std$drug)
    raw_reac <- data.table::copy(data_std$reac)

    # indi and ther
    indi_ther <- faers_merge(data_std, c("indi", "ther"))
    testthat::expect_s3_class(indi_ther, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$ther, raw_ther)
    testthat::expect_true(all(
        !c("indi_drug_seq", "dsg_drug_seq") %in% names(indi_ther)
    ))
    testthat::expect_in("drug_seq", names(indi_ther))

    # indi and drug
    indi_drug <- faers_merge(data_std, c("indi", "drug"))
    testthat::expect_s3_class(indi_drug, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$drug, raw_drug)
    testthat::expect_true(all(
        !c("indi_drug_seq") %in% names(indi_drug)
    ))
    testthat::expect_in("drug_seq", names(indi_drug))

    # ther and drug
    ther_drug <- faers_merge(data_std, c("ther", "drug"))
    testthat::expect_s3_class(ther_drug, "data.table")
    testthat::expect_identical(data_std$ther, raw_ther)
    testthat::expect_identical(data_std$drug, raw_drug)
    testthat::expect_true(all(
        !c("ther_drug_seq") %in% names(ther_drug)
    ))
    testthat::expect_in("drug_seq", names(ther_drug))

    # indi and reac don't modify data by reference and meddra terms don't
    # confilct
    raw_reac <- data.table::copy(data_std$reac)
    indi_reac <- faers_merge(data_std, c("indi", "reac"))
    testthat::expect_s3_class(indi_reac, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$reac, raw_reac)
    testthat::expect_in("indi_drug_seq", names(indi_reac))
    testthat::expect_in(paste("indi", meddra_cols, sep = "_"), names(indi_reac))
    testthat::expect_in(paste("reac", meddra_cols, sep = "_"), names(indi_reac))

    # only one caseid column was included in the final output
    testthat::expect_true(
        sum(
            startsWith(
                "caseid",
                names(faers_merge(data_std, c("indi", "drug", "demo", "ther")))
            )
        ) == 1L
    )
})

testthat::test_that("`faers_counts` for standardizated data works well", {
    testthat::expect_error(faers_counts(data))
    testthat::expect_no_error(counts <- faers_counts(data_std))
    testthat::expect_true(is.integer(counts$N))
    testthat::expect_true(all(names(counts) == c("soc_name", "N")))
})

testthat::test_that("`faers_phv_table` for standardizated data works well", {
    testthat::expect_error(faers_phv_table(data))
    interested_ids <- sample(faers_primaryid(data_std), 100L)
    obj1 <- faers_keep(data_std, interested_ids)
    obj2 <- faers_keep(data_std, interested_ids, invert = TRUE)
    testthat::expect_no_error(phv_table <- faers_phv_table(data_std,
        filter_params = list(field = "demo", .fn = function(x) {
            interested_ids
        })
    ))
    phv_table1 <- faers_phv_table(data_std, interested = obj1)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table1))
    phv_table2 <- faers_phv_table(obj1, object2 = obj2)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table2))
})
