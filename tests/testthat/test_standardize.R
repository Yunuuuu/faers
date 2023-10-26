testthat::skip_on_ci()

data <- suppressWarnings(faers(
    c(2004, 2004, 2011, 2012),
    c("q1", "q2", "q4", "q1"), "ascii",
    dir = testthat::test_path("testdata"),
    compress_dir = tempdir()
))

data_std <- faers_standardize(
    data, "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English"
)

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
        setdiff(names(data_std@data$
            reac), names(data@data$reac)) ==
            c("meddra_idx", "meddra_hierarchy", "meddra_code", "meddra_pt")
    ))
})

testthat::test_that("de-duplicating FAERS ascii data works well", {
    testthat::expect_error(faers_dedup(data))
    testthat::expect_no_error(data_dedup <- faers_dedup(data_std))
    testthat::expect_true(data_dedup@deduplication)
    testthat::expect_equal(anyDuplicated(faers_primaryid(data_dedup)), 0L)
})

testthat::test_that("`faers_get` for standardization works well", {
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

testthat::test_that("`$` for standardization works well", {
    meddra_cols <- names(data_std@meddra)
    testthat::expect_s3_class(data_std$indi, "data.table")
    testthat::expect_s3_class(data_std$reac, "data.table")
    testthat::expect_identical(
        data_std$indi[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$indi$meddra_idx]
    )
    testthat::expect_identical(
        data_std$reac[, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$reac$meddra_idx]
    )
})

testthat::test_that("`[[` for standardization works well", {
    meddra_cols <- names(data_std@meddra)
    testthat::expect_s3_class(data_std[["indi"]], "data.table")
    testthat::expect_s3_class(data_std[["reac"]], "data.table")
    testthat::expect_identical(
        data_std[["indi"]][, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$indi$meddra_idx]
    )
    testthat::expect_identical(
        data_std[["reac"]][, .SD, .SDcols = meddra_cols],
        data_std@meddra[data_std@data$reac$meddra_idx]
    )
})

testthat::test_that("`[` for standardization works well", {
    data_list <- data_std[c("indi", "reac", "demo", "drug")]
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
    meddra_cols <- names(data_std@meddra)
    for (i in c("indi", "reac")) {
        testthat::expect_identical(
            data_list[[i]][, .SD, .SDcols = meddra_cols],
            data_std@meddra[data_std@data[[i]]$meddra_idx]
        )
    }
})
