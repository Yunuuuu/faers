data <- suppressWarnings(faers(
    c(2004, 2004, 2011, 2012),
    c("q1", "q2", "q4", "q1"), "ascii",
    dir = testthat::test_path("testdata"),
    compress_dir = tempdir()
))

testthat::test_that("standardize FAERS ascii data works well", {
    testthat::expect_no_error(data_std <- faers_standardize(
        data, "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English"
    ))
    testthat::expect_true(data_std@standardization)
    testthat::expect_true(data.table::is.data.table(data_std@meddra))
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
    data_std <- faers_standardize(
        data, "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English"
    )
    testthat::expect_no_error(data_dedup <- faers_dedup(data_std))
})
