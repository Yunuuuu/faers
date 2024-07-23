testthat::test_that("meddra_load_version() works as expected", {
    testthat::expect_true(is.na(meddra_load_version("abc")))
})

testthat::test_that("meddra works well", {
    testthat::skip_if_not(dir.exists("~/Data/MedDRA/MedDRA_26_1_English"))
    path <- "~/Data/MedDRA/MedDRA_26_1_English" # nolint
    testthat::expect_true(rlang::is_string(meddra_load_version(path), "26.1"))
    testthat::expect_s3_class(data <- meddra_load_hierarchy(path), "data.table")
    testthat::expect_true(all(data$primary_soc_fg == "Y"))
    testthat::expect_s3_class(data <- meddra_load_smq(path), "data.table")

    # meddra() works well
    testthat::expect_s4_class(data <- meddra(path), "MedDRA")
    testthat::expect_true(rlang::is_string(meddra_version(data), "26.1"))
    testthat::expect_s3_class(hierarchy <- meddra_hierarchy(data), "data.table")
    testthat::expect_null(hierarchy$smq_code)
    testthat::expect_null(meddra_smq(data))

    # meddra() with add_smq = TRUE, works well
    testthat::expect_s4_class(data <- meddra(path, TRUE), "MedDRA")
    testthat::expect_true(rlang::is_string(meddra_version(data), "26.1"))
    testthat::expect_s3_class(hierarchy <- meddra_hierarchy(data), "data.table")
    testthat::expect_true(any(!is.na(hierarchy$smq_code)))
    testthat::expect_s3_class(meddra_smq(data), "data.table")
})
