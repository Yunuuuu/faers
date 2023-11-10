testthat::test_that("`faers_load()` works well", {
    testthat::expect_error(faers_load(""))
    testthat::expect_error(faers_load("nonexistdata"))
    testthat::expect_s3_class(faers_load("irAEs"), "data.table")
    meta <- load_data("faers_meta_data")
    testthat::expect_true(is.list(meta))
    testthat::expect_s3_class(meta$data, "data.table")
    testthat::expect_s3_class(meta$date, "POSIXct")
})
