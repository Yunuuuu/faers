testthat::test_that("FAERS object and extractor works well", {
    data <- suppressWarnings(faers(
        c(2004, 2004, 2011, 2012),
        c("q1", "q2", "q4", "q1"), "ascii",
        dir = testthat::test_path("testdata"),
        compress_dir = tempdir()
    ))
    data_list <- faers_data(data)
    testthat::expect_true(is.list(data_list))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
    testthat::expect_true(is.integer(faers_year(data)))
    testthat::expect_true(all(grepl("^20\\d{2}$", faers_year(data))))
    testthat::expect_true(is.character(faers_quarter(data)))
    testthat::expect_true(all(faers_quarter(data) %in% paste0("q", 1:4)))
    testthat::expect_true(all(
        paste0(faers_year(data), faers_quarter(data)) ==
            faers_period(data)
    ))
    testthat::expect_null(faers_meddra(data))
    testthat::expect_true(is.character(faers_deleted_cases(data)))
    testthat::expect_equal(length(faers_deleted_cases(data)), 0L)
})
