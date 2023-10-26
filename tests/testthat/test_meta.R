testthat::test_that("Parsing FAERS metadata works well", {
    testthat::skip_if_offline()
    gds <- faers_meta()
    testthat::expect_true(data.table::is.data.table(gds))
    testthat::expect_true(
        all(names(gds) == c("year", "quarter", "period", "ascii_urls", "ascii_file_size", "xml_urls", "xml_file_size"))
    )
    testthat::expect_true(all(grepl("^20\\d{2}$", gds$year)))
    testthat::expect_true(all(gds$quarter %in% paste0("q", 1:4)))
    testthat::expect_true(all(grepl("^(October|July|April|January)", gds$period)))
})
