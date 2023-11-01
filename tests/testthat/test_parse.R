testthat::test_that("Parsing FAERS ascii data works well", {
    data <- faers_parse(
        internal_file("extdata", "aers_ascii_2004q1.zip"),
        compress_dir = tempdir()
    )
    testthat::expect_s4_class(data, "FAERSascii")
    testthat::expect_null(data@meddra)
    testthat::expect_false(data@deduplication)
    testthat::expect_false(data@standardization)
    testthat::expect_equal(data@format, "ascii")

    testthat::expect_false(anyNA(data@data$demo$primaryid))
    testthat::expect_false(anyNA(data@data$drug$primaryid))
    testthat::expect_false(anyNA(data@data$indi$primaryid))
    testthat::expect_false(anyNA(data@data$reac$primaryid))
    testthat::expect_false(anyNA(data@data$ther$primaryid))
    testthat::expect_false(anyNA(data@data$rpsr$primaryid))
    testthat::expect_false(anyNA(data@data$outc$primaryid))


    data2 <- faers_parse(internal_file("extdata", "aers_ascii_2004q1.zip"),
        compress_dir = tempdir()
    )
    testthat::expect_s4_class(data2, "FAERSascii")
    testthat::expect_null(data2@meddra)
    testthat::expect_false(data2@deduplication)
    testthat::expect_false(data2@standardization)
    testthat::expect_equal(data2@format, "ascii")
})
