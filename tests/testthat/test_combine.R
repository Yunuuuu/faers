testthat::test_that("combine FAERS ojbect works as expected", {
    data1 <- faers_parse(
        internal_file("extdata", "aers_ascii_2004q1.zip"),
        compress_dir = tempdir()
    )
    data2 <- faers_parse(
        internal_file("extdata", "faers_ascii_2017q2.zip"),
        compress_dir = tempdir()
    )
    testthat::expect_no_error(faers_combine(data1, data2))
    testthat::expect_no_error(data3 <- faers_combine(list(data1, data2)))
    testthat::expect_error(faers_combine(data1, data1))
    testthat::expect_error(faers_combine(list(data1), list(data2)))
    testthat::expect_s4_class(data3, "FAERSascii")
    testthat::expect_false(data3@deduplication)
    testthat::expect_false(data3@standardization)
    testthat::expect_equal(data3@format, "ascii")
    testthat::expect_equal(
        nrow(data3@data$drug),
        nrow(data1@data$drug) + nrow(data2@data$drug)
    )
    testthat::expect_equal(
        nrow(data3@data$demo),
        nrow(data1@data$demo) + nrow(data2@data$demo)
    )
    testthat::expect_equal(
        nrow(data3@data$indi),
        nrow(data1@data$indi) + nrow(data2@data$indi)
    )
    testthat::expect_equal(
        nrow(data3@data$reac),
        nrow(data1@data$reac) + nrow(data2@data$reac)
    )
    testthat::expect_equal(
        nrow(data3@data$ther),
        nrow(data1@data$ther) + nrow(data2@data$ther)
    )
    testthat::expect_equal(
        nrow(data3@data$rpsr),
        nrow(data1@data$rpsr) + nrow(data2@data$rpsr)
    )
    testthat::expect_equal(
        nrow(data3@data$outc),
        nrow(data1@data$outc) + nrow(data2@data$outc)
    )
})
