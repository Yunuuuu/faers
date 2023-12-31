testthat::test_that("faers() function works as expected", {
    testthat::expect_no_error(data1 <- faers(2004, "q1", "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    ))
    testthat::expect_no_error(data2 <- faers(2017, "q2", "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    ))
    testthat::expect_no_error(data3 <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    ))
    testthat::expect_no_error(data3 <- faers(
        rep(c(2004, 2017), each = 2L),
        rep(c("q1", "q2"), each = 2L), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    ))
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
