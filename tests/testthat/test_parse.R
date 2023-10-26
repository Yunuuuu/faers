testthat::test_that("Parsing FAERS ascii data works well", {
    suppressWarnings(data <- faers_parse(
        testthat::test_path("testdata", "aers_ascii_2011q4.zip"),
        compress_dir = tempdir()
    ))
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

    testthat::expect_equal(nrow(data@data$drug), 734093)
    testthat::expect_equal(nrow(data@data$demo), 208489)
    testthat::expect_equal(nrow(data@data$indi), 362256)
    testthat::expect_equal(nrow(data@data$reac), 773537)
    testthat::expect_equal(nrow(data@data$ther), 321898)
    testthat::expect_equal(nrow(data@data$rpsr), 35032)
    testthat::expect_equal(nrow(data@data$outc), 182057)

    suppressWarnings(data2 <- faers_parse(
        testthat::test_path("testdata", "aers_ascii_2012q1.zip"),
        compress_dir = tempdir()
    ))
    testthat::expect_s4_class(data2, "FAERSascii")
    testthat::expect_null(data2@meddra)
    testthat::expect_false(data2@deduplication)
    testthat::expect_false(data2@standardization)
    testthat::expect_equal(data2@format, "ascii")
    testthat::expect_equal(nrow(data2@data$drug), 860983)
    testthat::expect_equal(nrow(data2@data$demo), 231737)
    testthat::expect_equal(nrow(data2@data$indi), 453398)
    testthat::expect_equal(nrow(data2@data$reac), 838238)
    testthat::expect_equal(nrow(data2@data$ther), 351431)
    testthat::expect_equal(nrow(data2@data$rpsr), 44313)
    testthat::expect_equal(nrow(data2@data$outc), 202719)
})
