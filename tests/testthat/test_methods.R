data <- suppressWarnings(faers(
    c(2004, 2004, 2011, 2012),
    c("q1", "q2", "q4", "q1"), "ascii",
    dir = testthat::test_path("testdata"),
    compress_dir = tempdir()
))

testthat::test_that("faers_get works well", {
    testthat::expect_s3_class(faers_get(data, "drug"), "data.table")
    testthat::expect_s3_class(faers_get(data, "indi"), "data.table")
    testthat::expect_s3_class(faers_get(data, "reac"), "data.table")
    testthat::expect_s3_class(faers_get(data, "demo"), "data.table")
    testthat::expect_s3_class(faers_get(data, "ther"), "data.table")
    testthat::expect_s3_class(faers_get(data, "rpsr"), "data.table")
    testthat::expect_s3_class(faers_get(data, "outc"), "data.table")
})

testthat::test_that("faers_primaryid works well", {
    x <- faers_primaryid(data)
    testthat::expect_identical(faers_get(data, "demo")$primaryid, x)
    testthat::expect_in(faers_get(data, "drug")$primaryid, x)
    testthat::expect_in(faers_get(data, "indi")$primaryid, x)
    testthat::expect_in(faers_get(data, "reac")$primaryid, x)
    testthat::expect_in(faers_get(data, "ther")$primaryid, x)
    testthat::expect_in(faers_get(data, "rpsr")$primaryid, x)
    testthat::expect_in(faers_get(data, "outc")$primaryid, x)
})

testthat::test_that("`[` works well", {
    data_list <- data[c("indi", "reac", "demo", "drug")]
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })

    data_list <- data[1:3]
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(names(data_list) == names(data@data[1:3])))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
})

testthat::test_that("`[[` works well", {
    testthat::expect_s3_class(data[["drug"]], "data.table")
    testthat::expect_s3_class(data[["indi"]], "data.table")
    testthat::expect_s3_class(data[["reac"]], "data.table")
    testthat::expect_s3_class(data[["demo"]], "data.table")
    testthat::expect_s3_class(data[["ther"]], "data.table")
    testthat::expect_s3_class(data[["rpsr"]], "data.table")
    testthat::expect_s3_class(data[["outc"]], "data.table")

    testthat::expect_s3_class(data[[1]], "data.table")
    testthat::expect_s3_class(data[[2]], "data.table")
    testthat::expect_s3_class(data[[3]], "data.table")
    testthat::expect_s3_class(data[[4]], "data.table")
    testthat::expect_s3_class(data[[5]], "data.table")
    testthat::expect_s3_class(data[[6]], "data.table")
    testthat::expect_s3_class(data[[7]], "data.table")
})

testthat::test_that("`$` works well", {
    testthat::expect_s3_class(data$drug, "data.table")
    testthat::expect_s3_class(data$indi, "data.table")
    testthat::expect_s3_class(data$reac, "data.table")
    testthat::expect_s3_class(data$demo, "data.table")
    testthat::expect_s3_class(data$ther, "data.table")
    testthat::expect_s3_class(data$rpsr, "data.table")
    testthat::expect_s3_class(data$outc, "data.table")

    testthat::expect_null(data$`1`)
    testthat::expect_null(data$`2`)
    testthat::expect_null(data$`3`)
    testthat::expect_null(data$`4`)
    testthat::expect_null(data$`5`)
    testthat::expect_null(data$`6`)
    testthat::expect_null(data$`7`)
})

testthat::test_that("faers_keep works well", {
    data1 <- faers_keep(data, "4204616")
    testthat::expect_setequal(data1$drug$primaryid, "4204616")
    testthat::expect_setequal(data1$indi$primaryid, "4204616")
    testthat::expect_setequal(data1$reac$primaryid, "4204616")
    testthat::expect_setequal(data1$demo$primaryid, "4204616")
    testthat::expect_setequal(data1$ther$primaryid, "4204616")
    testthat::expect_setequal(data1$rpsr$primaryid, "4204616")
    testthat::expect_setequal(data1$outc$primaryid, "4204616")

    data <- faers_keep(data, c("4204616", "4261678"))
    testthat::expect_setequal(data$demo$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$drug$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$indi$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$reac$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$ther$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$rpsr$primaryid, c("4204616", "4261678"))
    testthat::expect_setequal(data$outc$primaryid, c("4204616", "4261678"))
})
