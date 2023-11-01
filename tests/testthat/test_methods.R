data <- faers(c(2004, 2017),
    c("q1", "q2"), "ascii",
    dir = internal_file("extdata"),
    compress_dir = tempdir()
)

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
    ids1 <- sample(faers_primaryid(data), 1L)
    data1 <- faers_keep(data, ids1)
    testthat::expect_setequal(data1$drug$primaryid, ids1)
    testthat::expect_in(data1$indi$primaryid, ids1)
    testthat::expect_in(data1$reac$primaryid, ids1)
    testthat::expect_in(data1$demo$primaryid, ids1)
    testthat::expect_in(data1$ther$primaryid, ids1)
    testthat::expect_in(data1$rpsr$primaryid, ids1)
    testthat::expect_in(data1$outc$primaryid, ids1)

    ids2 <- sample(faers_primaryid(data), 2L)
    data2 <- faers_keep(data, ids2)
    testthat::expect_setequal(data2$demo$primaryid, ids2)
    testthat::expect_in(data2$drug$primaryid, ids2)
    testthat::expect_in(data2$indi$primaryid, ids2)
    testthat::expect_in(data2$reac$primaryid, ids2)
    testthat::expect_in(data2$ther$primaryid, ids2)
    testthat::expect_in(data2$rpsr$primaryid, ids2)
    testthat::expect_in(data2$outc$primaryid, ids2)
})
