testthat::test_that("`faers_get()` works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    testthat::expect_s3_class(faers_get(data, "drug"), "data.table")
    testthat::expect_s3_class(faers_get(data, "indi"), "data.table")
    testthat::expect_s3_class(faers_get(data, "reac"), "data.table")
    testthat::expect_s3_class(faers_get(data, "demo"), "data.table")
    testthat::expect_s3_class(faers_get(data, "ther"), "data.table")
    testthat::expect_s3_class(faers_get(data, "rpsr"), "data.table")
    testthat::expect_s3_class(faers_get(data, "outc"), "data.table")
})

testthat::test_that("`faers_primaryid()` works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
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
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
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
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
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
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
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


testthat::test_that("`faers_get()` for standardizated data works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    hierarchy_cols <- names(data_std@meddra@hierarchy)
    # internal don't modify data by reference
    raw_demo <- data.table::copy(data_std$demo)
    raw_drug <- data.table::copy(data_std$drug)
    raw_indi <- data.table::copy(data_std$indi)
    raw_ther <- data.table::copy(data_std$ther)
    raw_reac <- data.table::copy(data_std$reac)
    faers_get(data_std, "demo")
    testthat::expect_identical(data_std$demo, raw_demo)
    faers_get(data_std, "indi")
    testthat::expect_identical(data_std$indi, raw_indi)
    faers_get(data_std, "ther")
    testthat::expect_identical(data_std$ther, raw_ther)
    faers_get(data_std, "drug")
    testthat::expect_identical(data_std$drug, raw_drug)
    faers_get(data_std, "reac")
    testthat::expect_identical(data_std$reac, raw_reac)

    # other details works as expected
    testthat::expect_s3_class(faers_get(data_std, "indi"), "data.table")
    testthat::expect_s3_class(faers_get(data_std, "reac"), "data.table")
    testthat::expect_false(anyNA(faers_get(data_std, "indi")$meddra_pt))
    testthat::expect_false(anyNA(faers_get(data_std, "reac")$meddra_pt))
    testthat::expect_identical(
        faers_get(data_std, "indi")[, .SD, .SDcols = hierarchy_cols],
        data_std@meddra@hierarchy[data_std@data$indi$meddra_hierarchy_idx]
    )
    testthat::expect_identical(
        faers_get(data_std, "reac")[, .SD, .SDcols = hierarchy_cols],
        data_std@meddra@hierarchy[data_std@data$reac$meddra_hierarchy_idx]
    )
})

testthat::test_that("`faers_mget()` for standardizated data works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    hierarchy_cols <- names(data_std@meddra@hierarchy)
    # internal don't modify data by reference and drug_seq match well
    raw_demo <- data.table::copy(data_std$demo)
    raw_drug <- data.table::copy(data_std$drug)
    raw_indi <- data.table::copy(data_std$indi)
    raw_ther <- data.table::copy(data_std$ther)
    raw_reac <- data.table::copy(data_std$reac)
    faers_mget(data_std, "demo")
    testthat::expect_identical(data_std$demo, raw_demo)
    faers_mget(data_std, "indi")
    testthat::expect_identical(data_std$indi, raw_indi)
    faers_mget(data_std, "ther")
    testthat::expect_identical(data_std$ther, raw_ther)
    faers_mget(data_std, "drug")
    testthat::expect_identical(data_std$drug, raw_drug)
    faers_mget(data_std, "reac")
    testthat::expect_identical(data_std$reac, raw_reac)

    # other details
    data_list <- faers_mget(data_std, c("indi", "reac", "demo", "drug"))
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
    testthat::expect_identical(
        data_list$indi[, .SD, .SDcols = hierarchy_cols],
        data_std@meddra@hierarchy[data_std@data$indi$meddra_hierarchy_idx]
    )
    testthat::expect_identical(
        data_list$reac[, .SD, .SDcols = hierarchy_cols],
        data_std@meddra@hierarchy[data_std@data$reac$meddra_hierarchy_idx]
    )
})

testthat::test_that("`$` for standardizated data works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    testthat::expect_s3_class(data_std$indi, "data.table")
    testthat::expect_s3_class(data_std$reac, "data.table")
    testthat::expect_in("meddra_hierarchy_idx", names(data_std$indi))
    testthat::expect_in("meddra_hierarchy_idx", names(data_std$reac))
    data_std$indi[, .temp := 1L]
    testthat::expect_in(".temp", names(data_std$indi))
    data_std$indi[, .temp := NULL]
    data_std$drug[, .temp := 1L]
    testthat::expect_in(".temp", names(data_std$drug))
    data_std$drug[, .temp := NULL]
})

testthat::test_that("`[[` for standardizated data works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    testthat::expect_s3_class(data_std[["indi"]], "data.table")
    testthat::expect_s3_class(data_std[["reac"]], "data.table")
    testthat::expect_in("meddra_hierarchy_idx", names(data_std[["indi"]]))
    testthat::expect_in("meddra_hierarchy_idx", names(data_std[["reac"]]))
    data_std[["indi"]][, .temp := 1L]
    testthat::expect_in(".temp", names(data_std[["indi"]]))
    data_std[["indi"]][, .temp := NULL]
    data_std[["drug"]][, .temp := 1L]
    testthat::expect_in(".temp", names(data_std[["drug"]]))
    data_std[["drug"]][, .temp := NULL]
})

testthat::test_that("`[` for standardizated data works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    hierarchy_cols <- names(data_std@meddra@hierarchy)
    data_list <- data_std[c("indi", "reac", "demo", "drug")]
    testthat::expect_true(is.list(data_list))
    testthat::expect_true(all(
        names(data_list) == c("indi", "reac", "demo", "drug")
    ))
    lapply(data_list, function(x) {
        testthat::expect_s3_class(x, "data.table")
    })
})

testthat::test_that("`faers_keep()` works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    ids1 <- sample(faers_primaryid(data), 1L)
    ids1_invert <- setdiff(faers_primaryid(data), ids1)
    data1 <- faers_keep(data, ids1)
    data1_invert <- faers_keep(data, ids1, invert = TRUE)
    testthat::expect_setequal(data1$demo$primaryid, ids1)
    testthat::expect_in(data1$indi$primaryid, ids1)
    testthat::expect_in(data1$reac$primaryid, ids1)
    testthat::expect_in(data1$drug$primaryid, ids1)
    testthat::expect_in(data1$ther$primaryid, ids1)
    testthat::expect_in(data1$rpsr$primaryid, ids1)
    testthat::expect_in(data1$outc$primaryid, ids1)

    testthat::expect_setequal(data1_invert$demo$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$indi$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$reac$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$drug$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$ther$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$rpsr$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$outc$primaryid, ids1_invert)

    ids2 <- sample(faers_primaryid(data), 10L)
    ids2_invert <- setdiff(faers_primaryid(data), ids2)
    data2 <- faers_keep(data, ids2)
    data2_invert <- faers_keep(data, ids2, invert = TRUE)
    testthat::expect_setequal(data2$demo$primaryid, ids2)
    testthat::expect_in(data2$drug$primaryid, ids2)
    testthat::expect_in(data2$indi$primaryid, ids2)
    testthat::expect_in(data2$reac$primaryid, ids2)
    testthat::expect_in(data2$ther$primaryid, ids2)
    testthat::expect_in(data2$rpsr$primaryid, ids2)
    testthat::expect_in(data2$outc$primaryid, ids2)

    testthat::expect_setequal(data2_invert$demo$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$drug$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$indi$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$reac$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$ther$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$rpsr$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$outc$primaryid, ids2_invert)

    # for non-exist primarids
    ids3 <- "none_exist_ids"
    data3 <- faers_keep(data, ids3)
    testthat::expect_equal(nrow(data3$drug), 0L)
    testthat::expect_equal(nrow(data3$indi), 0L)
    testthat::expect_equal(nrow(data3$reac), 0L)
    testthat::expect_equal(nrow(data3$ther), 0L)
    testthat::expect_equal(nrow(data3$rpsr), 0L)
    testthat::expect_equal(nrow(data3$outc), 0L)
    data4 <- faers_keep(data, ids3, invert = TRUE)
    testthat::expect_identical(data4, data)

    # for duplicated primarids
    ids4 <- rep(ids2, 2L)
    ids4_invert <- setdiff(faers_primaryid(data), ids4)
    data4 <- faers_keep(data, ids4)
    data4_invert <- faers_keep(data, ids4, invert = TRUE)
    testthat::expect_true(anyDuplicated(data4$demo) == 0L)
    testthat::expect_setequal(data4$demo$primaryid, ids4)
    testthat::expect_in(data4$drug$primaryid, ids4)
    testthat::expect_in(data4$indi$primaryid, ids4)
    testthat::expect_in(data4$reac$primaryid, ids4)
    testthat::expect_in(data4$ther$primaryid, ids4)
    testthat::expect_in(data4$rpsr$primaryid, ids4)
    testthat::expect_in(data4$outc$primaryid, ids4)

    testthat::expect_setequal(data4_invert$demo$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$drug$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$indi$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$reac$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$ther$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$rpsr$primaryid, ids4_invert)
    testthat::expect_in(data4_invert$outc$primaryid, ids4_invert)
})

testthat::test_that("`faers_filter()` works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    testthat::expect_error(faers_filter(data, ~FALSE))
    ids1 <- sample(faers_primaryid(data), 1L)
    ids1_invert <- setdiff(faers_primaryid(data), ids1)
    data1 <- faers_filter(data, ~ids1)
    data1_invert <- faers_filter(data, ~ids1, .invert = TRUE)
    testthat::expect_setequal(data1$demo$primaryid, ids1)
    testthat::expect_in(data1$indi$primaryid, ids1)
    testthat::expect_in(data1$reac$primaryid, ids1)
    testthat::expect_in(data1$drug$primaryid, ids1)
    testthat::expect_in(data1$ther$primaryid, ids1)
    testthat::expect_in(data1$rpsr$primaryid, ids1)
    testthat::expect_in(data1$outc$primaryid, ids1)

    testthat::expect_setequal(data1_invert$demo$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$indi$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$reac$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$drug$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$ther$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$rpsr$primaryid, ids1_invert)
    testthat::expect_in(data1_invert$outc$primaryid, ids1_invert)

    ids2 <- sample(faers_primaryid(data), 10L)
    ids2_invert <- setdiff(faers_primaryid(data), ids2)
    data2 <- faers_filter(data, ~ids2)
    data2_invert <- faers_filter(data, ~ids2, .invert = TRUE)
    testthat::expect_setequal(data2$demo$primaryid, ids2)
    testthat::expect_in(data2$drug$primaryid, ids2)
    testthat::expect_in(data2$indi$primaryid, ids2)
    testthat::expect_in(data2$reac$primaryid, ids2)
    testthat::expect_in(data2$ther$primaryid, ids2)
    testthat::expect_in(data2$rpsr$primaryid, ids2)
    testthat::expect_in(data2$outc$primaryid, ids2)

    testthat::expect_setequal(data2_invert$demo$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$drug$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$indi$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$reac$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$ther$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$rpsr$primaryid, ids2_invert)
    testthat::expect_in(data2_invert$outc$primaryid, ids2_invert)
})

testthat::test_that("`faers_modify()` works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    # for reac data
    raw_reac <- data.table::copy(data$reac)
    faers_modify(data, "reac",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data$reac$new_col)))
    faers_modify(data, "reac",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data$reac, raw_reac)
    testthat::expect_error(faers_modify(data, "reac",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "reac",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "reac",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for indi data
    raw_indi <- data.table::copy(data$indi)
    faers_modify(data, "indi",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data$indi$new_col)))
    faers_modify(data, "indi",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data$indi, raw_indi)
    testthat::expect_error(faers_modify(data, "indi",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "indi",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "indi",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for demo data
    raw_demo <- data.table::copy(data$demo)
    faers_modify(data, "demo",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data$demo$new_col)))
    faers_modify(data, "demo",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data$demo, raw_demo)
    testthat::expect_error(faers_modify(data, "demo",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "demo",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "demo",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for drug data
    raw_drug <- data.table::copy(data$drug)
    faers_modify(data, "drug",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data$drug$new_col)))
    faers_modify(data, "drug",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data$drug, raw_drug)
    testthat::expect_error(faers_modify(data, "drug",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "drug",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "drug",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for ther data
    raw_ther <- data.table::copy(data$ther)
    faers_modify(data, "ther",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data$ther$new_col)))
    faers_modify(data, "ther",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data$ther, raw_ther)
    testthat::expect_error(faers_modify(data, "ther",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "ther",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data, "ther",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))
})

testthat::test_that("`faers_modify()` for standardizated data works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    # for reac data
    raw_reac <- data.table::copy(data_std$reac)
    data1 <- faers_modify(data_std, "reac",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_identical(data_std$reac, raw_reac)
    testthat::expect_identical(data1$reac[, !"new_col"], raw_reac)
    faers_modify(data_std, "reac",
        .fn = function(x) {
            testthat::expect_in("meddra_hierarchy_idx", names(x))
            x
        }
    )
    testthat::expect_error(faers_modify(data_std, "reac",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "reac",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "reac",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "reac",
        .fn = function(x) {
            data.table::copy(x)[, meddra_hierarchy_idx := NULL]
        }
    ))

    # for indi data
    raw_indi <- data.table::copy(data_std$indi)
    data2 <- faers_modify(data_std, "indi",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data2$indi[, !"new_col"], raw_indi)
    faers_modify(data_std, "indi",
        .fn = function(x) {
            testthat::expect_in("meddra_hierarchy_idx", names(x))
            x
        }
    )
    testthat::expect_error(faers_modify(data_std, "indi",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "indi",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "indi",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "indi",
        .fn = function(x) {
            data.table::copy(x)[, meddra_hierarchy_idx := NULL]
        }
    ))

    # for demo data
    raw_demo <- data.table::copy(data_std$demo)
    faers_modify(data_std, "demo",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data_std$demo$new_col)))
    faers_modify(data_std, "demo",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data_std$demo, raw_demo)
    testthat::expect_error(faers_modify(data_std, "demo",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "demo",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "demo",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for drug data
    raw_drug <- data.table::copy(data_std$drug)
    faers_modify(data_std, "drug",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data_std$drug$new_col)))
    faers_modify(data_std, "drug",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data_std$drug, raw_drug)
    testthat::expect_error(faers_modify(data_std, "drug",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "drug",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "drug",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))

    # for ther data
    raw_ther <- data.table::copy(data_std$ther)
    faers_modify(data_std, "ther",
        .fn = ~ .x[, new_col := NA_integer_]
    )
    testthat::expect_true(all(is.na(data_std$ther$new_col)))
    faers_modify(data_std, "ther",
        .fn = ~ .x[, new_col := NULL]
    )
    testthat::expect_identical(data_std$ther, raw_ther)
    testthat::expect_error(faers_modify(data_std, "ther",
        .fn = function(x) {
            data.table::copy(x)[, year := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "ther",
        .fn = function(x) {
            data.table::copy(x)[, quarter := NULL]
        }
    ))
    testthat::expect_error(faers_modify(data_std, "ther",
        .fn = function(x) {
            data.table::copy(x)[, primaryid := NULL]
        }
    ))
})
