testthat::test_that("`faers_counts()` works well", {
    testthat::skip_on_ci()
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    testthat::expect_error(faers_counts(data))
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    testthat::expect_error(counts <- faers_counts(data_std, "nonexistcol"))
    testthat::expect_no_error(counts <- faers_counts(data_std))
    testthat::expect_true(is.integer(counts$N))
    testthat::expect_true(all(names(counts) == c("soc_name", "N")))
})

testthat::test_that("`faers_counts()` modifies data by reference works as expetect", {
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
    # internal don't modify data by reference
    # for reac data
    raw_reac <- data.table::copy(data_std$reac)
    testthat::expect_no_error(
        counts <- faers_counts(data_std,
            .fn = ~ .x[, new_col := NA_integer_]
        )
    )
    testthat::expect_identical(data_std$reac, raw_reac)
    testthat::expect_no_error(
        counts <- faers_counts(data_std,
            .fn = ~ .x[, meddra_pt := NULL]
        )
    )
    testthat::expect_identical(data_std$reac, raw_reac)

    # for indi data
    raw_indi <- data.table::copy(data_std$indi)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "meddra_pt",
            .fn = ~ .x[, new_col := NA_integer_],
            .field = "indi"
        )
    )
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "meddra_pt",
            .fn = ~ .x[, meddra_code := NULL],
            .field = "indi"
        )
    )
    testthat::expect_identical(data_std$indi, raw_indi)

    # for demo data
    raw_demo <- data.table::copy(data_std$demo)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "rept_cod",
            .fn = ~ .x[, new_col := NA_integer_],
            .field = "demo"
        )
    )
    testthat::expect_identical(data_std$demo[, !"new_col"], raw_demo)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "rept_cod",
            .fn = ~ .x[, new_col := NULL],
            .field = "demo"
        )
    )
    testthat::expect_identical(data_std$demo, raw_demo)

    # for drug data
    raw_drug <- data.table::copy(data_std$drug)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "route",
            .fn = ~ .x[, new_col := NA_integer_],
            .field = "drug"
        )
    )
    testthat::expect_identical(data_std$drug[, !"new_col"], raw_drug)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "route",
            .fn = ~ .x[, new_col := NULL],
            .field = "drug"
        )
    )
    testthat::expect_identical(data_std$drug, raw_drug)

    # for ther data
    raw_ther <- data.table::copy(data_std$ther)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "start_dt",
            .fn = ~ .x[, new_col := NA_integer_],
            .field = "ther"
        )
    )
    testthat::expect_identical(data_std$ther[, !"new_col"], raw_ther)
    testthat::expect_no_error(
        counts <- faers_counts(data_std, "start_dt",
            .fn = ~ .x[, new_col := NULL],
            .field = "ther"
        )
    )
    testthat::expect_identical(data_std$ther, raw_ther)
})
