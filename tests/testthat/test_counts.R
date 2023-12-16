testthat::test_that("`faers_counts()` works well", {
    testthat::skip_if_not(dir.exists("~/Data/MedDRA/MedDRA_26_1_English"))
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
