testthat::test_that("`faers_counts()` works well", {
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
    testthat::expect_error(faers_counts(data))
    testthat::expect_no_error(counts <- faers_counts(data_std))
    testthat::expect_true(is.integer(counts$N))
    testthat::expect_true(all(names(counts) == c("soc_name", "N")))
})

testthat::test_that("`faers_phv_table()` works well", {
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
    testthat::expect_error(faers_phv_table(data))
    interested_ids <- sample(faers_primaryid(data_std), 100L)
    obj1 <- faers_keep(data_std, interested_ids)
    obj2 <- faers_keep(data_std, interested_ids, invert = TRUE)
    testthat::expect_no_error(phv_table <- faers_phv_table(data_std,
        filter_params = list(field = "demo", .fn = function(x) {
            interested_ids
        })
    ))
    phv_table1 <- faers_phv_table(data_std, interested = obj1)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table1))
    phv_table2 <- faers_phv_table(obj1, object2 = obj2)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table2))
})
