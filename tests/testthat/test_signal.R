testthat::test_that("`faers_phv_table()` works well", {
    testthat::skip_if_not(dir.exists("~/Data/MedDRA/MedDRA_26_1_English"))
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
    testthat::expect_no_error(phv_table <- faers_phv_table(
        faers_filter(data_std, .fn = function(x) {
            interested_ids
        }, .field = "demo"),
        .full = data_std
    ))
    phv_table1 <- faers_phv_table(obj1, .full = data_std)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table1))
    phv_table2 <- faers_phv_table(obj1, .object2 = obj2)
    testthat::expect_true(data.table::fsetequal(phv_table, phv_table2))
})

testthat::test_that("`faers_phv_signal()` works well", {
    testthat::skip_if_not(dir.exists("~/Data/MedDRA/MedDRA_26_1_English"))
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English", # nolint
        add_smq = TRUE
    )
    interested_ids <- sample(faers_primaryid(data_std), 100L)
    obj1 <- faers_keep(data_std, interested_ids)
    obj2 <- faers_keep(data_std, interested_ids, invert = TRUE)
    testthat::expect_error(faers_phv_signal(obj1))
    set.seed(1L)
    testthat::expect_no_error(signal <- suppressWarnings(faers_phv_signal(
        faers_filter(data_std, .fn = function(x) {
            interested_ids
        }),
        .full = data_std
    )))
    set.seed(1L)
    signal1 <- suppressWarnings(faers_phv_signal(obj1, .full = data_std))
    testthat::expect_true(data.table::fsetequal(signal, signal1))
    set.seed(1L)
    signal2 <- suppressWarnings(faers_phv_signal(obj1, .object2 = obj2))
    testthat::expect_true(data.table::fsetequal(signal, signal2))
})
