testthat::test_that("`faers_dedup()` works well", {
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
    testthat::expect_error(faers_dedup(data))
    # internal don't modify data by reference and drug_seq match well
    raw_demo <- data.table::copy(data_std$demo)
    raw_drug <- data.table::copy(data_std$drug)
    raw_indi <- data.table::copy(data_std$indi)
    raw_ther <- data.table::copy(data_std$ther)
    raw_reac <- data.table::copy(data_std$reac)
    testthat::expect_no_error(data_dedup <- faers_dedup(data_std))
    testthat::expect_true(data_dedup@deduplication)
    testthat::expect_equal(anyDuplicated(faers_primaryid(data_dedup)), 0L)


    testthat::expect_identical(data_dedup$demo, raw_demo)
    testthat::expect_identical(data_dedup$drug, raw_drug)
    testthat::expect_identical(data_dedup$indi, raw_indi)
    testthat::expect_identical(data_dedup$ther, raw_ther)
    testthat::expect_identical(data_dedup$reac, raw_reac)

    # don't introduce absent primaryid
    testthat::expect_in(data_dedup$indi$primaryid, data_std$indi$primaryid)
    testthat::expect_in(data_dedup$ther$primaryid, data_std$ther$primaryid)
    testthat::expect_in(data_dedup$drug$primaryid, data_std$drug$primaryid)
    testthat::expect_in(data_dedup$demo$primaryid, data_std$demo$primaryid)
    testthat::expect_in(data_dedup$reac$primaryid, data_std$reac$primaryid)
    testthat::expect_in(data_dedup$rpsr$primaryid, data_std$rpsr$primaryid)
    testthat::expect_in(data_dedup$outc$primaryid, data_std$outc$primaryid)
})
