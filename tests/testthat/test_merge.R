data <- suppressWarnings(faers(
    c(2004, 2004, 2011, 2012),
    c("q1", "q2", "q4", "q1"), "ascii",
    dir = testthat::test_path("testdata"),
    compress_dir = tempdir()
))

testthat::test_that("`faers_merge` for FAERS ascii data works well", {

    # internal don't modify data by reference and drug_seq match well
    raw_indi <- data.table::copy(data$indi)
    raw_ther <- data.table::copy(data$ther)
    raw_drug <- data.table::copy(data$drug)
    # indi and ther
    indi_ther <- faers_merge(data, c("indi", "ther"))
    testthat::expect_s3_class(indi_ther, "data.table")
    testthat::expect_identical(data$indi, raw_indi)
    testthat::expect_identical(data$ther, raw_ther)
    testthat::expect_true(all(
        !c("indi_drug_seq", "dsg_drug_seq") %in% names(indi_ther)
    ))
    testthat::expect_in("drug_seq", names(indi_ther))

    # indi and drug
    indi_drug <- faers_merge(data, c("indi", "drug"))
    testthat::expect_s3_class(indi_drug, "data.table")
    testthat::expect_identical(data$indi, raw_indi)
    testthat::expect_identical(data$drug, raw_drug)
    testthat::expect_true(all(
        !c("indi_drug_seq") %in% names(indi_drug)
    ))
    testthat::expect_in("drug_seq", names(indi_drug))

    # ther and drug
    ther_drug <- faers_merge(data, c("ther", "drug"))
    testthat::expect_s3_class(ther_drug, "data.table")
    testthat::expect_identical(data$ther, raw_ther)
    testthat::expect_identical(data$drug, raw_drug)
    testthat::expect_true(all(
        !c("ther_drug_seq") %in% names(ther_drug)
    ))
    testthat::expect_in("drug_seq", names(ther_drug))

    # only one caseid column was included in the final output
    testthat::expect_true(
        sum(
            startsWith(
                "caseid",
                names(faers_merge(data, c("indi", "drug", "demo", "ther")))
            )
        ) == 1L
    )
})
