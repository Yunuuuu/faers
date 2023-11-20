testthat::test_that("`faers_merge()` for FAERS ascii data works well", {
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    # demo and drug
    demo_drug <- faers_merge(data, c("demo", "drug"))
    testthat::expect_s3_class(demo_drug, "data.table")

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

testthat::test_that("`faers_merge()` for standardizated data works well", {
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
    hierarchy_cols <- c(
        meddra_columns(meddra_hierarchy_fields),
        "meddra_hierarchy_from", "meddra_code", "meddra_pt"
    )
    # indi and reac included meddra columns
    indi_data <- faers_merge(data_std, "indi")
    testthat::expect_in(hierarchy_cols, names(indi_data))
    reac_data <- faers_merge(data_std, "reac")
    testthat::expect_in(hierarchy_cols, names(reac_data))

    # internal don't modify data by reference and drug_seq match well
    raw_indi <- data.table::copy(data_std$indi)
    raw_ther <- data.table::copy(data_std$ther)
    raw_drug <- data.table::copy(data_std$drug)
    raw_reac <- data.table::copy(data_std$reac)

    # indi and ther
    indi_ther <- faers_merge(data_std, c("indi", "ther"))
    testthat::expect_s3_class(indi_ther, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$ther, raw_ther)
    testthat::expect_true(all(
        !c("indi_drug_seq", "dsg_drug_seq") %in% names(indi_ther)
    ))
    testthat::expect_in("drug_seq", names(indi_ther))

    # indi and drug
    indi_drug <- faers_merge(data_std, c("indi", "drug"))
    testthat::expect_s3_class(indi_drug, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$drug, raw_drug)
    testthat::expect_true(all(
        !c("indi_drug_seq") %in% names(indi_drug)
    ))
    testthat::expect_in("drug_seq", names(indi_drug))

    # ther and drug
    ther_drug <- faers_merge(data_std, c("ther", "drug"))
    testthat::expect_s3_class(ther_drug, "data.table")
    testthat::expect_identical(data_std$ther, raw_ther)
    testthat::expect_identical(data_std$drug, raw_drug)
    testthat::expect_true(all(
        !c("ther_drug_seq") %in% names(ther_drug)
    ))
    testthat::expect_in("drug_seq", names(ther_drug))

    # indi and reac don't modify data by reference and meddra terms don't
    # confilct
    raw_reac <- data.table::copy(data_std$reac)
    indi_reac <- faers_merge(data_std, c("indi", "reac"))
    testthat::expect_s3_class(indi_reac, "data.table")
    testthat::expect_identical(data_std$indi, raw_indi)
    testthat::expect_identical(data_std$reac, raw_reac)
    testthat::expect_in("indi_drug_seq", names(indi_reac))
    testthat::expect_in(paste("indi", hierarchy_cols, sep = "_"), names(indi_reac))
    testthat::expect_in(paste("reac", hierarchy_cols, sep = "_"), names(indi_reac))

    # only one caseid column was included in the final output
    testthat::expect_true(
        sum(
            startsWith(
                "caseid",
                names(faers_merge(data_std, c("indi", "drug", "demo", "ther")))
            )
        ) == 1L
    )
})
