testthat::test_that("`faers_standardize()` works well", {
    testthat::skip_if_not(dir.exists("~/Data/MedDRA/MedDRA_26_1_English"))
    data <- faers(c(2004, 2017),
        c("q1", "q2"), "ascii",
        dir = internal_file("extdata"),
        compress_dir = tempdir()
    )
    # internal don't modify data by reference
    raw_indi <- data.table::copy(data$indi)
    raw_reac <- data.table::copy(data$reac)
    testthat::expect_no_error(data_std <- faers_standardize(data,
        "~/Data/MedDRA/MedDRA_26_1_English",
        add_smq = TRUE
    ))
    testthat::expect_identical(data$indi, raw_indi)
    testthat::expect_identical(data$reac, raw_reac)

    # other details works as expected
    testthat::expect_true(data_std@standardization)
    testthat::expect_s4_class(data_std@meddra, "MedDRA")
    testthat::expect_s4_class(faers_meddra(data_std), "MedDRA")
    testthat::expect_s3_class(faers_meddra(data_std, "hierarchy"), "data.table")
    testthat::expect_s3_class(faers_meddra(data_std, "smq"), "data.table")
    testthat::expect_true(all(
        names(data_std@meddra@hierarchy) ==
            c(meddra_columns(meddra_hierarchy_fields), "smq_code")
    ))
    testthat::expect_true(all(
        setdiff(names(data_std@data$indi), names(data@data$indi)) ==
            c(
                "meddra_hierarchy_idx", "meddra_hierarchy_from",
                "meddra_code", "meddra_pt"
            )
    ))
    testthat::expect_true(all(
        setdiff(names(data_std@data$reac), names(data@data$reac)) ==
            c(
                "meddra_hierarchy_idx", "meddra_hierarchy_from",
                "meddra_code", "meddra_pt"
            )
    ))
})
