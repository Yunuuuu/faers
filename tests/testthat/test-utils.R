testthat::test_that("locate_files() works well", {
    testthat::expect_no_error(locate_files("."))
    testthat::expect_error(locate_dir(".", ".txt$"))
    testthat::expect_no_error(locate_files("."))
    testthat::expect_error(locate_dir(".", "noneexist_directory"))
})
