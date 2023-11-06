testthat::test_that("utils-file works well", {
    dir <- tempdir()
    dir.create(file.path(dir, "check_dir1"), recursive = TRUE)
    dir.create(file.path(dir, "check_dir2"), recursive = TRUE)
    file.create(file.path(dir, "check_file1"))
    file.create(file.path(dir, "check_file2"))
    testthat::expect_no_error(locate_file(dir, "^check_file1$"))
    testthat::expect_no_error(locate_file(dir, "^check_file2$"))
    testthat::expect_error(locate_file(dir, "^noneexist_file$"))
    testthat::expect_no_error(locate_files(dir))
    testthat::expect_no_error(locate_files(dir, "^check_file"))
    testthat::expect_error(locate_files(dir, "^noneexist_file$"))
    testthat::expect_no_error(locate_dir(dir, "^check_dir1$"))
    testthat::expect_no_error(locate_dir(dir, "^check_dir2$"))
    testthat::expect_error(locate_dir(dir, "^check_dir"))
    testthat::expect_error(locate_dir(dir, "^noneexist_directory$"))
})

testthat::test_that("dt_shallow() works as expected", {
    dt1 <- data.table::as.data.table(mtcars)
    x <- data.table::copy(names(dt1))
    dt2 <- dt_shallow(dt1)
    testthat::expect_false(data.table::address(dt1) == data.table::address(dt2))
    testthat::expect_identical(
        vapply(dt1, rlang::obj_address, character(1L)),
        vapply(dt2, rlang::obj_address, character(1L))
    )
    data.table::setnames(dt2, "cyl", "cyl2")
    testthat::expect_equal(names(dt2)[2L], "cyl2")
    testthat::expect_equal(names(dt1), x)
    dt2[, mpg := NULL]
    testthat::expect_equal(names(dt1), x)
    testthat::expect_equal(names(dt2)[1L], "cyl2")
})
