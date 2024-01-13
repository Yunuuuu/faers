#' Test whether years and quarters are before specified period
#'
#' @param years An atomic integer indicates years to test.
#' @param quarters An atomic character indicates quarters to test, only "q1",
#' "q2", "q3", and "q4" are allowed.
#' @param y An integer, specifying the period year.
#' @param q A string, specifying the period quarter.
#' @param inclusive A bool, whether to include the period specifid.
#' @return An atomic logical with the same length of the max length of `years`
#' and `quarters`.
#' @examples
#' faers_before_period(c(2011, 2012), c("q1", "q3"), 2011, "q2")
#' @export
faers_before_period <- function(years, quarters, y, q, inclusive = TRUE) {
    periods <- recycle_scalar(years = years, quarters = quarters)
    assert_inclusive(quarters, faers_file_quarters)
    assert_length(y, 1L)
    if (!rlang::is_string(q, string = faers_file_quarters)) {
        cli::cli_abort("{.arg q} must be a string in {.val {faers_file_quarters}}")
    }
    assert_bool(inclusive, 1L)
    do.call(
        is_before_period,
        c(periods, list(y = y, q = q, inclusive = inclusive))
    )
}

is_before_period <- function(years, quarters, y, q, inclusive = TRUE) {
    if (isTRUE(inclusive)) {
        years < y | (years == y & quarters <= q)
    } else {
        years < y | (years == y & quarters < q)
    }
}

#' test whether from legacy aers, before 2012q3
#' @noRd
is_from_laers <- function(years, quarters) {
    is_before_period(years, quarters, 2012L, "q3")
}
