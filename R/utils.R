`%||%` <- function(x, y) if (is.null(x)) y else x

#' test whether from legacy aers, before 2012q3
#' @noRd 
is_from_laers <- function(years, quarters) {
    is_before_period(years, quarters, 2012L, "q3")
}

#' included the specified one
#' @noRd
is_before_period <- function(years, quarters, y, q) {
    years < y | (years == y & quarters <= q)
}

fda_url <- "https://fis.fda.gov"
faers_file_types <- c("ascii", "xml")
faers_file_quarters <- c("q1", "q2", "q3", "q4")
faers_ascii_file_fields <- c("demo", "drug", "indi", "ther", "reac", "rpsr", "outc")
