#' Check if FAERS year
#'
#' This function check if data for the year and quarter selected are
#' available at FAERS to be downloaded.
#'
#' @param years An atomic integer indicates years for which data are required.
#' @param quarters An atomic character, only "q1", "q2", "q3", and "q4" are
#' allowed.
#'
#' @return A logical indicates FAERS can have data for the `years` and
#' `quarters` required?
#' @examples
#' faers_available(c(2011, 2023), c("q1", "q2"))
#' @export
faers_available <- function(years, quarters) {
    assert_inclusive(quarters, available_quarter)
    metadata <- faers_meta()
    years %in% metadata$year & quarters %in% metadata$quarter
}
available_quarter <- c("q1", "q2", "q3", "q4")
