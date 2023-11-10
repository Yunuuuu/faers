#' Load data attached in faers package
#'
#' @details
#' - `irAEs`: Immune-related adverse events examined in ICI-associated adverse
#'   events
#' @param nm A string of the data name.
#' Available name: `r oxford_comma(paste0('"', data_nms, '"'))`.
#' @return
#' - `irAEs`: A [data.table][data.table::data.table]
#' @references
#' - Chen Chen, Bin Wu, ChenYu Zhang, Ting Xu, Immune-related adverse events
#' associated with immune checkpoint inhibitors: An updated comprehensive
#' disproportionality analysis of the FDA adverse event reporting system,
#' International Immunopharmacology
#' @examples
#' faers_load("irAEs")
#' @export
faers_load <- function(nm) {
    assert_string(nm, empty_ok = FALSE)
    assert_inclusive(nm, data_nms)
    load_data(nm)
}

data_nms <- c("irAEs")

load_data <- function(nm) {
    readRDS(internal_file("extdata", sprintf("%s.rds", nm)))
}
