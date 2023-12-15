## code to prepare `DATASET` dataset goes here
meta <- faers_meta(force = TRUE)
saveRDS(
    list(data = meta, date = Sys.time()),
    "inst/extdata/faers_meta_data.rds",
    compress = "gzip"
)

faers_sample(2004, "q1", dir = "inst/extdata")
faers_sample(2017, "q2", dir = "inst/extdata")
data <- faers(c(2004, 2017), c("q1", "q2"),
  dir = system.file("extdata", package = "faers"),
  compress_dir = tempdir()
)
data <- faers_standardize(data, "~/Data/MedDRA/MedDRA_26_1_English") # nolint
saveRDS(data, "inst/extdata/standardized_data.rds")

# - Chen Chen, Bin Wu, ChenYu Zhang, Ting Xu,Immune-related adverse events
# associated with immune checkpoint inhibitors: An updated comprehensive
# disproportionality analysis of the FDA adverse event reporting system,
# International Immunopharmacology,
doc <- docxtractr::read_docx("data-raw/irAEs.docx")
irAEs <- docxtractr::docx_extract_tbl(doc, tbl_number = 2L)
irAEs <- tidyr::fill(
    dplyr::mutate(irAEs, Toxicity.types = dplyr::na_if(Toxicity.types, "")),
    Toxicity.types
)
data.table::setDT(irAEs)
irAEs <- unique(irAEs)
saveRDS(irAEs, "inst/extdata/irAEs.rds", compress = "bzip2")
