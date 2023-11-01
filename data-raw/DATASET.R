## code to prepare `DATASET` dataset goes here
meta <- faers_meta(force = TRUE)
saveRDS(
    list(data = meta, date = Sys.time()),
    "inst/extdata/faers_meta_data.rds"
)

faers_sample(2004, "q1", dir = "inst/extdata")
faers_sample(2017, "q2", dir = "inst/extdata")
