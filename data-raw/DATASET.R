## code to prepare `DATASET` dataset goes here
meta <- faers_meta()
ascii_data <- faers(meta$year, meta$quarter, dir = "~/WorkSpace/Data/FAERS")
demo <- faers_field(ascii_data, "demo")
