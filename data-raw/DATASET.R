## code to prepare `DATASET` dataset goes here
meta <- faers_meta()
# options(warn = 2L)
# 39L, 42:43 In eval(jsub, SDenv, parent.frame()) : NAs introduced by coercion
for (i in 46:49) { # seq_along(meta$year)
    faers(meta$year[i], meta$quarter[i], dir = "~/WorkSpace/Data/FAERS")
}

ascii_list <- lapply(seq_len(nrow(meta)), function(i) {
    faers(meta$year[i], meta$quarter[i], dir = "~/WorkSpace/Data/FAERS")
})
lapply(faers_ascii_file_fields[2L], function(field) {
    lapply(ascii_list, function(x) {
        names(faers_get(x, field))
    })
})
meta[43]
ascii_data <- faers(meta$year[1:3], meta$quarter[1:3],
    dir = "~/WorkSpace/Data/FAERS"
)
system.time(fs1 <- faers_standardize(
    ascii_data,
    "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English",
    add_smq = FALSE
))
system.time(fs2 <- faers_standardize(
    ascii_data,
    "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English",
    add_smq = TRUE
))
faers_get(fs2, "indi")
ascii_data
faers_get(ascii_data, "indi")
ascii_data <- faers_dedup(ascii_data)
ascii_data
ascii_data@dedup

cui <- faers_drug_normalize(
    unique(faers_get(ascii_data, "drug")$drugname),
    force = TRUE
)
cui[is.na(concept_id)]
x <- rxnorm_get_drugs("asdf")
rxnorm_map_to_rxcui(
    unique(faers_get(ascii_data, "drug")$drugname)[1:2000],
    pool = 5L
)
faers(meta$year[70], meta$quarter[70], dir = tempdir())




# test standardize --------------------------
ascii_data <- qs::qread(
    "~/WorkSpace/DataScience/06_faers_sex/munging/ascii_data.qs"
)
ascii_data@data$reac
ascii_data@data$reac[is.na(meddra_code)]
x@data$indi[is.na(meddra_code)]




ascii_data <- faers(meta$year[38], meta$quarter[38],
    dir = tempdir(), format = "xml"
)

# meddra test ------------------------------
meddra_data <- load_meddra("~/WorkSpace/Data/MedDRA/MedDRA_26_1_English")
load_meddra(
    "~/WorkSpace/Data/MedDRA/MedDRA_26_1_English",
    c("llt", "mdhier")
)
meddra_data$hlt_pt[anyDuplicated(pt_code)]
meddra_data$hlgt_hlt
meddra_data$soc_hlgt
meddra_data$soc
data <- meddra_hierarchy_data("~/WorkSpace/Data/MedDRA/MedDRA_26_1_English")
load_meddra("~/WorkSpace/Data/MedDRA/MedDRA_26_1_English", "smq_list")$smq_list[, names(.SD)]
names(meddra_data)
meddra_data$intl_ord
meddra_data$smq_content
meddra_data$smq_content
names(meddra_data$smq_list)
meddra_data$mdhier
meddra_data$mdhier
meddra_hierarchy_data()

meddra_data$llt$llt_code %in% meddra_data$smq_content$term_code
meddra_hierarchy_data("~/WorkSpace/Data/MedDRA/MedDRA_26_1_English")
meddra_data$mdhier[, unique(primary_soc_fg)]
