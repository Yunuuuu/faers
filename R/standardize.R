#' Standardize FAERS Quarterly Data for Preferred Term and drug names
#' @param object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods.
#' @return A [FAERSascii] object.
#' @export
#' @name faers_standardize
methods::setGeneric("faers_standardize", function(object, ...) {
    methods::makeStandardGeneric("faers_standardize")
})

#' @param meddra_path A string, define the path of MedDRA directory.
#' @export
#' @method faers_standardize FAERSascii
#' @rdname faers_standardize
methods::setMethod("faers_standardize", "FAERSascii", function(object, meddra_path) {
    # standardize PT terms
    # for indi
    assert_string(meddra_path)
    meddra_data <- meddra_hierarchy_data(meddra_path)
    object@data$indi[, cleaned_pt := str_trim(toupper(indi_pt))]
    object@data$indi <- cbind(
        object@data$indi,
        faers_standardize_pt(object@data$indi$cleaned_pt, meddra_data)
    )
    object@data$indi[
        is.na(meddra_code),
        meddra_code := data.table::fcase(
            cleaned_pt == "ACUTE  LYMPHOCYTIC LEUKAEMIA", "10000846",
            cleaned_pt == "ACUTE ISCHEMIC STROKE", "10061256",
            cleaned_pt == "ALZHEIMER'S  DISEASE", "10001896",
            cleaned_pt == "ARTRIAL FIBRILLATION", "10003658",
            cleaned_pt == "BACTERIAL PNEUMONIA", "10060946",
            cleaned_pt == "CONVULSION  PROPHYLAXIS", "10049885",
            cleaned_pt == "CORONARY STENT INSERTION", "10052086",
            cleaned_pt == "DRUG INDUCED LIVER INJURY", "10072268",
            cleaned_pt == "DRUG USE FOR  UNKNOWN INDICATION", "10057097",
            cleaned_pt == "DRUG USE FOR UNKNOWN  INDICATION", "10057097",
            cleaned_pt == "ESCHERICHIA  INFECTION", "10061126",
            cleaned_pt == "EXUDATIVE SENILE MACULAR DEGENERATIVE OF RETINA", "10015902",
            cleaned_pt == "HEPATITIS  PROPHYLAXIS", "10019717",
            cleaned_pt == "HEPATITIS B  IMMUNIZATION", "10054130",
            cleaned_pt == "HER-2 POSITIVE GASTRIC CANCER", "10065430",
            cleaned_pt == "HUMAN HERPESVIRUS  6  INFECTION", "10020431",
            cleaned_pt == "HYPOTHYROID", "10021114",
            cleaned_pt == "MALIGNANT NEOPLASM OF  PROSTATE", "10026389",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS INFECTION", "10027508",
            cleaned_pt == "NECROTIZING PNEUMONIA", "10049271",
            cleaned_pt == "OSTEOPORISIS", "10031282",
            cleaned_pt == "PERIPHERAL ARTERIAL  DISEASE", "10067825",
            cleaned_pt == "REACTIVE AIRWAY DISEASE", "10037993",
            cleaned_pt == "SCHIZOAFFECTIVE  DISORDER", "10039621",
            cleaned_pt == "SKIN AND SOFT TISSUE INFECTION", "10081417",
            cleaned_pt == "SMOKING CESSATION", "10008374",
            cleaned_pt == "SPINAL LUMBAR DISORDER", "10061368",
            cleaned_pt == "SUPERIOR VENA CAVAL OCCLUSION", "10042568",
            cleaned_pt == "THYROID HORMONE REPLACEMENT", "10068076",
            cleaned_pt == "UPPER RESPIRATORY TRACT  INFECTION", "10046306",
            cleaned_pt == "URINARY TRACT  INFECTION", "10046571",
            cleaned_pt == "ACID REFLUX", "10017885",
            cleaned_pt == "ACUTE  MYELOID  LEUKAEMIA", "10000880",
            cleaned_pt == "AEROMONA INFECTION", "10054205",
            cleaned_pt == "AUTIOIMMUNE INDUCED RASH", "",
            cleaned_pt == "B-CELL  LYMPHOMA", "10003899",
            cleaned_pt == "CARDIC DISORDER", "10061024",
            cleaned_pt == "CHOLESTEROL  HIGH", "10008661",
            cleaned_pt == "CHRONIC HEPATITIS  B", "10008910",
            cleaned_pt == "CHRONIC NERVE PAIN", "10029181",
            cleaned_pt == "COMPLEX PARTIAL EPILEPSY", "10010145",
            cleaned_pt == "CONVULSION  DISORDER", "10010907",
            cleaned_pt == "DEPRESIION", "10012378",
            cleaned_pt == "DRUG KNOWN FOR UNKNOWN INDICATION", "10057097",
            cleaned_pt == "DRUG USE UNKNOWN INDICATION", "10057097",
            cleaned_pt == "HEAVY BLEEDING", "10005103",
            cleaned_pt == "HEPATOBILLIARY DISORDER PROPHYLAXIS", "10081385",
            cleaned_pt == "HER-2 POSITIVE BREAST CANCER", "10065430",
            cleaned_pt == "HERPES  ZOSTER", "10019974",
            cleaned_pt == "HYPERTENSION  ARTERIAL", "10020775",
            cleaned_pt == "INFECTIVE ENDOCARDITIS", "10014678",
            cleaned_pt == "LACTATION  INHIBITION THERAPY", "10069058",
            cleaned_pt == "LENNOX--GASTAUT SYNDROME", "10048816",
            cleaned_pt == "LUMBAR SPONDYLOSIS WITH SCOLIOSIS AND ARTHRITIS", "10025007",
            cleaned_pt == "PRODUCT USED FOR UNKNOWN  INDICATION", "10070592",
            cleaned_pt == "SCHIZOPRENIA", "10039626",
            cleaned_pt == "SIEZURE", "10039906",
            cleaned_pt == "STAGE IV RECTAL ADENOCARCINOMA", "10038029",
            cleaned_pt == "SYSTEMIC INFLAMMATORY  RESPONSE SYNDROME", "10051379",
            cleaned_pt == "THYROID CONDITION", "10043710",
            cleaned_pt == "TYPE  IIB  HYPERLIPIDAEMIA", "10045263",
            cleaned_pt == "WET  MACULAR DEGENERATION", "10067791",
            cleaned_pt == "DRUG USE FO RUNKNOWN INDICATION", "10057097",
            cleaned_pt == "ACQUIRED IMMUNODEFICIENCY  SYNDROME", "10000565",
            cleaned_pt == "ACUTE  LYMPHOBLASTIC LEUKEMIA", "10000845",
            cleaned_pt == "ATRIAL FILBRILLATION", "10003658",
            cleaned_pt == "BLODD PRESSURE", "10005727",
            cleaned_pt == "CARDIAC  ABLATION", "10059864",
            cleaned_pt == "DEPRESSION NEC", "10012378",
            cleaned_pt == "DISBACTERIOSIS", "10064389",
            cleaned_pt == "DRUG USED FOR UNKNOWN INDICATION", "10057097",
            cleaned_pt == "FOLLICLE-STIMULATING HORMONE DEFICIENCY", "10071084",
            cleaned_pt == "GASTRO-JEJUNOSTOMY", "10017882",
            cleaned_pt == "HER-2 PROTEIN OVEREXPRESSION", "10075638",
            cleaned_pt == "INHALATION", "10061218",
            cleaned_pt == "INTERVERTEBRAL DISKITIS", "10060738",
            cleaned_pt == "IRREGULAR HEARTBEAT", "10019323",
            cleaned_pt == "LOW  BACK PAIN", "10024891",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS SEPSIS", "10058867",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS TEST", "10053429",
            cleaned_pt == "PANIC DISORDER/ DEPRESSION", "10033666",
            cleaned_pt == "PRODUCT  USED FOR UNKNOWN INDICATION", "10070592",
            cleaned_pt == "PULMONARY ARTERY HYPERTENSION", "10064911",
            cleaned_pt == "SKIN  ABRASION", "10064990",
            cleaned_pt == "SQUAMOUS CELL  CARCINOMA OF THE CERVIX", "10041848",
            cleaned_pt == "STAGE IV NON-SMALL CELL", "10029522",
            cleaned_pt == "STREPTOCOCCAL IDENTIFICATION TEST", "10067006",
            cleaned_pt == "TOTAL KNEE ARTHROPLASTY", "10003398",
            cleaned_pt == "TUBERCULOUS  MENINGITIS", "10045080",
            cleaned_pt == "B LYMPHOBLASTIC LEUKEMIA", "10054448",
            cleaned_pt == "BIPOLAR DISORDER II", "10004940",
            cleaned_pt == "CARDIAC CATH", "10007527",
            cleaned_pt == "CHEMOTHERAPY/RECTOSIGMOID CANCER", "10038093",
            cleaned_pt == "COROARY ARTERY STENT PLACEMENT", "10052086",
            cleaned_pt == "CORONARY ARTERY DISEASE/HYPERTENSION", "10020772",
            cleaned_pt == "CROHNS DISEASE", "10011401",
            cleaned_pt == "DEPRESSON", "10012378",
            cleaned_pt == "DRUG USE  FOR UNKNOWN INDICATION", "10057097",
            cleaned_pt == "DRUG USE FOR UNAPPROVED  INDICATION", "10053746",
            cleaned_pt == "EVAN'S SYNDROME", "10053873",
            cleaned_pt == "GENERAL ANXIETY DISORDER", "10018075",
            cleaned_pt == "H1NI INFLUENZA", "10069767",
            cleaned_pt == "HEADACHE/PROPHYLAXIS", "10019211",
            cleaned_pt == "HEADACHES", "10019211",
            cleaned_pt == "HELICOBACTER PYLORI PROPHYLAXIS", "10054263",
            cleaned_pt == "HUNTINGTONS DISEASE", "10070668",
            cleaned_pt == "HYPOTHALAMO-PITUITARY DISORDERS", "10021111",
            cleaned_pt == "INFECTION  MRSA", "10021839",
            cleaned_pt == "INR INCREAESD", "10022402",
            cleaned_pt == "METASTATIC CHOLANGIOCARCINOMA", "10077846",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS TEST POSITIVE", "10053427",
            cleaned_pt == "OSEOPOROSIS", "10031282",
            cleaned_pt == "OSTEOMYLITIS", "10031252",
            cleaned_pt == "PRODUCT USED FOR UNKNOWN INDCATION", "10070592",
            cleaned_pt == "RHEUMATOID  ARTHRITIS", "10039073",
            cleaned_pt == "TYPE II DIABETES", "10045242",

            # self added
            cleaned_pt == "TRAVELLER'S DIARRHEA", "10044552",
            # cleaned_pt == "INFLAMATION", "10061218",
            cleaned_pt == "PTSD", "10036316",
            cleaned_pt == "PAH", "10064911",
            cleaned_pt == "GBM", "10018336"
        )
    ]
    object@data$indi[, cleaned_pt := NULL]

    # for reac
    object@data$reac[, cleaned_pt := str_trim(toupper(pt))]
    object@data$reac <- cbind(
        object@data$reac,
        faers_standardize_pt(object@data$reac$cleaned_pt, meddra_data)
    )
    object@data$reac[
        is.na(meddra_code),
        meddra_code := data.table::fcase(
            cleaned_pt == "ANO-RECTAL STENOSIS", "10002581",
            cleaned_pt == "HER-2 POSITIVE BREAST CANCER", "10065430",
            cleaned_pt == "STAPHYLOCOCCAL IDENTIFICATION TEST POSITIVE", "10067140",
            cleaned_pt == "STREPTOCOCCAL IDENTIFICATION TEST", "10067006",
            cleaned_pt == "STREPTOCOCCAL SEROLOGY", "10059987",
            cleaned_pt == "ANO-RECTAL ULCER", "10002582",
            cleaned_pt == "BLASTIC PLASMACYTOID DENDRITRIC CELL NEOPLASIA", "10075460",
            cleaned_pt == "CORNELIA DE-LANGE SYNDROME", "10077707",
            cleaned_pt == "FRONTAL SINUS OPERATIONS", "10017379",
            cleaned_pt == "HER-2 POSITIVE GASTRIC CANCER", "10066896",
            cleaned_pt == "MAXILLARY ANTRUM OPERATIONS", "10026950",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS TEST NEGATIVE", "10053428",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS TEST POSITIVE", "10053427",
            cleaned_pt == "PAROVARIAN CYST", "10052456",
            cleaned_pt == "STAPHYLOCOCCAL IDENTIFICATION TEST NEGATIVE", "10067005",
            cleaned_pt == "STREPTOCOCCAL IDENTIFICATION TEST POSITIVE", "10067004",
            cleaned_pt == "AEROMONA INFECTION", "10054205",
            cleaned_pt == "DISBACTERIOSIS", "10064389",
            cleaned_pt == "GASTRO-ENTEROSTOMY", "10017873",
            cleaned_pt == "HER-2 PROTEIN OVEREXPRESSION", "10075638",
            cleaned_pt == "HYPOTHALAMO-PITUITARY DISORDERS", "10021111",
            cleaned_pt == "SUPERIOR VENA CAVAL OCCLUSION", "10058988",
            cleaned_pt == "AEROMONA INFECTION", "10054205",
            cleaned_pt == "CAPNOCYTOPHAGIA INFECTION", "10061738",
            cleaned_pt == "DISBACTERIOSIS", "10064389",
            cleaned_pt == "EAGLES SYNDROME", "10066835",
            cleaned_pt == "EVAN'S SYNDROME", "10053873",
            cleaned_pt == "GASTRO-INTESTINAL FISTULA", "10071258",
            cleaned_pt == "GLYCOPEPTIDE ANTIBIOTIC RESISTANT STAPHYLOCOCCAL AUREUS INFECTION", "10052101",
            cleaned_pt == "HEPATOBILLIARY DISORDER PROPHYLAXIS", "10081385",
            cleaned_pt == "HER-2 POSITIVE BREAST CANCER", "10065430",
            cleaned_pt == "MENINGEOMAS SURGERY", "10053765",
            cleaned_pt == "METHICILLIN-RESISTANT STAPHYLOCOCCAL AUREUS TEST", "10053429",
            cleaned_pt == "SPHENOID SINUS OPERATIONS", "10041508",
            cleaned_pt == "STREPTOCOCCAL SEROLOGY POSITIVE", "10059988",
            cleaned_pt == "SUPERIOR VENA CAVAL STENOSIS", "10064771",
            cleaned_pt == "SUPERIOR VENA CAVAL STENOSIS", "10064771",
            cleaned_pt == "GASTRO-JEJUNOSTOMY", "10017882",

            # self-added
            cleaned_pt == "IMMUNE-MEDIATED ADRENAL INSUFICIENCY", "10085547",
            cleaned_pt == "ORAL APPLIANCE", "10085270",
            cleaned_pt == "VAGINAL RING", "10082353",
            cleaned_pt == "VAGINAL CUFF", "10088846",
            cleaned_pt == "MOREL-LAVELLEE SEROMA", "10088873"
        )
    ]
    object@data$reac[, cleaned_pt := NULL]
    object
})

faers_standardize_pt <- function(terms, meddra_data) {
    out_code <- rep_len(NA_integer_, length(terms))
    idx <- rep_len(NA, length(terms))
    for (i in c("llt", "pt")) {
        operated_idx <- is.na(out_code)
        mapped_idx <- data.table::chmatch(
            terms[operated_idx],
            toupper(meddra_data[[paste(i, "name", sep = "_")]])
        )
        out_code[operated_idx] <- meddra_data[[
            paste(i, "code", sep = "_")
        ]][mapped_idx]
        idx[operated_idx] <- mapped_idx
    }
    out <- meddra_data[idx]
    out[, meddra_code := as.character(out_code)]
}

faers_standardize_drug <- function(terms, athena = NULL, force = FALSE, exact = TRUE, approximate = TRUE, search = 2L) {
    standardize_drug_by_athena(terms = terms, path = athena, force = force)
}

standardize_drug_by_rxnorm <- function(terms, exact = TRUE, approximate = TRUE, search = 2, pool = 5L) {
    assert_bool(exact)
    assert_bool(approximate)
    rxnorm_map_to_rxcui(terms,
        exact = exact, approximate = approximate,
        search = search, pool = pool
    )
    # get other drug details from rxnorm
}

rxnorm_map_to_rxcui <- function(terms, exact = TRUE, approximate = TRUE, allsrc = NULL, srclist = NULL, search = NULL, pool = 5L) {
    if (exact) {
        cli::cli_alert("Running Exact Match")
        out <- rxnorm_findRxcuiByString(terms,
            allsrc = allsrc,
            srclist = srclist, search = search,
            pool = pool
        )
    } else {
        out <- rep_len(NA_character_, length(terms))
    }
    if (anyNA(out) && approximate) {
        cli::cli_alert("Running Approximate Match")
        out2 <- rxnorm_getApproximateMatch(terms[is.na(out)],
            max_entries = 1L, pool = pool
        )
        out[is.na(out)] <- vapply(out2, function(x) {
            if (is.null(x)) {
                NA_character_
            } else {
                x$rxcui[[1L]] %||% NA_character_
            }
        }, character(1L))
    }
    out
}

standardize_drug_by_athena <- function(terms, path = NULL, force = FALSE) {
    data <- athena_parse(
        c("concept", "concept_synonym"),
        path = path, force = force
    )
    data$concept <- data$concept[domain_id == "Drug"] # nolint
    data$concept_synonym <- data$concept_synonym[
        concept_id %in% data$concept$concept_id # nolint
    ]
    mapped_concept_ids <- c(
        data$concept$concept_id,
        data$concept_synonym$concept_id
    )
    ..__mapped_concept_ids__.. <- mapped_concept_ids[data.table::chmatch(
        str_trim(tolower(terms)), str_trim(tolower(c(
            data$concept$concept_name,
            data$concept_synonym$concept_synonym_name
        )))
    )]
    out <- data$concept[match(..__mapped_concept_ids__.., concept_id)] # nolint
    out[, athena_drug_names := terms] # nolint
    data.table::setcolorder(out, "athena_drug_names", before = 1L)
}

utils::globalVariables(c(
    "domain_id", "concept_id", "athena_drug_names", "cleaned_pt", "meddra_code"
))
