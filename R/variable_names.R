message("✓ Loaded custom get_cdisc_labels() from variable_namess.R")

library("openxlsx")

#' Load CDISC Labels for Specified Dataset
#'
#' Reads CDISC variable labels from the TB-PACTS Data Dictionary for a specified dataset.
#'
#' @param dataset_code Character string. Lowercase dataset abbreviation (e.g., "dm", "ae", "lb")
#' @param metadata_folder Character string. Path to folder containing the data dictionary
#' @param dict_file Character string. Name of the data dictionary Excel file.
#'   Default is "2022-04-14 TB-PACTS_Data_Dictionary.xlsx"
#' @param sheet Character string. Name of the sheet containing variable descriptions.
#'   Default is "Variable Descriptions"
#' @param cols Numeric vector. Columns to read. Default is 1:3 (Variable, Label, Type)
#'
#' @return A data frame with CDISC variable labels, or NULL if dataset code not found
#'
#' @note Available dataset codes include: dm, ae, be, bs, ce, cm, co, da, dd, di, ds, dv,
#'   ec, eg, er, ex, fa, ho, ie, lb, mb, mh, mi, mo, ms, oe, pc, pe, pf, pp, pr, qs, re,
#'   rp, sc, se, sr, ss, su, sv, vs, xo, relrec, ta, td, te, ti, ts, tv, apdm, apmh
#'
#' @export
get_cdisc_labels <- function(dataset_code, 
                             metadata_folder, 
                             dict_file = "2022-04-14 TB-PACTS_Data_Dictionary.xlsx",
                             sheet = "Variable Descriptions",
                             cols = 1:3) {
  
  # Convert to lowercase for consistency
  dataset_code <- tolower(dataset_code)
  
  # Define row ranges for each dataset
  dataset_rows <- list(
    # Subject Level Datasets
    dm = 3:38,
    ae = 40:163,
    be = 165:186,
    bs = 188:210,
    ce = 212:250,
    cm = 252:324,
    co = 326:338,
    da = 340:369,
    dd = 371:390,
    di = 392:400,
    ds = 402:432,
    dv = 434:446,
    ec = 448:470,
    eg = 472:508,
    er = 510:522,
    ex = 524:580,
    fa = 582:606,
    ho = 608:617,
    ie = 619:633,
    lb = 635:689,
    mb = 691:736,
    mh = 738:776,
    mi = 778:791,
    mo = 793:803,
    ms = 805:845,
    oe = 847:879,
    pc = 881:918,
    pe = 920:951,
    pf = 953:996,
    pp = 998:1017,
    pr = 1019:1037,
    qs = 1039:1066,
    re = 1068:1101,
    rp = 1103:1137,
    sc = 1139:1163,
    se = 1165:1177,
    sr = 1179:1199,
    ss = 1201:1219,
    su = 1221:1255,
    sv = 1257:1281,
    vs = 1283:1315,
    xo = 1317:1335,
    # Trial Level Datasets
    relrec = 1338:1345,
    ta = 1347:1357,
    td = 1359:1372,
    te = 1374:1381,
    ti = 1383:1391,
    ts = 1393:1407,
    tv = 1409:1418,
    # Special Purpose Datasets
    apdm = 1421:1426,
    apmh = 1428:1446
  )
  
  # Check if dataset code exists
  if (!dataset_code %in% names(dataset_rows)) {
    stop(paste0("Dataset code '", dataset_code, "' not found. ",
                "Available codes: ", paste(names(dataset_rows), collapse = ", ")))
  }
  
  # Get the appropriate row range
  rows <- dataset_rows[[dataset_code]]
  
  # Read the labels
  labels <- read.xlsx(
    file.path(metadata_folder, dict_file),
    sheet = sheet,
    rows = rows,
    cols = cols
  )
  
  # Optional: convert column names to lowercase
  #names(labels) <- tolower(names(labels))
  
  return(labels)
}




# # Demographics Dataset (DM)
# rows = 3:38
# 
# # Adverse Events Dataset (AE)  
# rows = 40:163
# 
# # Biospecimen Events Dataset (BE)
# rows = 165:186
# 
# # Biospecimens Findings Dataset (BS)
# rows = 188:210
# 
# # Clinical Events Dataset (CE)
# rows = 212:250
# 
# # Concomitant Medications Dataset (CM)
# rows = 252:324
# 
# # Comments Dataset (CO)
# rows = 326:338
# 
# # Drug Accountability Dataset (DA)
# rows = 340:369
# 
# # Death Details Dataset (DD)
# rows = 371:390
# 
# # Device-in-Use Dataset (DI)
# rows = 392:400
# 
# # Disposition Dataset (DS)
# rows = 402:432
# 
# # Protocol Deviations Dataset (DV)
# rows = 434:446
# 
# # Exposure as Collected Dataset (EC)
# rows = 448:470
# 
# # ECG Test Results Dataset (EG)
# rows = 472:508
# 
# # Environmental Risk Factors Dataset (ER)
# rows = 510:522
# 
# # Exposure Dataset (EX)
# rows = 524:580
# 
# # Findings About Dataset (FA)
# rows = 582:606
# 
# # Hospitalisation Dataset (HO)
# rows = 608:617
# 
# # Inclusion/Exclusion Dataset (IE)
# rows = 619:633
# 
# # Laboratory Test Results Dataset (LB)
# rows = 635:689
# 
# # Microbiology Specimen Dataset (MB)
# rows = 691:736
# 
# # Medical History Dataset (MH)
# rows = 738:776
# 
# # Microbiology Susceptibility Dataset (MI)
# rows = 778:791
# 
# # Morphology Dataset (MO)
# rows = 793:803
# 
# # Microscopic Findings Dataset (MS)
# rows = 805:845
# 
# # Optical Examination Dataset (OE)
# rows = 847:879
# 
# # Pharmacokinetic Concentration Dataset (PC)
# rows = 881:918
# 
# # Physical Examination Dataset (PE)
# rows = 920:951
# 
# # Pharmacogenomics Findings Dataset (PF)
# rows = 953:996
# 
# # Pharmacokinetic Parameters Dataset (PP)
# rows = 998:1017
# 
# # Procedures Dataset (PR)
# rows = 1019:1037
# 
# # Questionnaires Dataset (QS)
# rows = 1039:1066
# 
# # Respiratory Findings Dataset (RE)
# rows = 1068:1101
# 
# # Reproductive Findings Dataset (RP)
# rows = 1103:1137
# 
# # Subject Characteristics Dataset (SC)
# rows = 1139:1163
# 
# # Subject Elements Dataset (SE)
# rows = 1165:1177
# 
# # Skin Response Dataset (SR)
# rows = 1179:1199
# 
# # Subject Status Dataset (SS)
# rows = 1201:1219
# 
# # Substance Use Dataset (SU)
# rows = 1221:1255
# 
# # Subject Visits Dataset (SV)
# rows = 1257:1281
# 
# # Vital Signs Dataset (VS)
# rows = 1283:1315
# 
# # Custom Outcomes Dataset (XO)
# rows = 1317:1335
# TRIAL LEVEL DATASETS:
#   r# Related Records Dataset (RELREC)
# rows = 1338:1345
# 
# # Trial Arms Dataset (TA)
# rows = 1347:1357
# 
# # Trial Dosing Dataset (TD)
# rows = 1359:1372
# 
# # Trial Elements Dataset (TE)
# rows = 1374:1381
# 
# # Trial Inclusion/Exclusion Criteria Dataset (TI)
# rows = 1383:1391
# 
# # Trial Summary Dataset (TS)
# rows = 1393:1407
# 
# # Trial Visits Dataset (TV)
# rows = 1409:1418
# SPECIAL PURPOSE DATASETS:
#   r# Associated Persons Demographics Dataset (APDM)
# rows = 1421:1426
# 
# # Associated Persons Medical History Dataset (APMH)
# rows = 1428:1446