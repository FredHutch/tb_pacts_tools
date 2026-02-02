# =============================================================================
# EFFICACY ANALYSIS DATASET PROCESSING TEMPLATE
# =============================================================================
#
# PURPOSE:
#   Process raw CDISC SDTM data to create a standardized efficacy analysis 
#   dataset including:
#     - Trial population definitions (randomized, ITT, safety, PP)
#     - Time to culture conversion (TTCC) endpoints
#     - Time to positivity (TTP) measures
#
# OUTPUT:
#   - eff_adata_[STUDYID]_[DATE].csv: Subject-level efficacy analysis dataset
#   - eff_adata_[STUDYID]_[DATE]_dictionary.csv: Column documentation
#
# USAGE:
#   1. Update the STUDY CONFIGURATION section below
#   2. Run the entire script
#   3. Use output with eff_analysis_report_template.qmd for visualization
#
# REQUIRED INPUT DOMAINS:
#   - dm.csv: Demographics (arm assignment)
#   - ds.csv: Disposition (treatment epoch entry)
#   - da.csv: Drug Accountability (adherence/compliance)
#   - ex.csv: Exposure (treatment dispensing)
#   - mb.csv: Microbiology (culture results, TTP)
#
# AUTHOR:       [Your Name]
# CREATED:      [Date]
# MODIFIED:     
# STUDY ID:     
# =============================================================================

# =============================================================================
# LOAD REQUIRED PACKAGES
# =============================================================================

devtools::load_all("./tb_pacts_tools")

# Core data manipulation packages
library(dplyr)      # Data manipulation
library(tidyr)      # Data tidying
library(magrittr)   # Pipe operators
library(lubridate)  # Date handling
library(stringr)    # String manipulation

# =============================================================================
# STUDY CONFIGURATION - MODIFY THIS SECTION FOR EACH STUDY
# =============================================================================

# ---- STUDY IDENTIFICATION ----
STUDY_IDS <- c("TB-1037")

# ---- FILE PATHS ----
DATA_INPUT_PATH <- tbpacts_csv
DATA_OUTPUT_PATH <- file.path(tbpacts_adata, paste(STUDY_IDS, collapse = "_"))

# ---- TREATMENT ARMS ----
# Set to NULL to include all arms, or specify arm codes to include
TREATMENT_ARMS <- NULL

# ---- ARM LABEL MAPPING (OPTIONAL) ----
# Create shorter labels for arms in plots. Set to NULL to use original labels.
# Format: c("Original Arm Name" = "Short Label", ...)
ARM_LABELS <- c(
 "Drug Resistant TB: BPaMZ daily for 26 weeks (6 months)" = "BPaMZ (DR, 26w)",
 "Drug Sensitive TB: BPaMZ daily for 17 weeks (4 months)" = "BPaMZ (DS, 17w)",
 "Drug Sensitive TB: HRZE/HR combination tablets daily for 26 weeks (6 months)" = "HRZE/HR (DS, 26w)"
)

# ---- EFFICACY ENDPOINT SETTINGS ----
# Culture conversion definition: number of consecutive negative cultures required
N_CONSECUTIVE_NEGATIVES <- 2

# TTP value to assign for negative cultures (days) - typically 42 for MGIT
TTP_NEGATIVE_VALUE <- 42


# =============================================================================
# SETUP - DO NOT MODIFY
# =============================================================================

OUTPUT_FILENAME <- paste0("eff_adata_", 
                          paste(STUDY_IDS, collapse = "_"), 
                          "_", format(Sys.Date(), "%Y%m%d"), 
                          ".csv")

if (!dir.exists(DATA_OUTPUT_PATH)) {
  dir.create(DATA_OUTPUT_PATH, recursive = TRUE)
  message("Created directory: ", DATA_OUTPUT_PATH)
}

message("\n=== Efficacy Analysis Dataset Processing ===")
message("Study ID(s): ", paste(STUDY_IDS, collapse = ", "))
message("Started: ", Sys.time())


# =============================================================================
# LOAD AND FILTER DATA
# =============================================================================

message("\n--- Loading Data ---")

# Required domains
dm <- load_domain("dm", DATA_INPUT_PATH, STUDY_IDS, required = TRUE)
mb <- load_domain("mb", DATA_INPUT_PATH, STUDY_IDS, required = TRUE)

# Optional domains
ds <- load_domain("ds", DATA_INPUT_PATH, STUDY_IDS, required = FALSE)
da <- load_domain("da", DATA_INPUT_PATH, STUDY_IDS, required = FALSE)
ex <- load_domain("ex", DATA_INPUT_PATH, STUDY_IDS, required = FALSE)

# Check critical domains
if (is.null(dm)) stop("DM domain is required but not found")
if (is.null(mb)) stop("MB domain is required for efficacy endpoints")


# =============================================================================
# DEFINE CORE COLUMNS FOR EACH DOMAIN
# =============================================================================

DM_CORE_COLS <- c(
  "usubjid", "studyid", "age", "sex", "race", "ethnic", "country",
  "actarmcd", "actarm", "armcd", "arm"
)

DS_CORE_COLS <- c(
  "usubjid", "domain", "dsseq", "dsterm", "dsdecod", "dscat", "dsscat",
  "epoch", "dsstdy"
)

DA_CORE_COLS <- c(
  "usubjid", "domain", "daseq", "datestcd", "datest", "dacat",
  "daorres", "dastresn", "epoch"
)

EX_CORE_COLS <- c(
  "usubjid", "domain", "exseq", "extrt", "exdose", "exdosu",
  "exstdy", "exendy", "epoch", "exact"
)

MB_CORE_COLS <- c(
  "usubjid", "domain", "mbseq", "spdevid", "mbtestcd", "mbtest", "mbtstdtl",
  "mbspec", "mborres", "mbstresc", "mbstresn", "mbdy", "visitnum", "visit"
)

# Select available columns for each domain
if (!is.null(dm)) {
  dm_cols <- report_missing_cols(DM_CORE_COLS, names(dm), "DM")
  dm <- dm %>% select(all_of(dm_cols))
}
if (!is.null(ds)) {
  ds_cols <- report_missing_cols(DS_CORE_COLS, names(ds), "DS")
  ds <- ds %>% select(all_of(ds_cols))
}
if (!is.null(da)) {
  da_cols <- report_missing_cols(DA_CORE_COLS, names(da), "DA")
  da <- da %>% select(all_of(da_cols))
}
if (!is.null(ex)) {
  ex_cols <- report_missing_cols(EX_CORE_COLS, names(ex), "EX")
  ex <- ex %>% select(all_of(ex_cols))
}
if (!is.null(mb)) {
  mb_cols <- report_missing_cols(MB_CORE_COLS, names(mb), "MB")
  mb <- mb %>% select(all_of(mb_cols))
}


# =============================================================================
# DEFINE TRIAL POPULATIONS FROM DS (Disposition)
# =============================================================================
# Adapted from 02_define_trial_populations.R

message("\n--- Defining Trial Populations ---")

if (!is.null(ds)) {
  populations_ds <- ds %>%
    filter(domain == "DS",
           epoch == "TREATMENT",
           !is.na(dsstdy),
           dsstdy >= 1) %>%
    group_by(usubjid) %>%
    summarise(
      randomized_flag = TRUE,
      itt_flag = TRUE,
      safety_flag = TRUE,
      first_dsstdy = min(dsstdy),
      last_dsstdy = max(dsstdy),
      .groups = "drop"
    )
  message("Subjects from DS: ", nrow(populations_ds))
} else {
  # Fallback: use DM if DS not available
  populations_ds <- dm %>%
    select(usubjid) %>%
    distinct() %>%
    mutate(
      randomized_flag = TRUE,
      itt_flag = TRUE,
      safety_flag = TRUE,
      first_dsstdy = NA_real_,
      last_dsstdy = NA_real_
    )
  message("DS not available, using DM for population: ", nrow(populations_ds), " subjects")
}


# =============================================================================
# DEFINE ADHERENCE METRICS FROM DA (Drug Accountability)
# =============================================================================

if (!is.null(da)) {
  populations_da <- da %>%
    filter(domain == "DA",
           dacat == "STUDY MEDICATION",
           datestcd == "COMPL",
           !is.na(dastresn)) %>%
    mutate(dastresn = pmin(dastresn, 100)) %>%
    group_by(usubjid) %>%
    summarise(
      mean_compl_pct = mean(dastresn),
      min_compl_pct = min(dastresn),
      n_compl_visits = n(),
      pp_adherent = mean_compl_pct >= 80,
      .groups = "drop"
    )
  message("Subjects with adherence data: ", nrow(populations_da))
} else {
  populations_da <- NULL
  message("DA domain not available, skipping adherence metrics")
}


# =============================================================================
# DEFINE EXPOSURE METRICS FROM EX (Exposure)
# =============================================================================

if (!is.null(ex)) {
  populations_ex <- ex %>%
    filter(domain == "EX",
           epoch == "TREATMENT",
           !is.na(exstdy),
           !is.na(exendy),
           !is.na(exdose)) %>%
    group_by(usubjid) %>%
    summarise(
      first_exstdy = min(exstdy),
      last_exendy = max(exendy),
      n_ex_records = n(),
      n_dispensing = sum(exact == "SCHEDULED DISPENSING", na.rm = TRUE),
      .groups = "drop"
    )
  message("Subjects with exposure data: ", nrow(populations_ex))
} else {
  populations_ex <- NULL
  message("EX domain not available, skipping exposure metrics")
}


# =============================================================================
# COMBINE POPULATION FLAGS
# =============================================================================

message("\n--- Combining Population Definitions ---")

trial_populations <- populations_ds

if (!is.null(populations_ex)) {
  trial_populations <- trial_populations %>%
    left_join(populations_ex, by = "usubjid")
}

if (!is.null(populations_da)) {
  trial_populations <- trial_populations %>%
    left_join(populations_da, by = "usubjid")
}

# Define per-protocol flag
# PP = safety population + no evidence of clear non-adherence (min compliance >= 50%)
trial_populations <- trial_populations %>%
  mutate(
    pp_flag = safety_flag & (is.na(min_compl_pct) | min_compl_pct >= 50)
  )

message("Total subjects in population: ", nrow(trial_populations))
message("  Randomized: ", sum(trial_populations$randomized_flag, na.rm = TRUE))
message("  ITT: ", sum(trial_populations$itt_flag, na.rm = TRUE))
message("  Safety: ", sum(trial_populations$safety_flag, na.rm = TRUE))
message("  PP: ", sum(trial_populations$pp_flag, na.rm = TRUE))


# =============================================================================
# PREPARE MGIT CULTURE RESULTS FROM MB (Microbiology)
# =============================================================================
# Adapted from create_efficacy_measures.R

message("\n--- Processing Microbiology Data ---")

# Filter to MGIT MTB culture results
mgit_culture <- mb %>%
  filter(domain == "MB",
         spdevid == "MGIT",
         mbtestcd == "MTB",
         mbtstdtl == "Culture Growth",
         mbspec == "SPUTUM",
         mbstresc %in% c("POSITIVE", "NEGATIVE"),
         !is.na(mbdy)) %>%
  transmute(
    usubjid,
    mbdy,
    visitnum,
    visit,
    result = mbstresc
  )

message("MGIT culture records: ", nrow(mgit_culture))

# Extract time-to-positivity (TTP)
mgit_ttp <- mb %>%
  filter(domain == "MB",
         spdevid == "MGIT",
         mbtestcd == "MTB",
         mbtstdtl == "Time to Detection",
         !is.na(mbdy),
         !is.na(mbstresc)) %>%
  mutate(ttp = as.numeric(mbstresc)) %>%
  filter(!is.na(ttp)) %>%
  group_by(usubjid, mbdy) %>%
  summarise(ttp_mean = mean(ttp), .groups = "drop")

message("TTP records: ", nrow(mgit_ttp))


# =============================================================================
# ARBITRATE CULTURE RESULTS (multiple specimens per day)
# =============================================================================

arbitrate_ttcc <- function(df) {
  df %>%
    group_by(usubjid, mbdy) %>%
    summarise(
      final_result = case_when(
        any(result == "POSITIVE") ~ "Positive",
        all(result == "NEGATIVE") ~ "Negative",
        TRUE ~ "Unknown"
      ),
      .groups = "drop"
    )
}

ttcc_results <- arbitrate_ttcc(mgit_culture)
message("Arbitrated culture results: ", nrow(ttcc_results), " subject-days")


# =============================================================================
# JOIN TTP AND APPLY NEGATIVE CODING
# =============================================================================

ttcc_results <- ttcc_results %>%
  left_join(mgit_ttp, by = c("usubjid", "mbdy")) %>%
  mutate(
    ttp = case_when(
      final_result == "Negative" ~ TTP_NEGATIVE_VALUE,
      final_result == "Positive" ~ ttp_mean,
      TRUE ~ NA_real_
    )
  ) %>%
  select(usubjid, mbdy, final_result, ttp)


# =============================================================================
# RESTRICT TO ANALYSIS POPULATION
# =============================================================================

ttcc_results <- trial_populations %>%
  transmute(usubjid = usubjid, randomized_flag) %>%
  filter(randomized_flag) %>%
  left_join(ttcc_results, by = "usubjid")

message("Culture results for analysis population: ", nrow(ttcc_results))


# =============================================================================
# DERIVE TIME TO CULTURE CONVERSION (TTCC)
# =============================================================================
# Definition: First of N consecutive negative cultures

message("\n--- Calculating Time to Culture Conversion ---")

# Find first occurrence of N consecutive negatives
bs_ttcc <- ttcc_results %>%
  group_by(usubjid) %>%
  arrange(usubjid, mbdy) %>%
  mutate(
    next_result = lead(final_result),
    next_day = lead(mbdy)
  ) %>%
  filter(final_result == "Negative" & next_result == "Negative") %>%
  summarise(
    t = min(mbdy, na.rm = TRUE),
    event = 1L,  # 1 = converted
    .groups = "drop"
  )

# Add censored subjects (no conversion observed)
bs_ttcc <- ttcc_results %>%
  group_by(usubjid) %>%
  summarise(last_day = max(mbdy, na.rm = TRUE), .groups = "drop") %>%
  left_join(bs_ttcc, by = "usubjid") %>%
  mutate(
    event = if_else(!is.na(t), event, 0L),  # 0 = censored
    t = if_else(!is.na(t), t, last_day)
  ) %>%
  select(usubjid, t, event)

message("TTCC derived for ", nrow(bs_ttcc), " subjects")
message("  Converted: ", sum(bs_ttcc$event == 1, na.rm = TRUE))
message("  Censored: ", sum(bs_ttcc$event == 0, na.rm = TRUE))


# =============================================================================
# GET ARM ASSIGNMENT FROM DM
# =============================================================================

message("\n--- Processing Treatment Arms ---")

arm_df <- dm %>%
  transmute(
    usubjid,
    arm = if ("arm" %in% names(.)) arm else actarm,
    armcd = if ("armcd" %in% names(.)) armcd else actarmcd
  ) %>%
  distinct()

# Apply arm filter if specified
if (!is.null(TREATMENT_ARMS)) {
  arm_df <- arm_df %>% filter(armcd %in% TREATMENT_ARMS)
  message("Filtered to arms: ", paste(TREATMENT_ARMS, collapse = ", "))
}

message("Subjects with arm assignment: ", nrow(arm_df))
message("Arms: ", paste(unique(arm_df$arm), collapse = ", "))


# =============================================================================
# BUILD FINAL EFFICACY ANALYSIS DATASET
# =============================================================================

message("\n--- Building Final Analysis Dataset ---")

eff_analysis <- bs_ttcc %>%
  rename(subject_id = usubjid, ttcc_time = t, ttcc_event = event) %>%
  left_join(
    trial_populations %>% rename(subject_id = usubjid),
    by = "subject_id"
  ) %>%
  left_join(
    arm_df %>% rename(subject_id = usubjid),
    by = "subject_id"
  )

# Apply arm labels if provided
if (!is.null(ARM_LABELS)) {
  eff_analysis <- eff_analysis %>%
    mutate(arm_label = recode(arm, !!!ARM_LABELS))
} else {
  eff_analysis <- eff_analysis %>%
    mutate(arm_label = arm)
}

# Add study ID
eff_analysis <- eff_analysis %>%
  mutate(studyid = paste(STUDY_IDS, collapse = "_"))

# Reorder columns
eff_analysis <- eff_analysis %>%
  select(
    studyid, subject_id, arm, armcd, arm_label,
    randomized_flag, itt_flag, safety_flag, pp_flag,
    ttcc_time, ttcc_event,
    starts_with("first_"), starts_with("last_"), starts_with("n_"),
    starts_with("mean_"), starts_with("min_"), contains("compl"), contains("adherent"),
    everything()
  )


# =============================================================================
# DATA QUALITY SUMMARY
# =============================================================================

message("\n--- Data Quality Summary ---")

n_subjects <- n_distinct(eff_analysis$subject_id)
n_arms <- n_distinct(eff_analysis$arm)

message("Total subjects: ", n_subjects)
message("Treatment arms: ", n_arms)

# Population summary
message("\nPopulation flags:")
message("  Randomized: ", sum(eff_analysis$randomized_flag, na.rm = TRUE))
message("  ITT: ", sum(eff_analysis$itt_flag, na.rm = TRUE))
message("  Safety: ", sum(eff_analysis$safety_flag, na.rm = TRUE))
message("  PP: ", sum(eff_analysis$pp_flag, na.rm = TRUE))

# TTCC summary by arm
message("\nTTCC by arm:")
ttcc_summary <- eff_analysis %>%
  group_by(arm_label) %>%
  summarise(
    n = n(),
    n_converted = sum(ttcc_event == 1, na.rm = TRUE),
    n_censored = sum(ttcc_event == 0, na.rm = TRUE),
    median_time = median(ttcc_time[ttcc_event == 1], na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(ttcc_summary))

# Missing data
message("\nColumns with missing data:")
missing_summary <- summarize_missing(eff_analysis) %>%
  filter(pct_missing > 0)
if (nrow(missing_summary) > 0) {
  print(missing_summary %>% select(column, n_missing, pct_missing))
} else {
  message("  No missing data in any column")
}


# =============================================================================
# SAVE OUTPUT
# =============================================================================

message("\n--- Saving Output ---")

output_file <- file.path(DATA_OUTPUT_PATH, OUTPUT_FILENAME)
write.csv(eff_analysis, output_file, row.names = FALSE)
message("Analysis dataset: ", output_file)

# Save column documentation
column_info <- data.frame(
  column = names(eff_analysis),
  type = sapply(eff_analysis, function(x) class(x)[1]),
  n_unique = sapply(eff_analysis, function(x) n_distinct(x)),
  n_missing = sapply(eff_analysis, function(x) sum(is.na(x))),
  examples = sapply(eff_analysis, function(x) paste(head(unique(na.omit(x)), 3), collapse = "; "))
)
info_file <- file.path(DATA_OUTPUT_PATH, sub("\\.csv$", "_dictionary.csv", OUTPUT_FILENAME))
write.csv(column_info, info_file, row.names = FALSE)
message("Column info: ", info_file)


# =============================================================================
# COMPLETION
# =============================================================================

message("\n=== Processing Complete ===")
message("Finished: ", Sys.time())
message("\nOutput files ready for use with eff_analysis_report_template.qmd")
