# =============================================================================
# ADVERSE EVENTS (AE) ANALYSIS DATASET PROCESSING TEMPLATE
# =============================================================================
#
# PURPOSE:
#   Process raw CDISC SDTM adverse event (AE) and demographics (DM) data to 
#   create a standardized analysis dataset for safety reporting.
#
# OUTPUT:
#   - ae_analysis_[STUDYID]_[DATE].csv: Analysis-ready dataset
#   - ae_analysis_[STUDYID]_[DATE]_column_info.csv: Column documentation
#
# USAGE:
#   1. Update the STUDY CONFIGURATION section below
#   2. Run the entire script
#   3. Use output with ae_analysis_report_template.qmd for visualization
#
#
# AUTHOR:       [Your Name]
# CREATED:      [Date]
# MODIFIED:     
# STUDY ID:     
# =============================================================================

# =============================================================================
# LOAD HELPERS AND CONFIGURATION
# =============================================================================

setwd("~/tb_pacts_tools")
devtools::load_all()

# =============================================================================
# STUDY CONFIGURATION - MODIFY THIS SECTION FOR EACH STUDY
# =============================================================================

# ---- STUDY IDENTIFICATION ----
# The unique study identifier(s) as they appear in STUDYID column
STUDY_IDS <- c("TB-1037")

# ---- FILE PATHS ----
# Path to directory containing CDISC CSV (path loaded from helpers.R)
DATA_INPUT_PATH <- tbpacts_csv

# Path to save output analysis dataset (path loaded from helpers.R)
DATA_OUTPUT_PATH <- file.path(tbpacts_adata, paste(STUDY_IDS, collapse = "_"))

# ---- STUDY DATES ----
# Reference date for calculating calendar dates from study days
# Typically first subject's first dose date or study start date
STUDY_REFERENCE_DATE <- as.Date("2018-07-31")

# Default consent date (used as cutoff for treatment-emergent AEs)
DEFAULT_CONSENT_DATE <- as.Date("2018-08-01")

# ---- TREATMENT ARMS ----
# Which treatment arms to include in analysis
# Set to NULL to include all arms found in the data
# Use ACTARMCD or ARMCD values (actual treatment arm codes)
TREATMENT_ARMS <- NULL


# =============================================================================
# SETUP - DO NOT MODIFY
# =============================================================================

# =============================================================================
# LOAD REQUIRED PACKAGES
# =============================================================================

# Core data manipulation packages
library(dplyr)      # Data manipulation
library(tidyr)      # Data tidying
library(magrittr)   # Pipe operators
library(lubridate)  # Date handling
library(stringr)    # String manipulation


# Generate output filename
OUTPUT_FILENAME <- paste0("ae_adata_", 
                          paste(STUDY_IDS, collapse = "_"), 
                          "_", format(Sys.Date(), "%Y%m%d"), 
                          ".csv")

# Ensure output directory exists
if (!dir.exists(DATA_OUTPUT_PATH)) {
  dir.create(DATA_OUTPUT_PATH, recursive = TRUE)
  message("Created directory: ", DATA_OUTPUT_PATH)
}

message("\n=== AE Analysis Dataset Processing ===")
message("Study ID(s): ", paste(STUDY_IDS, collapse = ", "))
message("Started: ", Sys.time())


# =============================================================================
# LOAD AND FILTER DATA
# =============================================================================

message("\n--- Loading Data ---")

ae <- load_domain("ae", DATA_INPUT_PATH, STUDY_IDS, required = TRUE)
dm <- load_domain("dm", DATA_INPUT_PATH, STUDY_IDS, required = TRUE)

message("AE after study filter: ", nrow(ae), " rows, ", ncol(ae), " columns")
message("DM after study filter: ", nrow(dm), " rows, ", ncol(dm), " columns")

if (nrow(ae) == 0) stop("No AE records found for study: ", paste(STUDY_IDS, collapse = ", "))
if (nrow(dm) == 0) stop("No DM records found for study: ", paste(STUDY_IDS, collapse = ", "))


# =============================================================================
# SELECT VARIABLES
# =============================================================================

# ---- AE Variables ----
# Core CDISC SDTM AE columns (standardized names)
AE_CORE_COLS <- c(
  "usubjid",    # Subject identifier
  "aeseq",      # Sequence number
  "aestdy",     # Study day of AE start
  "aeendy",     # Study day of AE end
  "epoch",      # Study epoch
  "aesoc",      # System Organ Class
  "aehlgt",     # High Level Group Term
  "aeser",      # Serious AE flag (Y/N)
  "aerel",      # Causality/Relationship
  "aeout",      # Outcome
  "aetoxgr",    # Toxicity grade
  "trtemerg"    # Treatment emergent flag (Y/N)
)

# ---- DM Variables ----
# Core CDISC SDTM DM columns (standardized names)
DM_CORE_COLS <- c(
  "usubjid",    # Subject identifier (for merge)
  "studyid",    # Study identifier
  "age",        # Age
  "sex",        # Sex
  "race",       # Race
  "ethnic",     # Ethnicity
  "country",    # Country
  "actarmcd",   # Actual arm code (standardized column for arm)
  "actarm",     # Actual arm name
  "armcd",      # Planned arm code (backup)
  "arm",        # Planned arm name (backup)
  # Population flags
  "safety",     # Safety population flag
  "itt",        # Intent-to-treat flag
  "dthfl"       # Death flag
)

# Select available columns (will report any columns above that are not in the dataset)
ae_cols <- report_missing_cols(AE_CORE_COLS, names(ae), "AE")
dm_cols <- report_missing_cols(DM_CORE_COLS, names(dm), "DM")

ae <- ae %>% select(all_of(ae_cols))
dm <- dm %>% select(all_of(dm_cols))


# =============================================================================
# FILTER TO TREATMENT-EMERGENT AEs
# =============================================================================

message("\n--- Filtering Treatment-Emergent AEs ---")

if ("trtemerg" %in% names(ae)) {
  n_before <- nrow(ae)
  ae <- ae %>% filter(trtemerg == "Y")
  message("Records before: ", n_before, " | After: ", nrow(ae), 
          " | Excluded: ", n_before - nrow(ae))
} else {
  message("Note: TRTEMERG column not found. Including all AEs.")
}


# =============================================================================
# MERGE AE AND DM
# =============================================================================

message("\n--- Merging AE and DM ---")

ae_dm <- ae %>%
  left_join(dm, by = "usubjid", suffix = c("", "_dm"))

message("Merged dataset: ", nrow(ae_dm), " rows")

# Check for unmatched subjects
n_unmatched <- sum(is.na(ae_dm$studyid))
if (n_unmatched > 0) {
  message("Warning: ", n_unmatched, " AE records have no matching DM record")
}


# =============================================================================
# FILTER TREATMENT ARMS
# =============================================================================

message("\n--- Processing Treatment Arms ---")

# Use actarmcd as the standard arm column
if (!"actarmcd" %in% names(ae_dm)) {
  if ("armcd" %in% names(ae_dm)) {
    message("Note: Using ARMCD instead of ACTARMCD")
    ae_dm <- ae_dm %>% mutate(actarmcd = armcd)
  } else {
    stop("No treatment arm column (ACTARMCD or ARMCD) found in data")
  }
}

# Report available arms
available_arms <- sort(unique(ae_dm$actarmcd))
message("Available arms: ", paste(available_arms, collapse = ", "))

# Filter to specified arms
if (!is.null(TREATMENT_ARMS)) {
  n_before <- nrow(ae_dm)
  ae_dm <- ae_dm %>% filter(actarmcd %in% TREATMENT_ARMS)
  message("Filtering to: ", paste(TREATMENT_ARMS, collapse = ", "))
  message("Records after arm filter: ", nrow(ae_dm))
  
  missing_arms <- setdiff(TREATMENT_ARMS, available_arms)
  if (length(missing_arms) > 0) {
    warning("Requested arms not found: ", paste(missing_arms, collapse = ", "))
  }
}


# =============================================================================
# CREATE STANDARDIZED ANALYSIS VARIABLES
# =============================================================================

message("\n--- Creating Standardized Variables ---")

ae_analysis <- ae_dm %>%
  mutate(
    ptid = usubjid,
    arm_code = actarmcd,
    arm_desc = actarm,
    ae_st_date = study_day_to_date(aestdy, STUDY_REFERENCE_DATE),
    dt_consent = DEFAULT_CONSENT_DATE,
    ae_ctcae_cat = toupper(aesoc),
    ae_ctcae_term_value = toupper(aehlgt),
    ae_sae = standardize_sae(aeser)
  )

# Add population flags if source columns exist
if ("safety" %in% names(ae_dm)) {
  ae_analysis <- ae_analysis %>% mutate(safety_population = standardize_flag(safety))
}
if ("itt" %in% names(ae_dm)) {
  ae_analysis <- ae_analysis %>% mutate(itt_population = standardize_flag(itt))
}
if ("dthfl" %in% names(ae_dm)) {
  ae_analysis <- ae_analysis %>% mutate(death_flag = standardize_flag(dthfl))
}


# =============================================================================
# SELECT FINAL OUTPUT COLUMNS
# =============================================================================

# New columns created during standardization
NEW_COLS <- c(
  "ptid",
  "arm_code", "arm_desc",
  "ae_st_date", "dt_consent",
  "ae_ctcae_cat", "ae_ctcae_term_value", "ae_sae",
  "safety_population", "itt_population", "death_flag"
)

# Output = new columns + original requested columns
OUTPUT_COLS <- unique(c(NEW_COLS, AE_CORE_COLS, DM_CORE_COLS))

# Select available columns in desired order
output_cols_available <- intersect(OUTPUT_COLS, names(ae_analysis))
ae_final <- ae_analysis %>% select(all_of(output_cols_available))


# =============================================================================
# DATA QUALITY SUMMARY
# =============================================================================

message("\n--- Data Quality Summary ---")

# Overall counts
n_subjects <- n_distinct(ae_final$ptid)
n_events <- nrow(ae_final)
n_arms <- n_distinct(ae_final$arm_code)

message("Total subjects: ", n_subjects)
message("Total AE events: ", n_events)
message("Treatment arms: ", n_arms)

# Population flags summary
if ("safety_population" %in% names(ae_final)) {
  n_safety <- sum(ae_final$safety_population == "Yes", na.rm = TRUE)
  n_safety_subj <- ae_final %>% 
    filter(safety_population == "Yes") %>% 
    summarise(n = n_distinct(ptid)) %>% 
    pull(n)
  message("\nSafety population: ", n_safety_subj, " subjects, ", n_safety, " AE records")
}

if ("itt_population" %in% names(ae_final)) {
  n_itt <- sum(ae_final$itt_population == "Yes", na.rm = TRUE)
  message("ITT population: ", n_itt, " AE records with ITT=Yes")
}

if ("death_flag" %in% names(ae_final)) {
  n_deaths <- ae_final %>% 
    filter(death_flag == "Yes") %>% 
    summarise(n = n_distinct(ptid)) %>% 
    pull(n)
  message("Deaths: ", n_deaths, " subjects")
}

# Events per arm
message("\nEvents per treatment arm:")
print(summarize_by_arm(ae_final))

# Missing data
message("\nColumns with missing data:")
missing_summary <- summarize_missing(ae_final) %>% 
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

# Save analysis dataset
output_file <- file.path(DATA_OUTPUT_PATH, OUTPUT_FILENAME)
write.csv(ae_final, output_file, row.names = FALSE)
message("Analysis dataset: ", output_file)

# Save column documentation
column_info <- data.frame(
  column = names(ae_final),
  type = sapply(ae_final, function(x) class(x)[1]),
  n_unique = sapply(ae_final, function(x) n_distinct(x)),
  n_missing = sapply(ae_final, function(x) sum(is.na(x))),
  examples = sapply(ae_final, function(x) paste(head(unique(na.omit(x)), 3), collapse = "; "))
)
info_file <- file.path(DATA_OUTPUT_PATH, sub("\\.csv$", "_dictionary.csv", OUTPUT_FILENAME))
write.csv(column_info, info_file, row.names = FALSE)
message("Column info: ", info_file)

# =============================================================================
# COMPLETION
# =============================================================================

message("\n=== Processing Complete ===")
message("Finished: ", Sys.time())
message("\nOutput files ready for use with ae_analysis_report_template.qmd")
