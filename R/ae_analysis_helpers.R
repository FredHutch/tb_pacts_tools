# =============================================================================
# AE ANALYSIS HELPER FUNCTIONS
# =============================================================================
#
# PURPOSE:
#   Reusable utility functions for processing CDISC SDTM adverse event data
#   and creating analysis datasets.
#
# USAGE:
#   source("ae_analysis_helpers.R")
#
# DEPENDENCIES:
#   - dplyr
#   - tidyr
#   - lubridate
#   - magrittr
#
# VERSION:      1.0
# =============================================================================


# =============================================================================
# VARIABLE STANDARDIZATION FUNCTIONS
# =============================================================================

#' Standardize SAE indicator to Yes/No format
#' 
#' Converts various SAE coding schemes to standardized "Yes"/"No" values.
#' Handles: Y/N, 1/0, Yes/No, TRUE/FALSE
#' 
#' @param x Vector of SAE values
#' @return Character vector with "Yes", "No", or NA
#' @examples
#' standardize_sae(c("Y", "N", "Y", NA))
standardize_sae <- function(x) {
  dplyr::case_when(
    toupper(x) %in% c("Y", "YES", "1", "TRUE") ~ "Yes",
    toupper(x) %in% c("N", "NO", "0", "FALSE") ~ "No",
    is.na(x) ~ NA_character_,
    TRUE ~ NA_character_
  )
}


#' Standardize population flags to Yes/No format
#' 
#' Converts various flag coding schemes to standardized "Yes"/"No" values.
#' 
#' @param x Vector of flag values
#' @return Character vector with "Yes", "No", or NA
standardize_flag <- function(x) {
  dplyr::case_when(
    toupper(as.character(x)) %in% c("Y", "YES", "1", "TRUE") ~ "Yes",
    toupper(as.character(x)) %in% c("N", "NO", "0", "FALSE") ~ "No",
    is.na(x) ~ NA_character_,
    TRUE ~ NA_character_
  )
}


#' Calculate date from study day and reference date
#' 
#' @param study_day Numeric study day (can be negative for pre-baseline)
#' @param reference_date Date object for day 1
#' @return Date object
#' @examples
#' study_day_to_date(15, as.Date("2018-08-01"))
study_day_to_date <- function(study_day, reference_date) {
  if (!inherits(reference_date, "Date")) {
    reference_date <- as.Date(reference_date)
  }
  reference_date + lubridate::days(study_day)
}


# =============================================================================
# DATA SUMMARY FUNCTIONS
# =============================================================================

#' Generate a summary of missing data by column
#' 
#' @param df Data frame to summarize
#' @return Data frame with column name, n_missing, pct_missing
#' @examples
#' summarize_missing(ae_data)
summarize_missing <- function(df) {
  data.frame(
    column = names(df),
    n_total = nrow(df),
    n_missing = sapply(df, function(x) sum(is.na(x))),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      n_complete = n_total - n_missing,
      pct_missing = round(100 * n_missing / n_total, 1),
      pct_complete = round(100 * n_complete / n_total, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(pct_missing))
}


#' Summarize AE data by treatment arm
#' 
#' @param df Analysis dataset with arm_code, ptid, ae_sae columns
#' @return Data frame with summary statistics per arm
#' @examples
#' summarize_by_arm(ae_analysis)
summarize_by_arm <- function(df) {
  df %>%
    dplyr::group_by(arm_code) %>%
    dplyr::summarise(
      n_subjects = dplyr::n_distinct(ptid),
      n_events = dplyr::n(),
      n_sae = sum(ae_sae == "Yes", na.rm = TRUE),
      n_non_sae = sum(ae_sae == "No", na.rm = TRUE),
      events_per_subject = round(dplyr::n() / dplyr::n_distinct(ptid), 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(arm_code)
}



# =============================================================================
# WRAPPER FUNCTIONS FOR MULTI-ARM PROCESSING
# =============================================================================
# These are additional convenience functions that wrap the Dobinda functions
# for easier use with dynamic treatment arms
# =============================================================================

#' Process AE Data for All Treatment Arms
#'
#' Wrapper function that processes AE data for all arms in the dataset
#' using the Dobinda CT.gov functions.
#'
#' @param ae_data Full analysis dataset with arm_code column
#' @param sae_or_other Either "Serious" or "Other"
#' @param safety_population_only Logical, filter to safety population if available
#' @return List with arm_tables, combined table, and arm names
#' @examples
#' results <- process_all_arms(ae_analysis, "Other")
process_all_arms <- function(ae_data, sae_or_other = "Other", safety_population_only = TRUE) {
  # Filter to safety population if requested and column exists
  if (safety_population_only && "safety_population" %in% names(ae_data)) {
    ae_data <- ae_data %>% filter(safety_population == "Yes")
  }
  
  # Get unique arms
  arms <- sort(unique(ae_data$arm_code))
  
  # Pre-process data using Dobinda function
 ae_processed <- pre_processing_ae_data(ae_data)
  
  # Split by arm
  ae_by_arm <- split(ae_processed, ae_processed$arm_code)
  
  # Process each arm using Dobinda function
  arm_tables <- list()
  for (arm in arms) {
    arm_data <- ae_by_arm[[arm]]
    
    # Create CT.gov table using Dobinda function
    arm_table <- ctgov_ae_tables_single_arm(
      processed_ae_data = arm_data,
      ptid_ae_evaluable = unique(arm_data$ptid),
      trt_name = arm,
      sae_or_other = sae_or_other
    )
    
    # Add numEvents (total event count, not just subjects)
    # This follows the pattern from the original process_ae_template.R
    events_col <- paste0(arm, "{numEvents}")
    arm_table <- arm_table %>%
      left_join(
        arm_data %>% 
          group_by(term) %>% 
          summarise(n_events = n(), .groups = "drop"),
        by = "term"
      ) %>%
      mutate(!!events_col := n_events) %>%
      select(-n_events)
    
    arm_tables[[arm]] <- arm_table
  }
  
  # Combine all arms using Dobinda function
  combined_table <- ctgov_ae_tables_multi_arm(arm_tables)
  
  return(list(
    arm_tables = arm_tables,
    combined = combined_table,
    arms = arms
  ))
}


#' Prepare Filtered AE Data for Plotting
#'
#' Transforms CT.gov table format into plotting-ready format.
#' This follows the pattern from the original process_ae_template.R
#'
#' @param arm_table Single arm CT.gov table from ctgov_ae_tables_single_arm()
#' @param arm_name Treatment arm name
#' @return Data frame ready for visualization
prepare_filtered_data <- function(arm_table, arm_name) {
  # Column names for this arm
  affected_col <- paste0(arm_name, "{numSubjectsAffected}")
  risk_col <- paste0(arm_name, "{numSubjectsAtRisk}")
  events_col <- paste0(arm_name, "{numEvents}")
  
  filtered <- arm_table %>%
    arrange(desc(.data[[affected_col]])) %>%
    mutate(
      pct_affected = .data[[affected_col]] / .data[[risk_col]],
      arm_desc = arm_name
    ) %>%
    rename(
      numAffected = !!affected_col,
      numAtRisk = !!risk_col,
      numEvents = !!events_col,
      event_type = adverseEventType
    )
  
  return(filtered)
}


# =============================================================================
# LOADING MESSAGE
# =============================================================================

message("
================================================================================
AE Analysis Helper Functions Loaded
================================================================================

Standardization:
  - standardize_sae()
  - standardize_flag()
  - study_day_to_date()

Summaries:
  - summarize_missing()
  - summarize_by_arm()

Wrapper Functions:
  - process_all_arms()
  - prepare_filtered_data()
================================================================================
")
