message("✓ Loaded custom functions for data cleaning from data_cleaning_functions.R")

#' Remove Empty Columns from a Data Frame
#'
#' Removes columns that contain only NA values or only empty strings (for character columns).
#' Optionally converts all column names to lowercase.
#'
#' @param my_data A data frame to clean
#' @param to_lower Logical. If TRUE (default), convert all column names to lowercase
#' @param verbose Logical. If TRUE (default), print the number of columns dropped
#'
#' @return A data frame with empty columns removed and optionally lowercased column names
#'
#' @examples
#' # Create example data with empty columns
#' df <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35),
#'   Empty_NA = c(NA, NA, NA),
#'   Empty_String = c("", "", ""),
#'   Score = c(90, 85, 95)
#' )
#' 
#' # Remove empty columns with verbose output
#' cleaned_df <- rm_cols(df)
#' 
#' # Remove empty columns silently, keep original column name case
#' cleaned_df <- rm_cols(df, to_lower = FALSE, verbose = FALSE)
#'
#' @export
rm_empty_cols <- function(my_data, to_lower = TRUE, verbose = TRUE) {
  n_cols_before <- ncol(my_data)
  
  cleaned_data <- my_data %>%
    dplyr::select(where(~!(all(is.na(.)) || (is.character(.) && all(. == "" | is.na(.))))))
  
  n_cols_after <- ncol(cleaned_data)
  n_dropped <- n_cols_before - n_cols_after
  
  if (verbose) {
    cat("Dropped", n_dropped, "column(s).", 
        "Remaining:", n_cols_after, "/", n_cols_before, "\n")
  }
  
  if (to_lower) {names(cleaned_data) <- tolower(names(cleaned_data))}
  
  return(cleaned_data)
}


#' Validate that required columns exist in a data frame
#' 
#' @param df Data frame to validate
#' @param required_cols Character vector of required column names
#' @param df_name Name of the data frame for error messages
#' @param stop_on_missing Logical, whether to stop execution if columns missing
#' @return List with 'valid' (logical) and 'missing' (character vector)
#' @examples
#' validate_columns(ae_data, c("usubjid", "aesoc"), "AE")
validate_columns <- function(df, required_cols, df_name = "data", stop_on_missing = TRUE) {
  missing <- setdiff(tolower(required_cols), tolower(names(df)))
  
  if (length(missing) > 0 && stop_on_missing) {
    stop(paste0("Missing required columns in ", df_name, ": ", 
                paste(missing, collapse = ", ")))
  }
  
  return(list(
    valid = length(missing) == 0,
    missing = missing
  ))
}


#' Report columns that were requested but not found
#' 
#' @param requested Character vector of requested column names
#' @param available Character vector of available column names
#' @param domain_name Name of the domain for messaging
#' @return Character vector of available columns (intersection)
report_missing_cols <- function(requested, available, domain_name = "data") {
  available_lower <- tolower(available)
  requested_lower <- tolower(requested)
  
  found <- requested[requested_lower %in% available_lower]
  missing <- requested[!requested_lower %in% available_lower]
  
  if (length(missing) > 0) {
    message("Note: ", domain_name, " columns not found (will be skipped): ", 
            paste(missing, collapse = ", "))
  }
  
  return(found)
}
