message("✓ Loaded custom functions: rm_cols() from data_cleaning_functions.R")

#' Remove Empty Columns from a Data Frame
#'
#' Removes columns that contain only NA values or only empty strings (for character columns).
#' Optionally converts all column names to lowercase.
#'
#' @param my_data A data frame to clean
#' @param tolower Logical. If TRUE (default), convert all column names to lowercase
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
#' cleaned_df <- rm_cols(df, tolower = FALSE, verbose = FALSE)
#'
#' @export
rm_cols <- function(my_data, tolower = TRUE, verbose = TRUE) {
  n_cols_before <- ncol(my_data)
  
  cleaned_data <- my_data %>%
    dplyr::select(where(~!(all(is.na(.)) || (is.character(.) && all(. == "" | is.na(.))))))
  
  n_cols_after <- ncol(cleaned_data)
  n_dropped <- n_cols_before - n_cols_after
  
  if (verbose) {
    cat("Dropped", n_dropped, "column(s).", 
        "Remaining:", n_cols_after, "/", n_cols_before, "\n")
  }
  
  if (tolower) {names(cleaned_data) <- tolower(names(cleaned_data))}
  
  return(cleaned_data)
}

