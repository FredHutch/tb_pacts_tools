message("✓ Loaded custom save_gg_pdf() from plot_helpers.R")
#' Save ggarrange Plot to PDF
#'
#' Saves a ggarrange plot object to a PDF file in the specified folder.
#' If file exists, prompts user for confirmation before overwriting.
#'
#' @param plot A plot object created by ggarrange
#' @param filename Character string. Name of the PDF file (with or without .pdf extension)
#' @param save_folder Character string. Path to folder where PDF should be saved.
#' @param width Numeric. Width of PDF in inches. Default is 11
#' @param height Numeric. Height of PDF in inches. Default is 8.5
#' @param verbose Logical. If TRUE, prints message about where file was saved. Default is TRUE
#' @param overwrite Logical. If TRUE, overwrites existing file without prompting. 
#'   If FALSE, stops if file exists. If NULL (default), prompts user interactively.
#'
#' @return Invisibly returns the file path where the PDF was saved, or NULL if cancelled
#'
#' @export
save_gg_pdf <- function(plot, 
                               filename, 
                               save_folder,
                               width = 11, 
                               height = 8.5,
                               verbose = TRUE,
                               overwrite = NULL) {
  
  # Add .pdf extension if not present
  if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".pdf")
  }
  
  # Create full file path
  filepath <- file.path(save_folder, filename)
  
  # Check if file exists and handle overwrite logic
  if (file.exists(filepath)) {
    if (is.null(overwrite)) {
      # Prompt user interactively
      response <- readline(prompt = paste0(
        "File '", filename, "' already exists. Overwrite? (y/n): "
      ))
      
      if (!tolower(response) %in% c("y", "yes")) {
        message("✗ Save cancelled. File not overwritten.")
        return(invisible(NULL))
      }
    } else if (!overwrite) {
      stop("File '", filepath, "' already exists and overwrite = FALSE")
    }
    # If overwrite = TRUE, proceed without prompting
  }
  
  # Save the plot
  ggplot2::ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    device = "pdf"
  )
  
  if (verbose) {
    message("✓ PDF saved to: ", filepath)
  }
  
  invisible(filepath)
}