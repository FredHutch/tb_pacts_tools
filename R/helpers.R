message("✓ Loaded important packages and relevant paths from helpers.R")

tbpacts_datapath = '/home/TB-PACTS'
tbpacts_adata = file.path("~/adata")
tbpacts_csv    = file.path(tbpacts_datapath, "csv")
metadata_folder = file.path(tbpacts_datapath, "metadata")

# suppressPackageStartupMessages({
  
#   # ---------------------------------------------------------------------------
#   # CORE DATA MANIPULATION (Required)
#   # ---------------------------------------------------------------------------
#   # These packages are essential for basic data operations
#   library(dplyr)        # Data manipulation (filter, mutate, summarize, etc.)
#   library(tidyr)        # Data reshaping (pivot, separate, nest, etc.)
  
#   # ---------------------------------------------------------------------------
#   # DATA IMPORT/EXPORT
#   # ---------------------------------------------------------------------------
#   # Packages for reading and writing various file formats
#   require(readxl)       # Read Excel files (.xlsx, .xls)
#   require(openxlsx)     # Read/write Excel with formatting support
#   require(writexl)      # Write Excel files (simpler alternative)
#   require(jsonlite)     # Read/write JSON data
  
#   # ---------------------------------------------------------------------------
#   # DATA MANIPULATION UTILITIES
#   # ---------------------------------------------------------------------------
#   require(stringr)      # String manipulation (str_detect, str_replace, etc.)
#   require(forcats)      # Factor/categorical variable handling
#   require(lubridate)    # Date/time manipulation and parsing
#   require(purrr)        # Functional programming tools (map functions)
#   require(magrittr)     # Pipe operators (%>%, %<>%, etc.)
#   require(tibble)       # Modern data frames
#   require(janitor)
  
#   # ---------------------------------------------------------------------------
#   # DATA VISUALIZATION
#   # ---------------------------------------------------------------------------
#   library(ggplot2)      # Core plotting package (grammar of graphics)
#   require(ggpubr)       # Publication-ready plots and statistical comparisons
#   require(fmsb)         # Radar/spider chart creation
#   require(ggradar)      # ggplot2-style radar charts (install via: devtools::install_github("ricardo-bion/ggradar"))
  
#   # ---------------------------------------------------------------------------
#   # TABLE FORMATTING & REPORTING
#   # ---------------------------------------------------------------------------
#   require(knitr)        # Dynamic report generation (R Markdown)
#   require(kableExtra)   # Advanced table formatting for HTML/PDF
#   require(gtsummary)    # Summary tables for statistical analysis
#   require(summarytools) # Descriptive statistics and data summaries
  
#   # ---------------------------------------------------------------------------
#   # SPECIALIZED/DOMAIN-SPECIFIC
#   # ---------------------------------------------------------------------------
#   # require(ctrdata)      # Clinical trial data retrieval and analysis
  
# })

