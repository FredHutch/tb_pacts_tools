message("✓ Loaded helpers from helpers.R")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(lubridate)
  library(stringr)
  library(forcats)
  library(ggradar)
  library(ggpubr)
  library(purrr)
})

tbpacts_datapath = '~/fg_data/TB-PACTS'
tbpacts_adata = file.path(tbpacts_datapath, "adata")
tbpacts_csv    = file.path(tbpacts_datapath, "csv")
metadata_folder = file.path(tbpacts_datapath, "metadata")