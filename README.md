# TB-PACTS Tools (tb_pacts_tools)

## Instructions

### RECONFIGURE THE FOLLOWING PATHS:

```{r}
tb_pacts_csv    = '/Volumes/fh/fast/gilbert_p/fg_data/TB-PACTS/csv'
tb_pacts_rds    = '/Volumes/fh/fast/gilbert_p/fg_data/TB-PACTS/rds_files'
metadata_folder = '/Volumes/fh/fast/gilbert_p/fg_data/TB-PACTS/metadata'
figure_save_folder = '/Volumes/fh/fast/gilbert_p/fg_data/TB-PACTS/gitrepos/tb_pacts_tools/demonstration/ae_demo_figures'
```

### LOAD HELPER FUNCTIONS WITH `devtools::load_all()`

```{r}
setwd("/Volumes/fh/fast/gilbert_p/fg_data/TB-PACTS/gitrepos/tb_pacts_tools")
devtools::load_all()
```

```
✓ Loaded Katrina Dobinda's custom functions: pre_processing_ae_data()
✓ Loaded Katrina Dobinda's custom functions: ctgov_ae_tables_single_arm()
✓ Loaded Katrina Dobinda's custom functions: ctgov_ae_tables_multi_arm()
✓ Loaded Katrina Dobinda's custom functions: exporting_xlsx_template()
✓ Loaded custom functions: rm_cols() from data_cleaning_functions.R
✓ Loaded custom save_gg_pdf() from plot_helpers.R
✓ Loaded custom get_cdisc_labels() from variable_namess.R
```

# Demonstrations

## 1. `/demonstration/ae_demo.r`  

This tutorial demonstrates how to analyze and visualize adverse events (AEs)
from the TB-PACTS SimpliciTB clinical trial (Study TB-1037). It loads CDISC-formatted trial data, processes adverse events across three treatment regimens using Katrina Dobinda's CT.gov reporting functions, and creates comparative visualizations. The script generates two types of plots: `spider/radar` charts showing the top 10 most prevalent AE categories by organ system across treatment arms, and `love plots` displaying individual AE terms ranked by frequency. All visualizations are automatically saved as PDFs to a specified folder.