# TB-PACTS Tools (tb_pacts_tools)

This repo is an R package with tools for creating and validating analysis datasets from the raw TB-PACTs datasets.

## Contents

### `R` folder

Since this repository is an actual R package, there are several helper functions in the `R` folder that can be imported by loading or reloading the package by installing the package and running `load(tb_pacts_tools)` or by changing to the repo folder and running:

```
setwd("~/tb_pacts_tools")
devtools::load_all()
```

You can modify the paths in `R/helpers.R` to change where code and data is located on your computer. Check these paths to make sure you have access to the data.


### `processing_templates` folder

The R scripts in this repo process and combine the raw CSV datasets from TB-PACTs and produce analysis datasets that are ready for downstream analysis. You should be able to run these scripts to create analysis datasets for adverse events (AE) and TB treatment efficacy:

```
process_ae_data_template.R
process_eff_data_template.R
```

They are considered templates because they have been written and tested to process data from one trial in the TB-PACTS repository. The scripts could be copied and modified to work for additional studies, with each checked individually for quality.

There are also two Quarto markdown files (QMDs) that load the analysis datasets and perform checks and make plots for verifying the quality of the processed data.
```
ae_analysis_report_template.qmd
eff_analysis_report_template.qmd
```

Once the relevenat analysis dataset has been created for the specific study, the QMD can be rendered either interactively in R studio or on the command line terminal:
```
quarto render ae_analysis_report_template.qmd
```

## Next steps

Make a copy of the template and name it for the study with which you want to be working. Try to run the processing script and the report QMD. Try to put all study-specific modifications into the pre-processing code when possible so that we get standardized analysis datasets for every study. Ideally, the same reporting QMD will work for the analysis dataset from all studies, enabling simplified meta-analysis.
