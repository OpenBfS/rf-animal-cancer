# Analysis Workflow

This repository contains the R code used for the analysis of glioma and schwannoma data from long-term carcinogenicity experiments exposing laboratory rats to radiofrequency electromagnetic fields.  
The code provides meta-analyses and dose-response meta-analyses as a quantiative extension of the systematic review by Mevissen et al. 2025, Environment International, doi: 10.1016/j.envint.2025.109482 .
This main repository directory contains analyses done with the odds ratio (OR) as the effect measure. For a sensitivity analysis using the risk ratio (RR), navigate to the directory "sensitivity-analysis-RR".
The workflow is divided into three main stages.

---

## Quick Start
All analyses were performed using the R version and package versions documented in *session_info.txt* to ensure reproducibility.

Before running the scripts, update the `workdir` variable in config.R file to match the directory where you saved this repository.

You already have the prepared `data_*.xlsx` files in the current directory, so you can skip to Step 1 and execute the scripts to run the main analysis.

---

## 0. (Optional) Reproducing the Input Data for the NTP Study

If you want to recreate the poly3-adjusted numbers entered for the NTP study in the Excel tables, run the following two scripts manually in RStudio:

1. glioma_poly3adjust.R
2. schwannoma_poly3adjust.R

Why manual?  
These scripts include a step where Excel files are downloaded and must be opened and re-saved manually in a recent .xlsx format. Because of this, you should run them line by line inside RStudio, following the comments in the code.

> Note: If you already have the prepared data\_\*.xlsx source data Excel tables in the current directory, you can skip this step and move directly to Step 1.

---

## 1. Main Analysis

The main analysis is automated and consists of 12 R scripts.

For every sex-outcome combination there is one script for the associated meta-analyses and one script for the dose-response meta-analyses.  
  
Run each script from your terminal (not inside the R console) using the Rscript command:

```bash
Rscript meta-analysis_male_schwannoma.R
Rscript dosresmeta_male_schwannoma.R

Rscript meta-analysis_female_schwannoma.R
Rscript dosresmeta_female_schwannoma.R

Rscript meta-analysis_m+f_schwannoma.R
Rscript dosresmeta_m+f_schwannoma.R

Rscript meta-analysis_male_glioma.R
Rscript dosresmeta_male_glioma.R

Rscript meta-analysis_female_glioma.R
Rscript dosresmeta_female_glioma.R

Rscript meta-analysis_m+f_glioma.R
Rscript dosresmeta_m+f_glioma.R
```

## 2. Organizing the Output Files
After running the analysis scripts, the output files can be organized into a clear folder structure.

From the repository root directory, open your regular terminal (not RStudio’s terminal) and run:

```bash
cp organize_files.sh meta
cp organize_files.sh dosresmeta
cd meta
bash organize_files.sh
```
Ignore the error "No such file or directory".

Do the same for the dosresmeta directory:

```bash
cd ../dosresmeta
bash organize_files.sh
```

These scripts will move the output files into corresponding subdirectories, giving you a clean, structured overview of all results.

This workflow has been tested in a Unix-like environment (Linux/macOS). Windows users may need to adapt terminal commands.


## License
Code license: MIT (see LICENSE.txt)  
Data license: CC0 1.0 (see LICENSE-DATA.txt)