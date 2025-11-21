# Analysis Workflow

This repository contains the R code used for the analysis of glioma and schwannoma data from long-term carcinogenicity experiments exposing laboratory rats to radiofrequency electromagnetic fields.  
The code provides meta-analyses and dose-response meta-analyses as a quantiative extension of the systematic review by Mevissen et al. 2025, Environment International, doi: 10.1016/j.envint.2025.109482 .
This directory contains code for a sensitivity analysis testing the robustness of results when the risk ratio (RR) is used as the effect measure, instead of the odds ratio (OR).
The workflow is divided into two main stages.

---

## Quick Start
All analyses were performed using the R version and package versions documented in *session_info.txt* to ensure reproducibility.

Before running the scripts, update the `workdir` variable in config.R file to match the directory where you saved this repository.

You already have the prepared `data_*.xlsx` files in the current directory, so you can run the main analysis.

---

## 1. Main Analysis

The main analysis is automated and consists of 8 R scripts.

For every sex-outcome combination there is one script for the associated meta-analyses and one script for the dose-response meta-analyses.  
  
Run each script from your terminal (not inside the R console) using the Rscript command:

```bash
Rscript meta-analysis_male_schwannoma_RR.R
Rscript dosresmeta_male_schwannoma_RR.R

Rscript meta-analysis_female_schwannoma_RR.R
Rscript dosresmeta_female_schwannoma_RR.R

Rscript meta-analysis_male_glioma_RR.R
Rscript dosresmeta_male_glioma_RR.R

Rscript meta-analysis_female_glioma_RR.R
Rscript dosresmeta_female_glioma_RR.R


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