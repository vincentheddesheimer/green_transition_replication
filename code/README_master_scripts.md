# Master Scripts for Green Transition Replication

This directory contains master scripts that organize and run all the replication scripts in the correct order.

**Note:** The raw data for the SOEP, Partypress, and Candidates analysis cannot be shared due to licensing restrictions. Replicators need to acquire this data themselves from the original sources:

- **SOEP data**: German Socio-Economic Panel (DIW Berlin)
- **Partypress data**: Party press releases and media analysis (Erfort et al. (2023))
- **Candidates data**: German Longitudinal Election Study (GLES)

However, we provide all code for creating the final datasets that produce all results. All scripts using this restricted data contain information about where the data should be stored once acquired. To ensure that the master files run without producing errors, we have commented out the code that uses this data as inputs.

**For complete replication:** Once you have acquired the restricted data and placed it in the correct directories, you can uncomment the relevant sections in the build scripts to generate the full datasets.


## Available Master Scripts

### 1. `master_full_replication.R` - Complete Replication
**Use this script for full replication of the entire study.**

This script runs everything in the correct order:
- Phase 1: Data building (all datasets)
- Phase 2: Analysis (all figures and tables)  
- Phase 3: SOEP remote analysis (Stata scripts - manual execution required)

**Usage:**
```r
source("code/master_full_replication.R")
```

### 2. `master_data_build.R` - Data Building Only
**Use this script if you only want to create the datasets.**

This script runs all data preparation scripts:
- County data preparation (crosswalks, brownness, covariates)
- Candidates data preparation
- Partypress data preparation
- SOEP data preparation

**Note:** The intermediate dataset `county_isei_2015.csv` is built via SOEP remote analysis and is used in the county-level analysis. The `master_data_build.R` script is designed so that replicators can run the county-level analysis without needing to execute the SOEP remote scripts first (the required `county_isei_2015.csv` file is included in the replication package).

**For complete replication from scratch:** If you want to rebuild the entire database including the SOEP remote data, you would need to:
1. Remove the merging of `county_isei_2015.csv` from the county-level build file (`04_build_data_main.R`)
2. Run the SOEP remote scripts (Stata files in `code/analyze/soep_remote/`)
3. Add the newly generated `county_isei_2015.csv` data back to the county-level analysis


**Usage:**
```r
source("code/master_data_build.R")
```

### 3. `master_analysis.R` - Analysis Only
**Use this script if you only want to run the analysis (for full replication, this code requires replicators to acquire the restricted datasets).**

This script runs all analysis scripts:
- Main figures and tables (Figures 2-7, Tables 1, F1-F5, G1, G3-G5, G9-G10)
- Supplementary figures and tables (Figures A1-A3, B1-B2, C1, D1, F7, G2-G5, Tables E1-E2, F6-F8, G6-G8)
- SOEP remote analysis (Stata scripts - listed but not executed)

**Usage:**
```r
source("code/master_analysis.R")
```

## Prerequisites

Before running any master script, ensure you have:

1. **Required R packages:**
   ```r
   install.packages("pacman")
   pacman::p_load(tidyverse, data.table, sf, ggrepel, kableExtra, fixest)
   ```

2. **Required data files:**
   - All raw data files in `data/raw/` directories
   - Shapefiles in `data/raw/shapefiles/`

3. **For SOEP remote analysis:**
   - Either access the SOEP data via remote access or via a guest stay at DIW Berlin
   - SOEP data files (not included in replication package)

## Execution Order

The scripts are designed to be run in this order:

1. **Data Building** (`master_data_build.R`):
   - County crosswalks (01_build_cty_crosswalk.R)
   - County brownness scores (02_build_cty_brownness.R)
   - County covariates (03_build_covars_data.R)
   - Main county dataset (04_build_data_main.R)
   - Annual county dataset (05_build_data_annual.R)
   - Sanity checks (06_sanity_checks.R)
   - Far-right parties data (data_far_right_parties.R)
   - Candidates data (01-03_build_candidates.R)
   - Partypress data (01-03_build_partypress.R)
   - SOEP data (various scripts in soep/ subdirectories)

2. **Analysis** (`master_analysis.R`):
   - Main figures and tables
   - Supplementary figures and tables
   - SOEP remote analysis (manual Stata execution)

## Output Files

### Data Files (created by `master_data_build.R`)
- `data/final/data_main.csv` - Main county-level dataset
- `data/final/cty_annual.csv` - Annual county-level dataset
- Other datasets in `data/final/` depending on whether restricted data was acquired

### Results Files (created by `master_analysis.R`)
- `results/fig_*.pdf` - All figures
- `results/tab_*.tex` - All tables