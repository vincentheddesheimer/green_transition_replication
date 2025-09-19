# Replication Materials for "The Green Transition and Political Polarization Along Occupational Lines"

**Authors:** Vincent Heddesheimer, Hanno Hilbig, Erik Voeten 
 
**Contact:** [vincent.heddesheimer@princeton.edu](mailto:vincent.heddesheimer@princeton.edu); [hhilbig@ucdavis.edu](mailto:hhilbig@ucdavis.edu)

**Date:** August 27, 2025

## Overview

This repository contains the complete replication materials for the paper "The Green Transition and Political Polarization Along Occupational Lines." The analysis examines the relationship between brown employment (jobs in emission-intensive sectors) and support for far-right parties in Germany, particularly the AfD, using county-level election data and individual-level SOEP survey data.

## Table of Contents

### Documentation Files
- `README.md` - This file
- `LICENSE` - License information
- `codebooks/` - Data codebooks:
  - `data_main_codebook.md` - County-level data documentation
  - `data_candidates_codebook.md` - Candidate data documentation
  - `data_soep_codebook.md` - SOEP survey data documentation
  - `data_partypress_codebook.md` - Party press data documentation

### Code Files

#### Master Scripts (Recommended)
- `code/master_full_replication.R` - **Complete replication pipeline** (data building + analysis)
- `code/master_data_build.R` - Data building only (creates all openly available datasets)
- `code/master_analysis.R` - Analysis only (creates results based on openly available datasets)
- `code/check_prerequisites.R` - Check if all prerequisites are met before running

#### Main Analysis Scripts
- `code/analyze/tab_1.R` - Table 1: Summary statistics for county-level variables (2013)
- `code/analyze/fig_2_4_E1_E2_E3.R` - Figures 2, 4, E.1, E.2, E.3: Maps and scatter plots
- `code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R` - Main county level analysis (Figures 3, 5, F.1-F.14, Tables F.1-F.5)
- `code/analyze/fig_C1.R` - Figure C.1: Far-right parties over time
- `code/analyze/fig_F7.R` - Figure F.7: Changes in brown employment as treatment


#### Analysis Scripts that require restricted data
- `code/analyze/fig_6_G1_tab_G1_G3_G4_G5.R` - SOEP individual-level analysis (Figure 6, G.1, Tables G.1, G.3-G.5)
- `code/analyze/fig_7_tab_G9_G10.R` - Mechanism analysis (Figure 7, Tables G.9, G.10)
- `code/analyze/fig_G2_G3_G4.R` - Supplementary figures G.2-G.4
- `code/analyze/fig_A1_A2_A3_B1_B2.R` - Appendix figures A.1-A.3, B.1-B.2: Press release analysis
- `code/analyze/fig_D1.R` - Figure D.1: SOEP occupation differences over time
- `code/analyze/tab_E1.R` - Table E.1: Brown occupations classification
- `code/analyze/tab_E2.R` - Table E.2: Brown employment over time
- `code/analyze/tab_F6_F7_F8.R` - Tables F.6-F.8: Candidate analysis
- `code/analyze/fig_G5.R` - Figure G.5: Predicted status analysis
- `code/analyze/tab_G6.R` - Table G.6: Individual attitudes analysis
- `code/analyze/tab_G7_G8.R` - Tables G.7-G.8: Heterogeneity analysis

#### SOEP Remote Analysis Scripts
- `code/analyze/soep_remote/01_upload_cty_brownness.do` - Upload county brownness data
- `code/analyze/soep_remote/02_upload_isco08_brownness.do` - Upload ISCO-08 brownness data
- `code/analyze/soep_remote/03_upload_isco88_brownness.do` - Upload ISCO-88 brownness data
- `code/analyze/soep_remote/04_upload_weights.do` - Upload survey weights
- `code/analyze/soep_remote/05_prepare_soep.do` - Prepare SOEP data
- `code/analyze/soep_remote/06_transform_soep.do` - Transform SOEP data
- `code/analyze/soep_remote/07_merge_dfs.do` - Merge datasets
- `code/analyze/soep_remote/08_voting.do` - Voting behavior analysis
- `code/analyze/soep_remote/09_attitudes.do` - Political attitudes analysis
- `code/analyze/soep_remote/10_upload_cty_brownness_1999.do` - Upload 1999 county brownness
- `code/analyze/soep_remote/11_merge_w_1999.do` - Merge with 1999 data
- `code/analyze/soep_remote/12_attitudes_1999_womig.do` - Attitudes excluding migration
- `code/analyze/soep_remote/13_prepapre_soep_is.do` - Prepare SOEP IS data
- `code/analyze/soep_remote/14_transform_soep_is.do` - Transform SOEP IS data
- `code/analyze/soep_remote/15_merge_dfs_is.do` - Merge IS datasets
- `code/analyze/soep_remote/16_status.do` - Status analysis
- `code/analyze/soep_remote/17_status_pooled.do` - Pooled status analysis
- `code/analyze/soep_remote/18_status_pooled_table.do` - Status pooled table

#### Helper Functions
- `code/helper_functions/func_dict_covars.R` - Covariate dictionary
- `code/helper_functions/func_fixest_dict_cty.R` - Variable labels for county-level regressions
- `code/helper_functions/func_fixest_dict_soep.R` - Variable labels for SOEP regressions
- `code/helper_functions/func_theme_custom.R` - Custom ggplot theme
- `code/helper_functions/func_tidy_feols.R` - Tidy fixed effects regression results

### Data Files

#### Final Analysis Datasets
- `data/final/data_main.csv` - County-level election data with covariates (1998-2021), harmonized to 2021 county boundaries
- `data/final/cty_annual.csv` - Annual county-level data with time-varying covariates

#### Restricted Data (Not Included in Replication Files)
- `data/final/not_to_be_shared/` - Contains restricted data files:
  - `data_candidates.rds` - Candidate data from GLES with occupation codes and brownness scores
  - `data_partypress_frames.csv` - Party press framing data with issue classifications
  - `data_partypress_sentiment.csv` - Party press sentiment analysis data
  - `data_partypress.csv` - Party press data for share of press releases figure
  - `data_soep_descriptive.csv` - SOEP descriptive statistics and summary data
  - `data_soep_is.csv` - SOEP Innovation Sample data (1998-2021)
  - `data_soep_predstatus.csv` - SOEP predicted status variables from random forest models
  - `data_soep_weights.rds` - SOEP survey weights for representative analysis
  - `data_soep.csv` - Main SOEP dataset with individual-level survey responses

#### Intermediate Data
- `data/intermediate/` - Processed intermediate datasets created during the data building process:
  - `area_pop_emp.csv` - Area, population, and employment data by county
  - `brownness_districts_*.csv` - Brown employment shares by electoral district for different election years (2013, 2017, 2021)
  - `brownness_districts_all.csv` - Combined brown employment shares by electoral district across all election years
  - `county_afd_fielding.csv` - AFD candidate fielding data by county and election year
  - `county_crosswalks.csv` - Crosswalks between different county boundary definitions over time for harmonization
  - `county_isei_2015.csv` - ISEI status scores by county for 2015
  - `cty_brownness.csv` - County-level brown employment shares calculated from occupation data using multiple classification schemes (brownweights, brown1, brown0.3, brown0.5)
  - `cty_crimes.csv` - Crime statistics including anti-refugee violence incidents and crime ratios by county
  - `cty_employment_harm2021.csv` - Employment data harmonized to 2021 county boundaries including working population and manufacturing employment shares
  - `cty_gdp_pc_ardeco.csv` - GDP per capita data from ARDECO harmonized to 2021 county boundaries
  - `cty_hh_income_harm2021.csv` - Household income data harmonized to 2021 county boundaries
  - `cty_inkar_add.csv` - Additional INKAR indicators harmonized to 2021 county boundaries
  - `cty_inkar_harm2021.csv` - INKAR demographic data harmonized to 2021 county boundaries including high school graduation rates and foreign population shares
  - `cty_unemploymed_harm2021.csv` - Unemployment data harmonized to 2021 county boundaries
  - `candidate_occupations/` - Processed candidate occupation data including unique occupation matches (unique_occupation_matches.csv), sample data (matches_sample.csv), and prepared data for analysis (prepped_data.csv) (not included in public replication due to restrictions)
  - `candidates/` - Processed candidate data with brownness scores and electoral district information from GLES surveys including cleaned candidate data (gles_candidates_clean.rds) and merged candidate data (candidates_and_gles_merged.rds) (not included in public replication due to restrictions)
  - `soep/` - Processed SOEP data files (not included in public replication due to restrictions)
  - `soep_remote/` - Results from SOEP remote analysis including voting behavior (voting.csv), political attitudes (attitudes.csv), attitudes excluding migration (attitudes_1999_woimmig.csv), status analysis (status_pooled.csv, status_pooled_r2.csv), and supporting data files (weights.txt, isco_correspondence.txt, brownness_isco08.txt, brownness_isco88.txt, cty_brownness_1999.txt, cty_brownness_covars.txt, isei_2015.txt) (not included in public replication due to restrictions)

  
#### Raw Data
- `data/raw/` - Raw data files:
  - `candidates/` - GLES candidate study datasets (2013, 2017, 2021) with occupation codes and candidate characteristics, including ZA5716 (2013), ZA6814 (2017), and ZA7704 (2021) datasets (not included in public replication due to restrictions)
  - `candidate_occupations/` - Candidate occupation data including KldB occupational classifications (kldb_for_embedding.csv), German dictionary files (german_utf8.dic), historical candidate data (candidates_all_80_21.RDS), and crosswalks between KLDB2020 and ISCO-08 classifications (not included in public replication due to restrictions)
  - `county/` - County-level administrative and economic data including employment statistics (cty_employment.csv), GDP per capita (gdp_pc_krs.rds), unemployment rates (cty_unemployment.csv), INKAR indicators (inkar_KRE_wide.rds), Südekum alternative treatment data (co2regions.xlsx), municipal and federal election results from GERDA (municipal_harm.csv, federal_cty_harm.csv), and Volkswirtschaftliche Gesamtrechnungen der Länder economic indicators (vgrdl_r2b2_bs2022.xlsx, vgrdl_r2b3_bs2022.xlsx)
  - `crosswalks/` - Geographic and occupational crosswalks including county boundary changes (ref-kreise-umrech-2021-1990-2020.xlsx), ISCO-08 to ISCO-88 conversions (346634_SvB_ISCO-08_2012-2022.xlsx), KldB88 occupational classifications for different time periods (1996-1998, 1999-2010), and county crosswalks (04_KreiseVorjahr.xlsx)
  - `occupations/` - Occupation classification data with brownness/greenness scores from Vona et al. (2018) (Volna_ISCOCodes.csv, brownness_isco88.csv), plus correspondence tables between ISCO08 and ISCO88
  - `partypress/` - Party press release data from the PARTYPRESS database (not included in public replication due to restrictions) and German Political Text sentiment dictionary from Wiedmann Wich (ed8.RData)
  - `shapefiles/` - Geographic boundary files including county and state shapefiles (VG250_KRS.shp, VG250_LAN.shp), electoral district boundaries for different election years (btw13, btw17, btw21), city coordinates (cities_de.csv), and sociodemographic data by electoral district
  - `soep/` - SOEP survey data files (not included in public replication due to restrictions)

#### Results
- `results/` - Generated figures (PDF) and tables (LaTeX)

**Note**: The data building process transforms raw data files into intermediate and final datasets. Raw data files contain the original source data, intermediate files contain processed versions with harmonized boundaries and merged datasets, and final files contain the analysis-ready datasets used in the replication scripts.

## Quick Start Guide

### Option 1: Complete Replication (Recommended)
Run the complete replication pipeline:
```r
source("code/master_full_replication.R")
```

### Option 2: Step-by-Step Replication
1. **Check prerequisites:**
   ```r
   source("code/check_prerequisites.R")
   ```

2. **Build datasets:**
   ```r
   source("code/master_data_build.R")
   ```

3. **Run analysis:**
   ```r
   source("code/master_analysis.R")
   ```

### Option 3: Individual Scripts
Run individual scripts in the order listed in the code documentation below.

### Notes
- SOEP remote analysis requires Stata and must be run manually
- See `code/README_master_scripts.md` for detailed documentation
- All scripts include progress indicators and error checking

## Data and Code Availability Statement

### Publicly Available Data Sources

1. **County-Level Election Data**: Federal Statistical Office of Germany (Destatis) & GERDA: The German Election Database
   - Source: Official election results by county, harmonized to 2021 county boundaries
   - Access: http://www.german-elections.com/
   - Source: Heddesheimer, V., Hilbig, H., Sichart, F. et al. GERDA: The German Election Database. Sci Data 12, 618 (2025). https://doi.org/10.1038/s41597-025-04811-5
   - Cost: Free
   - Requirements: None

2. **County-Level Brown Occupations Data**: 
   - Source: 
      - Vona, Francesco, Giovanni Marin, Davide Consoli and David Popp. 2018. “Environmental Regulation and Green Skills: An Empirical Exploration.” Journal of the Association of Environmental and Resource Economists 5(4):713–753.
      - Cavallotti, E., Colantone, I., Stanig, P. and Vona, F., 2025. Green collars at the voting booth: Material interest and environmental voting.
      - German Federal Employment Agency
   - Cost: Data acquisition fee payed to German Federal Employment Agency

3. **County-Level Economic Data**: Federal Statistical Office of Germany
   - Source: 
      - 13312-01-05-4: Erwerbstätige nach Wirtschaftsbereichen - Jahresdurchschnitt - regionale Tiefe: Kreise und krfr. Städte
   - Access: https://www.regionalstatistik.de/
   - Cost: Free
   - Requirements: Registration required

4. **County-Level Demographic Data**: INKAR database
   - Source: Federal Institute for Research on Building, Urban Affairs and Spatial Development
   - Access: https://www.inkar.de/
   - Cost: Free
   - Requirements: Registration required

5. **Geographic Data**: Federal Agency for Cartography and Geodesy
   - Source: Administrative boundaries and shapefiles - 2022 Zensus
   - Access: https://www.bkg.bund.de/DE/Home/home.html
   - Cost: Free
   - Requirements: None

**Note**: For other covariate database sources, see codebooks stored in `/codebooks`.

### Restricted Data Sources

1. **SOEP (German Socio-Economic Panel)**
   - Source: German Institute for Economic Research (DIW Berlin)
   - Specific Dataset:
      - Socio-Economic Panel, Update data from 1984-2021 (SOEP-Core, v38.1, International Edition - Update), 2023, doi:10.5684/soep.core.v38.1i
      - Socio-Economic Panel, data from 1984-2021 (SOEP-Core, v38.1, Remote Edition - Update) 2023, doi:10.5684/soep.core.v38.1r
      - SOEP-Innovationssample (SOEP-IS), Daten der Jahre 1998-2021. 2023. DOI: 10.5684/soep.is.2021
   - Access: https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html
   - Cost: Free for academic research
   - Requirements: 
     - Academic affiliation required
     - Data use agreement must be signed
     - Remote access available through DIW Berlin

2. **GLES Candidate Study**
   - Source: GESIS - Leibniz Institute for the Social Sciences
   - Access: https://www.gesis.org/gles/daten-und-dokumentation#c109799
   - Cost: Free for academic research
   - Requirements:
     - Academic affiliation required
     - Data use agreement must be signed

3. **Party Press Data**
   - Source: Erfort, Cornelius, Lukas F. Stoetzer, and Heike Klüver. "The PARTYPRESS Database: A new comparative database of parties’ press releases." Research & Politics 10, no. 3 (2023): 20531680231183512.
   - Access: Contact authors for access

### Instructions for Obtaining Restricted Data

#### SOEP Data Access
1. Visit https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html
   - Socio-Economic Panel, data from 1984-2021 (SOEP-Core, v38, International Edition): 10.5684/soep.core.v38i
   - Socio-Economic Panel, data from 1984-2021 (SOEP-Core, v38, Remote Edition): 10.5684/soep.core.v38r
   - SOEP Innovation-Sample (SOEP-IS), data from 1998-2021: 10.5684/soep.is.2021
2. Apply for access and remote access (or visit DIW in person).
3. Download SOEP v38.1 & SOEP-IS v38.1 and save in `data/raw/soep`.
4. Run scripts in `code/build/soep`.
5. Run Stata scripts in `code/analyze/soep_remote/` directory via remote email access.
6. Results are sent back via email. Parse results using `code/analyze/soep_remote/results_parser.R`.

#### GLES Data Access
1. Visit GLES Candidate Study websites
   - 2013: GLES (2014). Candidate Campaign Survey 2013, Survey and Electoral/Structural Data (GLES) (ZA5716; Version 3.0.0) [Data set]. GESIS, Cologne. https://doi.org/10.4232/1.12043
   - 2017: GLES (2018). Candidate Campaign Survey (GLES 2017) (ZA6814; Version 3.0.0) [Data set]. GESIS, Cologne. https://doi.org/10.4232/1.13089
   - 2021: GLES (2023). GLES Candidate Study 2021 (ZA7704; Version 2.0.0) [Data set]. GESIS, Cologne. https://doi.org/10.4232/1.14100
2. Register for a GESIS account
3. Submit data access application with research proposal
4. Download approved datasets
5. Process using the provided R scripts in `code/build/candidates`.

## Computational Requirements

### Computations were conducted using two computers:

- Apple MacBook Pro M2 (2023)
   - 16 GB
   - macOS: 15.5 (24F74)
   - R version 4.4.0 (2024-04-24) -- "Puppy Cup"

- Apple MacBook Pro M2 (2023)
   - 32 GB
   - macOS: 15.2
   - R version 4.4.0 (2024-04-24) -- "Puppy Cup"

### R Packages (with versions)
```r
# Core packages
data.table     (1.17.6)
fixest         (0.12.1)
kableExtra     (1.4.0)
pacman         (0.5.1)
tidyverse      (2.0.0)

# Additional packages
arvig              (17.12.0)
CBPS               (0.23)
conflicted         (1.2.0)
foreign            (0.8.87)
furrr              (0.3.1)
future             (1.58.0)
ggrepel            (0.9.6)
H onestDiD         (0.2.6)
haven              (2.5.4)
ISCO08ConveRsions  (0.2.0)
janitor            (2.2.0)
MatchIt            (4.5.5)
pbapply            (1.7.2)
quanteda           (4.3.0)
ranger             (0.17.0)
readxl             (1.4.3)
sf                 (1.0.17)
stringdist         (0.9.12)
stringi            (1.8.7)
testthat           (3.2.1.1)
tidytext           (0.4.2)
tm                 (0.7.14)
tmap               (3.3.4)
tmaptools          (3.1.1)
units              (0.8.5)
```

### Hardware Requirements
- **CPU**: Intel i5 or equivalent (4+ cores recommended)
- **RAM**: 8GB minimum, 16GB recommended
- **Storage**: 5GB free space
- **Internet**: Required for downloading packages and data

### Estimated Runtime
- **County-level analysis**: 10-15 minutes
- **SOEP analysis**: 5-10 minutes (after data access)
- **Figure generation**: 1-3 minutes per figure
- **Complete replication**: 30 minutes (excluding data access time)

## Instructions for Running the Code

### Step 1: Setup
1. Clone or download this repository
2. Set working directory to the repository root
3. **Note**: The code will automatically install required R packages on your computer. This may take several minutes on first run.
4. Install required R packages:
   ```r
   install.packages(c("tidyverse", "data.table", "fixest", "sf", "ggrepel", "kableExtra", "pacman"))
   ```

### Step 2: Data Preparation
1. Ensure all data files are in the `data/final/` directory
2. For restricted data, follow the access instructions above
3. Run data preparation scripts if needed (none required for public replication)

### Step 3: Analysis
1. **County-level analysis**: Run scripts in `code/analyze/` directory
2. **SOEP analysis**: After obtaining SOEP access, run remote scripts
3. **Generate all results**: Execute scripts in the following order:
   ```r
   # Summary statistics
   source("code/analyze/tab_1.R")
   
   # Main figures and tables
   source("code/analyze/fig_2_4_E1_E2_E3.R")
   source("code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R")
   
   # SOEP analysis (requires data access)
   source("code/analyze/fig_6_G1_tab_G1_G3_G4_G5.R")
   source("code/analyze/fig_7_tab_G9_G10.R")
   
   # Additional analyses
   source("code/analyze/fig_G2_G3_G4.R")
   source("code/analyze/fig_G5.R")
   source("code/analyze/tab_G6.R")
   source("code/analyze/tab_G7_G8.R")
   ```

### Step 4: Results
- Generated figures are saved as PDF files in `results/`
- Generated tables are saved as LaTeX files in `results/`

## Notes for Tables and Figures

### Main Text
- **Table 1**: `code/analyze/tab_1.R` - Summary statistics for county-level variables (2013)
- **Figure 2**: `code/analyze/fig_2_4_E1_E2_E3.R` - Map of brown employment share
- **Figure 3**: `code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R` - Main regression results
- **Figure 4**: `code/analyze/fig_2_4_E1_E2_E3.R` - Scatter plot of brown employment and AfD support
- **Figure 5**: `code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R` - Event study results
- **Figure 6**: `code/analyze/fig_6_G1_tab_G1_G3_G4_G5.R` - SOEP individual-level analysis
- **Figure 7**: `code/analyze/fig_7_tab_G9_G10.R` - Additional robustness checks

### Appendix
- **Figures A.1-A.3, B.1-B.2**: `code/analyze/fig_A1_A2_A3_B1_B2.R` - Press release analysis
- **Figure C.1**: `code/analyze/fig_C1.R` - Far-right parties over time
- **Figure D.1**: `code/analyze/fig_D1.R` - Differences over time
- **Figures E.1-E.3**: `code/analyze/fig_2_4_E1_E2_E3.R` - Additional maps and scatter plots
- **Figures F.1-F.14**: `code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R` - Additional regression results
- **Figures G.1-G.5**: Various SOEP analysis scripts
- **Tables E.1-E.2**: `code/analyze/tab_E1.R`, `code/analyze/tab_E2.R` - Brown occupations and descriptive statistics
- **Tables F.1-F.8**: Various regression tables
- **Tables G.1-G.10**: SOEP analysis tables

## Random Seeds

Seeds are used when applying random forrest models to predict the status variable in the SOEP (`code/build/soep/03_soep_status_predict.R`). We use `seed = 123`.

### Contact Information
For questions about the replication materials, please contact:
- **Vincent Heddesheimer**: [vincent.heddesheimer@princeton.edu](mailto:vincent.heddesheimer@princeton.edu)
- **Hanno Hilbig**: [hhilbig@ucdavis.edu](mailto:hhilbig@ucdavis.edu)
- **Erik Voeten**: [ev42@georgetown.edu](mailto:ev42@georgetown.edu)

## Citation

If you use this replication material, please cite:
Heddesheimer, V., Hilbig, H., & Voeten, E. (2025). The Green Transition and Political Polarization Along Occupational Lines. American Political Science Review.

## License

This replication package is licensed under the MIT License. See the LICENSE file for details.