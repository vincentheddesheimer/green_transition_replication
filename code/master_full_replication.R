# =============================================================================
# Master Full Replication Script
# Vincent Heddesheimer
#
# This script runs the complete replication pipeline:
# 1. Data building (all datasets)
# 2. Analysis (all figures and tables)
#
# This is the main script to run for complete replication.
#
# Note: We cannot share the raw data for the SOEP, Partypress, and Candidates analysis. Replicators need to acquire this data themselves from the original sources:
# - **SOEP data**: German Socio-Economic Panel (DIW Berlin)
# - **Partypress data**: Party press releases and media analysis (Erfort et al. (2023))
# - **Candidates data**: German Longitudinal Election Study (GLES)
#
# However, we provide all code for creating the final datasets that produce all results. All scripts using this proprietary data contain information about where the data should be stored once acquired. To ensure that the master files run without producing errors, we have commented out the code that uses this data as inputs.
#
# **For complete replication:** Once you have acquired the proprietary data and placed it in the correct directories, you can uncomment the relevant sections in the build scripts to generate the full datasets.
# =============================================================================

# Clear workspace
rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, sf, ggrepel, kableExtra, fixest)

# Set working directory to project root
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")


# =============================================================================
# PHASE 1: DATA BUILDING
# =============================================================================

cat("\nPHASE 1: DATA BUILDING\n")
cat(strrep("-", 40), "\n")

# Source the data building master script
source("code/master_data_build.R")

cat("\nData building completed successfully!\n")

# =============================================================================
# PHASE 2: ANALYSIS
# =============================================================================

cat("\nPHASE 2: ANALYSIS\n")
cat(strrep("-", 40), "\n")

# Source the analysis master script
source("code/master_analysis.R")

cat("\nAnalysis completed successfully!\n")

# =============================================================================
# PHASE 3: SOEP REMOTE ANALYSIS (STATA)
# =============================================================================

cat("\nPHASE 3: SOEP REMOTE ANALYSIS (STATA)\n")
cat(strrep("-", 40), "\n")

cat("Note: SOEP remote analysis requires Stata and must be run separately.\n")
cat("The following Stata scripts need to be executed manually:\n\n")

cat("SOEP Remote Analysis Scripts (in code/analyze/soep_remote/):\n")
cat("  01_upload_cty_brownness.do\n")
cat("  02_upload_isco08_brownness.do\n")
cat("  03_upload_isco88_brownness.do\n")
cat("  04_upload_weights.do\n")
cat("  05_prepare_soep.do\n")
cat("  06_transform_soep.do\n")
cat("  07_merge_dfs.do\n")
cat("  08_voting.do\n")
cat("  09_attitudes.do\n")
cat("  10_upload_cty_brownness_1999.do\n")
cat("  11_merge_w_1999.do\n")
cat("  12_attitudes_1999_womig.do\n")
cat("  13_prepapre_soep_is.do\n")
cat("  14_transform_soep_is.do\n")
cat("  15_merge_dfs_is.do\n")
cat("  16_status.do\n")
cat("  17_status_pooled.do\n")
cat("  18_status_pooled_table.do\n\n")

cat("These scripts should be run in numerical order.\n")

# =============================================================================
# COMPLETION SUMMARY
# =============

# Summary of outputs
cat("\nOUTPUT SUMMARY:\n")
cat(strrep("-", 20), "\n")

# Data outputs
cat("\nData files created:\n")
final_files <- list.files("data/final", pattern = "\\.(csv|rds)$", full.names = FALSE)
for (file in final_files) {
  cat("  - ", file, "\n")
}

# Results outputs
cat("\nResults files created:\n")
result_files <- list.files("results", pattern = "\\.(pdf|tex)$", full.names = FALSE)
for (file in result_files) {
  cat("  - ", file, "\n")
}

cat("\n" %+% strrep("=", 60) %+% "\n")
cat("REPLICATION COMPLETE!\n")
cat("Note: SOEP remote analysis (Stata scripts) must be run separately.\n")
cat(strrep("=", 60) %+% "\n") 