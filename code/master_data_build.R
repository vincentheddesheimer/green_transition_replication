# =============================================================================
# Master Data Building Script
#
# This script runs all data preparation and building scripts in the correct order
# to create the final datasets used in the analysis.
#
# Execution order:
# 1. County data preparation
# 2. Candidates data preparation
# 3. Partypress data preparation
# 4. SOEP data preparation
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
pacman::p_load(tidyverse, data.table)

# Set working directory to project root
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# =============================================================================
# 1. COUNTY DATA PREPARATION
# =============================================================================

cat("Starting county data preparation...\n")

# Build county crosswalks
source("code/build/county/01_build_cty_crosswalk.R")
cat("✓ County crosswalks completed\n")

# Build county brownness scores
source("code/build/county/02_build_cty_brownness.R")
cat("✓ County brownness scores completed\n")

# Build county covariates
source("code/build/county/03_build_covars_data.R")
cat("✓ County covariates completed\n")

# Build main county dataset
source("code/build/county/04_build_data_main.R")
cat("✓ Main county dataset completed\n")

# Build annual county dataset
source("code/build/county/05_build_data_annual.R")
cat("✓ Annual county dataset completed\n")

# Run sanity checks
source("code/build/county/06_sanity_checks.R")
cat("✓ County data sanity checks completed\n")

# # =============================================================================
# # 2. CANDIDATES DATA PREPARATION
# # =============================================================================

# cat("\nStarting candidates data preparation...\n")

# # Build candidates dataset
# source("code/build/candidates/01_build_candidates.R")
# cat("✓ Candidates dataset completed\n")

# # Add candidate occupations
# source("code/build/candidates/02_add_candidate_occupations.R")
# cat("✓ Candidate occupations completed\n")

# # Add brownness to candidates
# source("code/build/candidates/03_add_brownness.R")
# cat("✓ Candidate brownness completed\n")

# # =============================================================================
# # 3. PARTYPRESS DATA PREPARATION
# # =============================================================================

# cat("\nStarting partypress data preparation...\n")

# # Build partypress dataset
# source("code/build/partypress/01_build_partypress.R")
# cat("✓ Partypress dataset completed\n")

# # Build partypress sentiment
# source("code/build/partypress/02_build_partypress_sentiment.R")
# cat("✓ Partypress sentiment completed\n")

# # Build partypress frames
# source("code/build/partypress/03_build_partypress_frames.R")
# cat("✓ Partypress frames completed\n")

# # =============================================================================
# # 4. SOEP DATA PREPARATION
# # =============================================================================

# cat("\nStarting SOEP data preparation...\n")

# # SOEP Remote data preparation
# cat("  Preparing SOEP remote data...\n")
# source("code/build/soep/soep_remote/01_prepare_soep_remote.R")
# cat("  ✓ SOEP remote preparation completed\n")

# source("code/build/soep/soep_remote/02_results_parser.R")
# cat("  ✓ SOEP results parsing completed\n")

# source("code/build/soep/soep_remote/03_prepare_results_dfs.R")
# cat("  ✓ SOEP results dataframes completed\n")

# source("code/build/soep/soep_remote/04_crosstab_isei.do")
# cat("  ✓ SOEP ISEI crosstab completed\n")

# # source("code/build/soep/soep_remote/05_crosstab_isei_parser.R")
# # cat("  ✓ SOEP ISEI parser completed\n")

# # SOEP main data preparation
# cat("  Preparing SOEP main data...\n")
# source("code/build/soep/soep/01_soep_cleanup.R")
# cat("  ✓ SOEP cleanup completed\n")

# source("code/build/soep/soep/02_soep_generate_weights.R")
# cat("  ✓ SOEP weights generation completed\n")

# # SOEP IS data preparation (if directory exists)
# cat("  Preparing SOEP IS data...\n")
# source("code/build/soep/soep_is/01_soep_is_cleanup.R")
# cat("  ✓ SOEP IS preparation completed\n")

# # SOEP status prediction: need to run this after the SOEP IS data is built
# source("code/build/soep/soep/03_soep_status_predict.R")
# cat("  ✓ SOEP status prediction completed\n")

# source("code/build/soep/soep/04_soep_build_descriptive_df.R")
# cat("  ✓ SOEP descriptive dataframe completed\n")

# =============================================================================
# COMPLETION MESSAGE
# =============================================================================

cat("\n" %+% strrep("=", 60) %+% "\n")
cat("ALL DATA BUILDING SCRIPTS COMPLETED SUCCESSFULLY!\n")
cat("Final datasets are available in data/final/\n")
cat(strrep("=", 60) %+% "\n")

# List final datasets
cat("\nFinal datasets created:\n")
final_files <- list.files("data/final", pattern = "\\.(csv|rds)$", full.names = FALSE)
for (file in final_files) {
    cat("  - ", file, "\n")
}
