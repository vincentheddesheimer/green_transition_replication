# =============================================================================
# Master Analysis Script
#
# This script runs all analysis scripts to generate figures and tables
# for the green transition paper.
#
# Prerequisites: Run master_data_build.R first to create all datasets
#
# Note: This file only runs the county analyses. 
# For all other analyses, you need to get access to the proprietary data.
# See README_master_scripts.md for more information.
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
# CHECK PREREQUISITES
# =============================================================================

cat("Checking prerequisites...\n")

# Check if main datasets exist
required_files <- c(
  "data/final/data_main.csv",
  "data/final/cty_annual.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cat("ERROR: Missing required datasets:\n")
  for (file in missing_files) {
    cat("  - ", file, "\n")
  }
  cat("\nPlease run master_data_build.R first to create all datasets.\n")
  stop("Missing required datasets")
}

cat("✓ All required datasets found\n")

# =============================================================================
# 1. MAIN FIGURES AND TABLES
# =============================================================================

cat("\nStarting main analysis...\n")

# Figure 2, 4 and E1-E3: Maps and scatter plots
source("code/analyze/fig_2_4_E1_E2_E3.R")
cat("✓ Figures 2, 4, E1, E2, E3 completed\n")

# Figure 3, 5 and F1-F14: Main regression results
source("code/analyze/fig_3_5_F1_F2_F3_F4_F5_F6_F8_F9_F10_F11_F12_F13_F_14_tab_F1_F2_F3_F4_F5.R")
cat("✓ Figures 3, 5, F1-F14 and Tables F1-F5 completed\n")

# # Figure 6, G1 and Tables G1, G3-G5: Additional analysis
# source("code/analyze/fig_6_G1_tab_G1_G3_G4_G5.R")
# cat("✓ Figure 6, G1 and Tables G1, G3-G5 completed\n")

# # Figure 7 and Tables G9-G10: Further analysis
# source("code/analyze/fig_7_tab_G9_G10.R")
# cat("✓ Figure 7 and Tables G9-G10 completed\n")

# Table 1: Summary statistics
source("code/analyze/tab_1.R")
cat("✓ Table 1 completed\n")

# =============================================================================
# 2. SUPPLEMENTARY FIGURES AND TABLES
# =============================================================================

cat("\nStarting supplementary analysis...\n")

# # Figures A1-A3, B1-B2: Additional maps and plots
# source("code/analyze/fig_A1_A2_A3_B1_B2.R")
# cat("✓ Figures A1-A3, B1-B2 completed\n")

# Figure C1: Additional analysis
source("code/analyze/fig_C1.R")
cat("✓ Figure C1 completed\n")

# # Figure D1: Additional analysis
# source("code/analyze/fig_D1.R")
# cat("✓ Figure D1 completed\n")

# # Tables E1-E2: Additional tables
# source("code/analyze/tab_E1.R")
# cat("✓ Table E1 completed\n")

# source("code/analyze/tab_E2.R")
# cat("✓ Table E2 completed\n")

# # Tables F6-F8: Additional tables
# source("code/analyze/tab_F6_F7_F8.R")
# cat("✓ Tables F6-F8 completed\n")

# Figure F7: Additional figure
source("code/analyze/fig_F7.R")
cat("✓ Figure F7 completed\n")

# # Figures G2-G4: Additional figures
# source("code/analyze/fig_G2_G3_G4.R")
# cat("✓ Figures G2-G4 completed\n")

# # Figure G5: Additional figure
# source("code/analyze/fig_G5.R")
# cat("✓ Figure G5 completed\n")

# # Tables G6-G8: Additional tables
# source("code/analyze/tab_G6.R")
# cat("✓ Table G6 completed\n")

# source("code/analyze/tab_G7_G8.R")
# cat("✓ Tables G7-G8 completed\n")

# =============================================================================
# 3. SOEP REMOTE ANALYSIS (STATA SCRIPTS)
# =============================================================================

cat("\nStarting SOEP remote analysis (Stata scripts)...\n")

# Note: These are Stata scripts that need to be run separately
# They are located in code/analyze/soep_remote/
cat("  Note: SOEP remote analysis requires Stata\n")
cat("  Scripts are located in code/analyze/soep_remote/\n")
cat("  Please run these manually if needed:\n")
cat("    - 01_upload_cty_brownness.do\n")
cat("    - 02_upload_isco08_brownness.do\n")
cat("    - 03_upload_isco88_brownness.do\n")
cat("    - 04_upload_weights.do\n")
cat("    - 05_prepare_soep.do\n")
cat("    - 06_transform_soep.do\n")
cat("    - 07_merge_dfs.do\n")
cat("    - 08_voting.do\n")
cat("    - 09_attitudes.do\n")
cat("    - 10_upload_cty_brownness_1999.do\n")
cat("    - 11_merge_w_1999.do\n")
cat("    - 12_attitudes_1999_womig.do\n")
cat("    - 13_prepapre_soep_is.do\n")
cat("    - 14_transform_soep_is.do\n")
cat("    - 15_merge_dfs_is.do\n")
cat("    - 16_status.do\n")
cat("    - 17_status_pooled.do\n")
cat("    - 18_status_pooled_table.do\n")
cat("  \n")
cat("  These scripts should be run in numerical order.\n")

# =============================================================================
# COMPLETION MESSAGE
# =============================================================================

cat("\n" %+% strrep("=", 60) %+% "\n")
cat("ALL ANALYSIS SCRIPTS COMPLETED SUCCESSFULLY!\n")
cat("Figures and tables are available in results/\n")
cat(strrep("=", 60) %+% "\n")

# List generated files
cat("\nGenerated files:\n")
result_files <- list.files("results", pattern = "\\.(pdf|tex)$", full.names = FALSE)
for (file in result_files) {
  cat("  - ", file, "\n")
}

cat("\nNote: SOEP remote analysis (Stata scripts) must be run separately.\n") 