# =============================================================================
# Prerequisites Check Script
#
# This script checks if all required files and packages are available
# before running the master replication scripts.
# =============================================================================

# Clear workspace
rm(list = ls())

# Set working directory to project root
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

cat("Checking prerequisites for Green Transition Replication...\n")
cat(strrep("=", 50), "\n")

# =============================================================================
# 1. CHECK REQUIRED R PACKAGES
# =============================================================================

cat("\n1. Checking required R packages...\n")

required_packages <- c("tidyverse", "data.table", "sf", "ggrepel", "kableExtra", "fixest", "pacman")

missing_packages <- c()
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("  ❌ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("  Install with: install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n")
} else {
  cat("  ✓ All required packages are installed\n")
}

# =============================================================================
# 2. CHECK REQUIRED DATA FILES
# =============================================================================

cat("\n2. Checking required data files...\n")

# Check raw data directories
raw_dirs <- c(
  "data/raw/county",
  "data/raw/candidates",
  "data/raw/partypress",
  "data/raw/soep",
  "data/raw/crosswalks",
  "data/raw/occupations",
  "data/raw/shapefiles"
)

missing_dirs <- c()
for (dir in raw_dirs) {
  if (!dir.exists(dir)) {
    missing_dirs <- c(missing_dirs, dir)
  }
}

if (length(missing_dirs) > 0) {
  cat("  ❌ Missing directories:", paste(missing_dirs, collapse = ", "), "\n")
} else {
  cat("  ✓ All required directories exist\n")
}

# Check specific important files
important_files <- c(
  "data/raw/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx",
  "data/raw/county/cty_employment.csv",
  "data/raw/shapefiles/VG250_KRS.shp",
  "data/raw/shapefiles/VG250_LAN.shp"
)

missing_files <- c()
for (file in important_files) {
  if (!file.exists(file)) {
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("  ❌ Missing important files:", paste(missing_files, collapse = ", "), "\n")
} else {
  cat("  ✓ All important files exist\n")
}

# =============================================================================
# 3. CHECK SCRIPT FILES
# =============================================================================

cat("\n3. Checking script files...\n")

# Check master scripts
master_scripts <- c(
  "code/master_data_build.R",
  "code/master_analysis.R",
  "code/master_full_replication.R"
)

missing_scripts <- c()
for (script in master_scripts) {
  if (!file.exists(script)) {
    missing_scripts <- c(missing_scripts, script)
  }
}

if (length(missing_scripts) > 0) {
  cat("  ❌ Missing master scripts:", paste(missing_scripts, collapse = ", "), "\n")
} else {
  cat("  ✓ All master scripts exist\n")
}

# Check key build scripts
key_build_scripts <- c(
  "code/build/county/01_build_cty_crosswalk.R",
  "code/build/county/02_build_cty_brownness.R",
  "code/build/county/03_build_covars_data.R",
  "code/build/county/04_build_data_main.R"
)

missing_build_scripts <- c()
for (script in key_build_scripts) {
  if (!file.exists(script)) {
    missing_build_scripts <- c(missing_build_scripts, script)
  }
}

if (length(missing_build_scripts) > 0) {
  cat("  ❌ Missing key build scripts:", paste(missing_build_scripts, collapse = ", "), "\n")
} else {
  cat("  ✓ All key build scripts exist\n")
}

# =============================================================================
# 4. SUMMARY
# =============================================================================

cat("\n" %+% strrep("=", 50), "\n")
cat("PREREQUISITES CHECK SUMMARY\n")
cat(strrep("=", 50), "\n")

total_issues <- length(missing_packages) + length(missing_dirs) + 
                length(missing_files) + length(missing_scripts) + 
                length(missing_build_scripts)

if (total_issues == 0) {
  cat("✅ ALL PREREQUISITES MET!\n")
  cat("You can now run the master scripts:\n")
  cat("  - source('code/master_data_build.R')     # Data building only\n")
  cat("  - source('code/master_analysis.R')       # Analysis only\n")
  cat("  - source('code/master_full_replication.R') # Complete replication\n")
} else {
  cat("❌ ISSUES FOUND:", total_issues, "\n")
  cat("Please resolve the issues above before running the master scripts.\n")
}

cat("\nNote: SOEP remote analysis requires Stata and SOEP data files.\n")
cat("These are not checked by this script.\n") 