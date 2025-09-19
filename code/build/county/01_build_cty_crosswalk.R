# =============================================================================
# County Level Crosswalk Construction
# Vincent Heddesheimer
#
# This script creates county-level crosswalks from 1990-2021, harmonizing
# county boundaries to the 2021 administrative structure. It processes
# crosswalk data from Excel files and creates datasets for area, population,
# and employment data at the county level.
# =============================================================================

# Remove scientific notation (important for preserving correct county codes)
options(scipen = 999)

rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, readxl, data.table)

# =============================================================================
# CROSSWALK DATA PROCESSING
# =============================================================================

# Define column names for different time periods
# Years 1990-1996: No employment data available
names_90to96 <- c("county_code", "county_name", "area_cw", "pop_cw", 
                   "area", "population", "county_code_21", "county_name_21")

# Years 1997-2020: Employment data included
names_97to20 <- c("county_code", "county_name", "area_cw", "pop_cw", "emp_cw", 
                   "area", "population", "employees", "county_code_21", "county_name_21")

# Specify the Excel file path containing crosswalk data
excel_file <- "data/raw/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx"

# Read all sheets from the Excel file into a list of dataframes
# Each sheet corresponds to a year from 1990 to 2020
cw_list <- excel_sheets(excel_file) %>%
  map(~ read_excel(excel_file, sheet = .)) %>%
  map2(1990:2020, ~ {
    # Assign appropriate column names based on year
    colnames(.x) <- if (.y < 1997) {
      names_90to96
    } else {
      names_97to20
    }
    .x$year <- .y
    .x
  })

# Combine all dataframes into one comprehensive crosswalk dataset
cw_combined <- bind_rows(cw_list) |>
  relocate(year, .after = county_name) |>
  arrange(county_code, year)

# =============================================================================
# COUNTY CODE STANDARDIZATION
# =============================================================================

# Transform county codes to standard format
# Remove trailing zeros and pad to 5-digit format
cw_combined <- cw_combined |>
  mutate(
    # Remove last three digits from county_code (administrative level suffix)
    county_code = str_remove(county_code, "\\d{3}$"),
    # Pad with 0 if only 4 digits long to ensure 5-digit format
    county_code = str_pad(county_code, width = 5, side = "left", pad = "0"),
    # Apply same transformation to county_code_21 (2021 boundaries)
    county_code_21 = str_remove(county_code_21, "\\d{3}$"),
    county_code_21 = str_pad(county_code_21, width = 5, side = "left", pad = "0")
  )

# =============================================================================
# SAVE CROSSWALK DATASET
# =============================================================================

# Write comprehensive crosswalk dataset to CSV
fwrite(cw_combined, "data/intermediate/county_crosswalks.csv")

# =============================================================================
# CREATE COVARIATE DATASET
# =============================================================================

# Filter crosswalk data to create covariate dataset
# Focus on German election years since 1990
cw <- cw_combined

# -----------------------------------------------------------------------------
# Harmonize to 2021 county boundaries
# -----------------------------------------------------------------------------

# Aggregate data to 2021 county boundaries using crosswalk weights
cw <- cw |>
  group_by(county_code_21, county_name_21, year) |>
  summarise(
    area = sum(area, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    employees = sum(employees, na.rm = TRUE)
  ) |>
  ungroup() |>
  # Calculate population density (population per km²)
  mutate(pop_density = population * 1000 / area) |>
  # Set employment to NA if zero (indicates missing data)
  mutate(employees = ifelse(employees == 0, NA, employees))

# Verify no duplicate county-year combinations
cw |>
  count(county_code_21, year) |>
  filter(n > 1)
# No duplicates found

# Check number of counties per year
cw |> count(year)
# Consistent 400 counties per year

# -----------------------------------------------------------------------------
# Add 2021 data from separate source
# -----------------------------------------------------------------------------

# Load 2021 county data from different Excel file
# This file contains current county boundaries and population data
cw21 <- read_excel(path = "data/raw/crosswalks/04_KreiseVorjahr.xlsx", sheet = 2) |>
  select(
    county_code_21 = `Kreisfreie Städte und Landkreise nach Fläche, Bevölkerung und Bevölkerungsdichte`,
    area = `...5`,
    population = `...6`
  ) |>
  mutate(year = 2021) |>
  # Remove header rows
  slice(-c(1:7)) |>
  # Filter for valid county codes (5 digits)
  filter(!is.na(county_code_21) & nchar(county_code_21) == 5) |>
  select(county_code_21, year, area, population) |>
  mutate(
    # Convert population from thousands to actual numbers
    population = as.numeric(population) / 1000,
    area = as.numeric(area),
    population = as.numeric(population),
    # Calculate population density
    pop_density = population * 1000 / area
  )

# -----------------------------------------------------------------------------
# Combine datasets and finalize
# -----------------------------------------------------------------------------

# Bind historical data with 2021 data
cw <- cw |>
  bind_rows(cw21) |>
  arrange(county_code_21, year)

# Final verification: Check for duplicates
cw |>
  count(county_code_21, year) |>
  filter(n > 1)
# No duplicates found

# Display number of counties per year for verification
cw |> 
  count(year) |>
  print(n = 32)

# =============================================================================
# SAVE FINAL COVARIATE DATASET
# =============================================================================

# Write final covariate dataset to CSV
fwrite(cw, "data/intermediate/area_pop_emp.csv")

# =============================================================================
# END OF SCRIPT
# =============================================================================