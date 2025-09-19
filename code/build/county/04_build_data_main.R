# =============================================================================
# Create Main County Dataset
# Vincent Heddesheimer
#
# This script merges all county-level datasets into the main analysis dataset.
# It combines election data, brownness scores, economic covariates, demographic
# variables, and other county-level indicators into a single comprehensive dataset
# for the main econometric analysis.
# =============================================================================

rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, data.table, janitor, readxl)

# =============================================================================
# DATA LOADING
# =============================================================================

# Load baseline election dataset
df <- fread("data/raw/county/federal_cty_harm.csv") |>
  rename(county = county_code) |>
  select(-c(area, population))

# Load brownness data (employment in brown occupations)
brownness <- fread("data/intermediate/cty_brownness.csv")

# Load covariate datasets
# Area, population, and employment data
area_pop_emp_cty <- fread("data/intermediate/area_pop_emp.csv") |>
  select(county = county_code_21, year:pop_density)

# Manufacturing employment data
manu <- fread("data/intermediate/cty_employment_harm2021.csv")

# Unemployment data
un <- fread("data/intermediate/cty_unemploymed_harm2021.csv")

# Household income data
hh_inc <- fread("data/intermediate/cty_hh_income_harm2021.csv")

# INKAR data (Indikatoren und Karten zur Raum- und Stadtentwicklung)
inkar <- fread("data/intermediate/cty_inkar_harm2021.csv")

# Additional INKAR data
inkar_add <- fread("data/intermediate/cty_inkar_add.csv")

# GDP per capita data (ARDECO)
ardeco <- fread("data/intermediate/cty_gdp_pc_ardeco.csv")

# Crime and anti-refugee violence data
cty_crimes <- fread("data/intermediate/cty_crimes.csv")

# Suedekum CO2 emissions data
suedekum <- readxl::read_xlsx("data/raw/county/co2regions.xlsx") |>
  select(-1, -3) |>
  # Clean column names by removing parenthetical references
  rename_with(~ gsub("\\(.*\\)", "", .x)) |>
  select(AGS, `Anteil Beschäftige in Sektoren mit CO2-FTE Anstieg `)

# Load ISEI status data from SOEP remote analysis
isei_2015 <- fread("data/intermediate/county_isei_2015.csv")

# Load dialect data for peripherality controls
dialect <- read_rds("data/raw/county/zhb_data_main.rds") %>%
  dplyr::select(ags_2017, hannover_sim) %>%
  dplyr::rename(county = ags_2017) %>%
  dplyr::rename(zhb_dialectal_distance = hannover_sim) %>%
  # Invert dialectal distance for interpretation
  mutate(zhb_dialectal_distance = -1 * zhb_dialectal_distance) %>%
  mutate(county = as.numeric(county))

# Load local AFD election data
county_afd <- fread("data/intermediate/county_afd_fielding.csv")

# =============================================================================
# DATA PREPARATION
# =============================================================================

# Transform INKAR data: use 2020 data for 2021 (no 2021 data available yet)
inkar <- inkar |>
  filter(year != 2021) |>
  mutate(year = ifelse(year == 2020, 2021, year))

# Impute missing values for 2021 using values from 2020 for additional INKAR data
inkar_add <- inkar_add %>%
  filter(year != 2021) |>
  mutate(year = ifelse(year == 2020, 2021, year))

# =============================================================================
# MERGE ALL DATASETS
# =============================================================================

# Merge all datasets by county and election year
df <- df |>
  # Merge brownness data
  left_join(brownness, by = c("county" = "county_code", "election_year" = "year")) |>
  # Merge area, population, and employment data
  left_join(area_pop_emp_cty, by = c("county", "election_year" = "year")) |>
  # Merge manufacturing employment data
  left_join(manu, by = c("county", "election_year" = "year")) |>
  # Merge unemployment data
  left_join(un, by = c("county", "election_year" = "year")) |>
  # Merge household income data
  left_join(hh_inc, by = c("county", "election_year" = "year")) |>
  # Merge INKAR data
  left_join(inkar, by = c("county", "election_year" = "year")) |>
  # Merge additional INKAR data
  left_join(inkar_add, by = c("county", "election_year" = "year")) |>
  # Merge GDP per capita data
  left_join(ardeco, by = c("county", "election_year" = "year")) |>
  # Merge crime data (time-invariant, so only by county)
  left_join(cty_crimes, by = "county") |>
  # Merge CO2 emissions data
  left_join(suedekum, by = c("county" = "AGS")) |>
  # Merge ISEI status data
  left_join(isei_2015, by = c("county" = "county")) |>
  # Merge dialect data
  left_join(dialect, by = "county") |>
  # Merge local AFD election data
  left_join(county_afd, by = c("county", "election_year"))

# =============================================================================
# CREATE ADDITIONAL VARIABLES
# =============================================================================

# Calculate unemployment rate
df <- df |>
  mutate(unemp_rate_regstat = unemployed_regstat / (working_population_regstat + unemployed_regstat))

# Create state and regional variables
# State variable: first two digits of county code
df <- df |>
  mutate(
    state = substr(str_pad(county, 5, side = "left", pad = "0"), 1, 2),
    # Map state codes to state names
    state_name = recode(state,
      `01` = "Schleswig-Holstein",
      `02` = "Hamburg",
      `03` = "Niedersachsen",
      `04` = "Bremen",
      `05` = "North Rhine-Westphalia",
      `06` = "Hesse",
      `07` = "Rhineland-Palatinate",
      `08` = "Baden-Württemberg",
      `09` = "Bavaria",
      `10` = "Saarland",
      `11` = "Berlin",
      `12` = "Brandenburg",
      `13` = "Mecklenburg-Vorpommern",
      `14` = "Saxony",
      `15` = "Saxony-Anhalt",
      `16` = "Thuringia"
    ),
    # Regional district: first three digits of county code
    regbez = substr(str_pad(county, 5, side = "left", pad = "0"), 1, 3)
  ) |>
  # Reorganize columns for better readability
  relocate(state, state_name, regbez, .after = election_year)

# Create post-treatment dummies
df <- df %>%
  mutate(
    post_2015 = if_else(election_year > 2015, 1, 0),
    post_2019 = election_year > 2019
  )

# =============================================================================
# CREATE BROWNNESS SHARE VARIABLES
# =============================================================================

# Calculate brownness shares (proportion of employees in brown occupations)
df <- df |>
  mutate(
    brownness_share = brownweights / total_employees,
    brown1_share = brown1 / total_employees,
    brown0.3_share = brown0.3 / total_employees,
    brown0.5_share = brown0.5 / total_employees
  )

# =============================================================================
# CLEAN DATASET
# =============================================================================

# Remove vote data for uninteresting parties and redundant variables
df <- df |>
  select(-c(
    eligible_voters:invalid_votes,
    cdu, csu, npd:total_votes,
    far_left:far_left_w_linke,
    employees # Keep brownness variables instead of crosswalked employees
  ))

# Clean variable names using janitor package
df <- clean_names(df)

# =============================================================================
# SAVE FINAL DATASET
# =============================================================================

# Write final merged dataset
fwrite(df, "data/final/data_main.csv")

# Verify that there are no duplicates
print(df %>%
  group_by(county, election_year) %>%
  summarise(n = n()) %>%
  filter(n > 1))
# No duplicates found

# N counties per year
print(df %>%
  group_by(election_year) %>%
  summarise(n = n_distinct(county)))

# =============================================================================
# END OF SCRIPT
# =============================================================================
