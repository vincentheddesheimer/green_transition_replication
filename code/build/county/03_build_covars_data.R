# =============================================================================
# County Level Covariate Creation
# Vincent Heddesheimer
#
# This script creates various county-level covariates by processing and harmonizing
# data from multiple sources including employment, unemployment, income, crime,
# demographic statistics, and local AFD election data.
# All data is harmonized to 2021 county boundaries.
# =============================================================================

rm(list = ls())

# Load required packages
# Package for crime data
# devtools::install_github("davben/arvig")
pacman::p_load(tidyverse, data.table, readxl, arvig, readxl)

# =============================================================================
# LOAD CROSSWALK DATA
# =============================================================================

# Load county crosswalks for harmonization to 2021 boundaries
cw <- fread("data/intermediate/county_crosswalks.csv")

# =============================================================================
# MANUFACTURING EMPLOYMENT DATA
# =============================================================================

# Load employment data from CSV file
df <- fread("data/raw/county/cty_employment.csv") |>
  slice(-c(1:7)) |>
  select(c(1, 2, 4, 7))

# Assign meaningful column names
names <- c("year", "county_code", "working_population", "manufacturing")
colnames(df) <- names

# -----------------------------------------------------------------------------
# Handle special cases for city-states
# -----------------------------------------------------------------------------

# Berlin (02) and Hamburg (11) need special county code formatting
df <- df %>%
  mutate(county_code = case_when(
    county_code == "02" ~ "02000",
    county_code == "11" ~ "11000",
    T ~ county_code
  ))

# -----------------------------------------------------------------------------
# Data cleaning and harmonization
# -----------------------------------------------------------------------------

# Clean and standardize data
df <- df |>
  # Keep only counties with 5-digit codes
  filter(str_detect(county_code, "^\\d{5}$")) |>
  mutate(
    year = as.numeric(year),
    # Convert from thousands to actual numbers
    working_population = parse_number(working_population, locale = locale(decimal_mark = ",")) * 1000,
    manufacturing = parse_number(manufacturing, locale = locale(decimal_mark = ",")) * 1000,
  )

# Merge with crosswalk data for harmonization
df_merged <- cw |>
  mutate(county_code = str_pad(county_code, 5, side = "left", pad = "0")) |>
  left_join(df, by = c("county_code", "year"))

# Harmonize to 2021 county boundaries using population-weighted crosswalks
df_harm <- df_merged |>
  group_by(county_code_21, year) |>
  summarise(
    working_population = sum(working_population * pop_cw, na.rm = TRUE),
    manufacturing = sum(manufacturing * pop_cw, na.rm = TRUE),
  ) |>
  ungroup() |>
  mutate(
    # Set zero values to NA (indicates missing data)
    manufacturing = ifelse(manufacturing == 0, NA, manufacturing),
    working_population = ifelse(working_population == 0, NA, working_population),
    # Calculate manufacturing share as percentage
    share_manufacturing = manufacturing / working_population * 100
  ) |>
  select(county_code = county_code_21, year, share_manufacturing, working_population, manufacturing)

# Add 2021 data directly (already in 2021 boundaries)
df_final <- df_harm |>
  bind_rows(df |>
    filter(year == 2021) |>
    mutate(
      county_code = as.numeric(county_code),
      share_manufacturing = manufacturing / working_population * 100
    ) |>
    select(county_code, year, share_manufacturing, working_population, manufacturing)) |>
  rename(county = county_code) |>
  # Filter to years with available data
  filter(year > 2000)

# Verify data completeness
df_final |> count(year) |> print(n = Inf)

# Rename variables with suffix to avoid confusion with municipality-level data
df_final <- df_final |>
  rename(
    share_manufacturing_regstat = share_manufacturing,
    working_population_regstat = working_population,
    manufacturing_regstat = manufacturing
  )

# Save harmonized manufacturing employment data
fwrite(df_final, "data/intermediate/cty_employment_harm2021.csv")

# =============================================================================
# UNEMPLOYMENT DATA
# =============================================================================

# Load unemployment data
df <- fread("data/raw/county/cty_unemployment.csv") |>
  slice(-c(1:9)) |>
  select(c(1, 2, 4))

# Assign column names
names <- c("year", "county_code", "unemployed")
colnames(df) <- names

# -----------------------------------------------------------------------------
# Handle special cases for city-states
# -----------------------------------------------------------------------------

# Berlin (02) and Hamburg (11) need special county code formatting
df <- df %>%
  mutate(county_code = case_when(
    county_code == "02" ~ "02000",
    county_code == "11" ~ "11000",
    T ~ county_code
  ))

# -----------------------------------------------------------------------------
# Data cleaning and harmonization
# -----------------------------------------------------------------------------

# Clean and standardize data
df <- df |>
  # Keep only counties with 5-digit codes
  filter(str_detect(county_code, "^\\d{5}$")) |>
  mutate(
    year = as.numeric(year),
    unemployed = as.numeric(unemployed)
  )

# Merge with crosswalk data for harmonization
df_merged <- cw |>
  mutate(county_code = str_pad(county_code, 5, side = "left", pad = "0")) |>
  left_join(df, by = c("county_code", "year"))

# Harmonize to 2021 county boundaries using population-weighted crosswalks
df_harm <- df_merged |>
  group_by(county_code_21, year) |>
  summarise(
    unemployed = sum(unemployed * pop_cw, na.rm = TRUE),
  ) |>
  ungroup() |>
  mutate(unemployed = ifelse(unemployed == 0, NA, unemployed)) |>
  select(county_code = county_code_21, year, unemployed)

# Add 2021 data directly (already in 2021 boundaries)
df_final <- df_harm |>
  bind_rows(df |>
    filter(year == 2021) |>
    mutate(county_code = as.numeric(county_code)) |>
    select(county_code, year, unemployed)) |>
  rename(county = county_code) |>
  # Filter to years with available data
  filter(year > 2000)

# Verify data completeness
df_final |> count(year)

# Rename variables with suffix to avoid confusion with municipality-level data
df_final <- df_final |>
  rename(
    unemployed_regstat = unemployed
  )

# Save harmonized unemployment data
fwrite(df_final, "data/intermediate/cty_unemploymed_harm2021.csv")

# =============================================================================
# HOUSEHOLD INCOME DATA
# =============================================================================

# Load household income data from Excel file
# Verfügbares Einkommen der privaten Haushalte einschl. der privaten Organisationen ohne Erwerbszweck
# je Einwohnerin bzw. Einwohner in EUR
df <- read_excel("data/raw/county/vgrdl_r2b3_bs2022.xlsx", sheet = 17)

# Use fourth row as column names
colnames(df) <- as.character(df[4, ])

# Clean and reshape data
df <- df |>
  slice(-c(1:6)) |>
  select(county_code = `Regional-schlüssel`, `2000`:`2021`) |>
  # Handle special cases for city-states
  mutate(county_code = case_when(
    county_code == "02" ~ "02000",
    county_code == "11" ~ "11000",
    T ~ county_code
  )) |>
  # Keep only counties with 5-digit codes
  filter(str_detect(county_code, "^\\d{5}$")) |>
  # Reshape to long format
  pivot_longer(cols = -county_code, names_to = "year", values_to = "hh_income") |>
  mutate(
    year = as.numeric(year),
    hh_income = as.numeric(hh_income)
  ) |>
  # Sort by county and year
  arrange(county_code, year)

# Merge with crosswalk data for harmonization
df_merged <- cw |>
  mutate(county_code = str_pad(county_code, 5, side = "left", pad = "0")) |>
  left_join(df, by = c("county_code", "year"))

# Harmonize to 2021 county boundaries using population-weighted averages
df_harm <- df_merged |>
  mutate(weights = pop_cw * population) |>
  group_by(county_code_21, year) |>
  summarise(
    hh_income = weighted.mean(hh_income, weights, na.rm = TRUE),
  ) |>
  ungroup() |>
  select(county_code = county_code_21, year, hh_income)

# Add 2021 data directly (already in 2021 boundaries)
df_final <- df_harm |>
  bind_rows(df |>
    filter(year == 2021) |>
    mutate(county_code = as.numeric(county_code)) |>
    select(county_code, year, hh_income)) |>
  rename(county = county_code) |>
  # Filter to years with available data
  filter(year > 2000)

# Verify data completeness
df_final |> count(year)

# Rename variables with source suffix
df_final <- df_final |>
  rename(
    hh_income_vgrdl = hh_income
  )

# Save harmonized household income data
fwrite(df_final, "data/intermediate/cty_hh_income_harm2021.csv", na = "NA")

# =============================================================================
# INKAR DATA (MAIN DATASET)
# =============================================================================

# Load INKAR data (Indikatoren und Karten zur Raum- und Stadtentwicklung)
# This dataset contains various socio-economic indicators at the county level
df_inkar <- read_rds("data/raw/county/inkar_KRE_wide.rds") |>
  rename(county = kreis_id) |>
  mutate(
    county = as.numeric(county),
    year = as.numeric(year)
  ) |>
  select(county, year, Schulabgänger_mit_allgemeiner_Hochschulreife_inkar = Schulabgänger_mit_allgemeiner_Hochschulreife, Ausländeranteil_inkar = Ausländeranteil)

# Note: INKAR data is already harmonized to 2020 counties
# No changes occurred from 2020 to 2021, so data can be used as-is

# Save INKAR data
fwrite(df_inkar, "data/intermediate/cty_inkar_harm2021.csv")

# =============================================================================
# ADDITIONAL INKAR DATA
# =============================================================================

# Load additional INKAR data from CSV file
df_inkar_add <- fread("data/raw/county/inkar_VH_24_02_05.csv",
  header = FALSE
)

# Combine the first and second rows as column names
combined_colnames <- paste(df_inkar_add[1, ], df_inkar_add[2, ], sep = "_")
colnames(df_inkar_add) <- combined_colnames

# Remove the first two rows (which were used for column names)
df_inkar_add <- df_inkar_add[-c(1, 2), ]

# Reshape data to long format and clean
df_inkar_add <- df_inkar_add |>
  pivot_longer(cols = -c(1, 2), names_to = "year", values_to = "value") |>
  # Separate year column into variable and year
  separate(year, into = c("variable", "year"), sep = "_") |>
  filter(variable != "Aggregat") |>
  select(-Raumeinheit_) |>
  # Reshape to wide format
  pivot_wider(names_from = variable, values_from = value) |>
  select(-Bruttoverdienst) |>
  # Convert all columns to numeric: first replace points, then commas, then as.numeric
  mutate(across(
    .cols = everything(),
    .fns = ~ as.numeric(gsub(",", ".", gsub("\\.", "", .)))
  )) |>
  rename(county = Kennziffer_) |>
  select(county, year, Arbeitslosenquote_inkar = Arbeitslosenquote)

# Save additional INKAR data
fwrite(df_inkar_add, "data/intermediate/cty_inkar_add.csv")

# =============================================================================
# ARDECO DATA (GDP PER CAPITA)
# =============================================================================

# Load ARDECO data for GDP per capita at county level
df_ardeco <- read_rds("data/raw/county/gdp_pc_krs.rds") |>
  select(
    county = ags,
    year = year,
    gdp_pc_ardeco = gdp_pc
  ) |>
  mutate(county = as.numeric(county))

# Save ARDECO data
fwrite(df_ardeco, "data/intermediate/cty_gdp_pc_ardeco.csv")

# =============================================================================
# CRIME AND REFUGEE VIOLENCE DATA
# =============================================================================

# Load ARVIG data (Anti-Refugee Violence in Germany)
data("arvig")

# Process ARVIG data for 2015
arvig <- arvig |>
  mutate(year = lubridate::year(date))

# Filter to 2015 and aggregate to county level
arvig_2015 <- arvig |>
  filter(year == 2015) |>
  mutate(
    # Extract county code (first five digits of community_id)
    county = substr(community_id, 1, 5)
  ) |>
  # Count incidents by county
  count(county) |>
  rename(anti_refugee_violence_2015 = n)

# Load crime statistics data
crimes <- read_xlsx("data/raw/county/tb01_FaelleGrundtabelleKreise_excel.xlsx") |>
  select(
    type = `...2`,
    county = `...3`,
    n_crimes_2015 = `KfS = kreisfreie Stadt`
  ) |>
  slice(9:max(n())) |>
  filter(type == "Straftaten insgesamt") |>
  select(-type) |>
  mutate(n_crimes_2015 = as.numeric(n_crimes_2015))

# Merge crime and anti-refugee violence data
crimes <- crimes |>
  left_join(arvig_2015, by = "county")

# -----------------------------------------------------------------------------
# Handle county boundary changes
# -----------------------------------------------------------------------------

# Crosswalk three counties that changed boundaries
crimes <- crimes |>
  mutate(county = case_when(
    county == "03152" ~ "03159",
    county == "03156" ~ "03159",
    county == "16056" ~ "16063",
    TRUE ~ county
  ))

# Check for duplicates after crosswalking
crimes |>
  count(county) |>
  filter(n > 1)
# Yes: 2 duplicates found

# Summarize for counties to remove duplicates
crimes <- crimes |>
  group_by(county) |>
  summarise(
    n_crimes_2015 = sum(n_crimes_2015, na.rm = TRUE),
    anti_refugee_violence_2015 = sum(anti_refugee_violence_2015, na.rm = TRUE)
  )

# Calculate anti-refugee violence ratio per 1000 crimes
crimes <- crimes |>
  mutate(anti_refugee_violence_2015 = ifelse(is.na(anti_refugee_violence_2015), 0, anti_refugee_violence_2015)) |>
  mutate(anti_refugee_violence_crimeratio = anti_refugee_violence_2015 / (n_crimes_2015 / 1000))

# -----------------------------------------------------------------------------
# Calculate population-based ratios
# -----------------------------------------------------------------------------

# Load population data for 2015
area_pop_emp_cty <- fread("data/intermediate/area_pop_emp.csv") |>
  filter(year == 2015) |>
  select(county = county_code_21, population) |>
  mutate(county = str_pad(county, 5, side = "left", pad = "0"))

# Merge with crime data
crimes <- crimes |>
  left_join(area_pop_emp_cty, by = "county")

# Check for missing population data
crimes |>
  filter(is.na(population))
# None found

# Calculate anti-refugee violence ratio per 10,000 population
crimes <- crimes |>
  mutate(anti_refugee_violence_popratio = anti_refugee_violence_2015 / (population / 10)) |>
  select(-population)

# Final check for duplicates
crimes |>
  count(county) |>
  filter(n > 1)
# No duplicates

# Save crime and anti-refugee violence data
fwrite(crimes, "data/intermediate/cty_crimes.csv")


# =============================================================================
# LOCAL AFD ELECTION DATA
# =============================================================================

# Load local election data
local_elections <- fread("data/raw/county/municipal_harm.csv")

# Map local election years to federal election years
# Pre-2015 elections map to 2013, post-2015 to 2017
local_elections <- local_elections %>%
  mutate(
    federal_election_year = case_when(
      election_year <= 2015 ~ 2013,
      election_year <= 2019 ~ 2017,
      TRUE ~ 2021
    )
  )

# Create county-level AFD fielding measures
county_afd <- local_elections %>%
  # Group by county and mapped federal election year
  group_by(county, federal_election_year) %>%
  summarise(
    # Weighted average of AFD vote share
    afd_local_vote_share = weighted.mean(afd, eligible_voters, na.rm = TRUE),
    # Number of municipalities where AFD ran (non-NA values)
    afd_local_ran = sum(!is.na(afd)),
    # Total number of municipalities
    total_local_munis_cty = n(),
    # Voter-weighted proportion of municipalities where AFD ran
    afd_local_ran_prop = sum(eligible_voters * (!is.na(afd)), na.rm = TRUE) /
      sum(eligible_voters, na.rm = TRUE),
    # Total eligible voters in county
    total_local_eligible_voters_cty = sum(eligible_voters, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Rename to match the merge field
  rename(election_year = federal_election_year) |>
  arrange(county, election_year)

# Save the county-level AFD measures
fwrite(county_afd, "data/intermediate/county_afd_fielding.csv")

# =============================================================================
# END OF SCRIPT
# =============================================================================