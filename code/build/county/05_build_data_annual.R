## Annual County Data Preparation Script
## This script creates annual county-level datasets by merging various socio-economic
## variables including employment, demographics, economic indicators, and brownness
## measures for analysis of the green transition effects at the county level.

# switch off scientific notation
options(scipen = 999)

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, janitor)

# Load covariate datasets for county-level analysis ----------------------

# Area, population, and employment data
area_pop_emp_cty <- fread("data/intermediate/area_pop_emp.csv") |>
  select(county = county_code_21, year:pop_density)

# Brown employment measures
brownness <- fread("data/intermediate/cty_brownness.csv") |>
  select(county = county_code, year:crosswalked)

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

# Merge all datasets into comprehensive county dataframe -----------------

df <- area_pop_emp_cty |>
  left_join(brownness, by = c("county", "year")) |>
  left_join(manu, by = c("county", "year")) |>
  left_join(un, by = c("county", "year")) |>
  left_join(hh_inc, by = c("county", "year")) |>
  left_join(inkar, by = c("county", "year")) |>
  left_join(inkar_add, by = c("county", "year")) |>
  left_join(ardeco, by = c("county", "year")) |>
  left_join(cty_crimes, by = "county") |>
  left_join(suedekum, by = c("county" = "AGS")) |>
  left_join(isei_2015, by = c("county" = "county"))

# Create additional derived variables --------------------------------------

# Calculate unemployment rate from employment and unemployment counts
df <- df |> mutate(unemp_rate_regstat = unemployed_regstat / (working_population_regstat + unemployed_regstat))

# Create state and regional variables from county codes
df <- df |>
  mutate(
    state = substr(str_pad(county, 5, side = "left", pad = "0"), 1, 2),
    # Map state codes to state names for analysis
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
  # relocate state, state_name, regbez after election_year
  relocate(state, state_name, regbez, .after = year)

# Create post-treatment dummy variables for different time periods
df <- df %>%
  mutate(
    post_2015 = if_else(year > 2015, 1, 0),
    post_2019 = year > 2019)

# Calculate brownness shares as proportions of total employment
df <- df |>
  mutate(
    brownness_share = brownweights / total_employees,
    # brownesss_area_share = brownweights_area / total_area,
    brown1_share = brown1 / total_employees,
    # brown1_area_share = brown1_area / total_area,
    brown0.3_share = brown0.3 / total_employees,
    # brown0.3_area_share = brown0.3_area / total_area,
    brown0.5_share = brown0.5 / total_employees,
    # brown0.5_area_share = brown0.5_area / total_area,
  )

# Clean variable names for consistency
df <- clean_names(df)

# Save final annual county dataset --------------------------------------

fwrite(df, "data/final/cty_annual.csv")

### END