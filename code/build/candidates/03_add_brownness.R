## Electoral District Brownness Mapping Script
## This script matches brown employment scores from county-level data to electoral districts
## using geographic intersection analysis. It creates district-level brownness measures
## for analyzing the relationship between brown employment and political outcomes.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, sf, tmap, tmaptools, units, ggrepel, ggplot2)

# Load data ---------------------------------------------------------------

# 1. Load brownness data at county level
brownness <- fread("data/final/cty_annual.csv") %>%
  mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
  select(county, year, total_employees, brownweights, brown1, brown0_3, brown0_5, brownness_share, brown1_share, brown0_3_share, brown0_5_share, state)

# Load area data for county boundaries
area <- fread("data/final/cty_annual.csv") %>%
  mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
  select(county, year, area)

# 2. Load shapefiles for geographic analysis
# County shapefiles (2021 boundaries)
counties <- st_read("data/raw/shapefiles/VG250_KRS.shp") %>%
  select(AGS_0, GEN) %>%
  mutate(county = substr(AGS_0, 1, 5)) %>%
  select(-AGS_0) %>%
  rename(county_name = GEN)

# Load state boundaries
land <- st_read("data/raw/shapefiles/VG250_LAN.shp") %>%
  filter(GF == 4) %>%
  select(AGS_0) %>%
  mutate(state_id = substr(AGS_0, 1, 2)) %>%
  select(-AGS_0)

# Load electoral district shapefiles for all years
wk_2013 <- st_read("data/raw/shapefiles/btw13_geometrie_wahlkreise_etrs89-vg1000_geo_shp/Geometrie_Wahlkreise_18DBT_VG1000.shp") %>%
  st_transform(25832) %>% # Transform to UTM32
  select(WKR_NR, WKR_NAME) %>%
  rename(wk_id = WKR_NR, wk_name = WKR_NAME)

wk_2017 <- st_read("data/raw/shapefiles/btw17_geometrie_wahlkreise_vg250_shp/Geometrie_Wahlkreise_19DBT_VG250.shp") %>%
  st_transform(25832) %>%
  select(WKR_NR, WKR_NAME) %>%
  rename(wk_id = WKR_NR, wk_name = WKR_NAME)

wk_2021 <- st_read("data/raw/shapefiles/btw21_geometrie_wahlkreise_vg250_shp/Geometrie_Wahlkreise_20DBT_VG250.shp") %>%
  st_transform(25832) %>%
  select(WKR_NR, WKR_NAME) %>%
  rename(wk_id = WKR_NR, wk_name = WKR_NAME)

# Function to process brownness data for electoral districts --------------

# Function to process brownness data for a given year and electoral district shapefile
process_brownness_districts <- function(wk_shapefile, election_year) {
  # Calculate intersection of counties and electoral districts
  intersection <- st_intersection(counties, wk_shapefile) %>%
    group_by(county_name, county, wk_id, wk_name) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

  # Calculate area of each intersection
  intersection$area <- st_area(intersection)

  # Calculate total area of each county
  county_areas <- counties %>%
    group_by(county_name) %>%
    summarise(geometry = st_union(geometry)) %>%
    mutate(total_area = st_area(.)) %>%
    st_drop_geometry() %>%
    select(county_name, total_area)

  # Join county areas and calculate proportion of overlap
  overlap <- intersection %>%
    left_join(county_areas, by = "county_name") %>%
    mutate(proportion = as.numeric(area / total_area)) %>%
    st_drop_geometry()

  # Aggregate brownness scores for the election year using area-weighted averages
  brownness_districts <- overlap %>%
    left_join(brownness %>% filter(year == election_year),
      by = "county"
    ) %>%
    group_by(wk_id) %>%
    summarise(
      brownweights = sum(brownweights * proportion, na.rm = TRUE),
      brown1 = sum(brown1 * proportion, na.rm = TRUE),
      brown0_3 = sum(brown0_3 * proportion, na.rm = TRUE),
      brown0_5 = sum(brown0_5 * proportion, na.rm = TRUE),
      total_employees = sum(total_employees * proportion, na.rm = TRUE),
      state = first(state),
      .groups = "drop"
    ) %>%
    mutate(election_year = election_year) %>%
    # Calculate brownness shares as proportions of total employment
    mutate(
      brownness_share = brownweights / total_employees,
      brown1_share = brown1 / total_employees,
      brown0_3_share = brown0_3 / total_employees,
      brown0_5_share = brown0_5 / total_employees
    )

  return(brownness_districts)
}

# Process brownness data for all election years --------------------------

run_processing <- TRUE # Set to TRUE to reprocess data

if (run_processing) {
  # Process data for each election year
  brownness_2013 <- process_brownness_districts(wk_2013, 2013)
  brownness_2017 <- process_brownness_districts(wk_2017, 2017)
  brownness_2021 <- process_brownness_districts(wk_2021, 2021)

  # Bind all years into single dataset
  brownness_districts <- bind_rows(brownness_2013, brownness_2017, brownness_2021)

  # Save results for each year and combined dataset
  fwrite(brownness_2013, "data/intermediate/brownness_districts_2013.csv")
  fwrite(brownness_2017, "data/intermediate/brownness_districts_2017.csv")
  fwrite(brownness_2021, "data/intermediate/brownness_districts_2021.csv")
  fwrite(brownness_districts, "data/intermediate/brownness_districts_all.csv")
} else {
  # Load cached files if processing is not needed
  brownness_2013 <- fread("data/intermediate/brownness_districts_2013.csv")
  brownness_2017 <- fread("data/intermediate/brownness_districts_2017.csv")
  brownness_2021 <- fread("data/intermediate/brownness_districts_2021.csv")
  brownness_districts <- fread("data/intermediate/brownness_districts_all.csv")
}

# Merge with candidates data ----------------------------------------------

# Load candidate data
cf <- read_rds("data/intermediate/candidates/candidates_and_gles_merged.rds")

# Merge brownness data with candidate data by electoral district and year
districts_with_candidates <- brownness_districts %>%
  left_join(cf, by = c(
    "wk_id" = "elec_district",
    "election_year" = "elec_year"
  )) %>%
  # Create indicators for AfD candidate fielding
  mutate(afd_fields_candidate = ifelse(is.na(occupation), 0, 1)) %>%
  mutate(afd_fields_candidate_and_gles = ifelse(is.na(occupation) & gles_included == 1, 1, 0))

# Save final dataset with brownness and candidate information
write_rds(districts_with_candidates, "data/final/not_to_be_shared/data_candidates.rds")

### END