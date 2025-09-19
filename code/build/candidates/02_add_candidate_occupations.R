## Candidate Data Merging Script
## This script merges candidate occupation data with 
## GLES candidate survey data to create a comprehensive dataset of AfD candidates
## with both occupational and political background information.

rm(list = ls())

pacman::p_load(tidyverse, readxl, stringi, data.table)

# Load candidate occupation data from Hilbig repository --------------------
path <- "data/raw/candidates/"


# Get the data and filter to AfD direct candidates
cf <- fread(paste0(path, "candidates_with_occupations.csv")) %>%
    filter(elec_year %in% c(2013, 2017, 2021)) %>%
    filter(party == "afd") %>%
    filter(direct_cand == 1) %>%
    dplyr::select(elec_year, elec_district, occupation, matches("kldb"), matches("isco|isei"), full_name) %>%
    arrange(elec_year, elec_district) %>%
    group_by(elec_year, elec_district) %>%
    slice(1) %>% # Keep only first candidate per district (in case of duplicates)
    ungroup()

# Load and prepare GLES candidate survey data ----------------------------

# Get candidate study data
gles_full <- read_rds("data/intermediate/candidates/gles_candidates_clean.rds") %>%
    filter(party == "afd") %>%
    dplyr::select(-party)

# Rename political activity variables for clarity
gles_full <- gles_full %>%
    rename_with(
        ~ str_replace(., "^PA_", "pol_act_"),
        starts_with("PA_")
    )

# Add prefix "gles_" to all variables except key identifiers
gles_full <- gles_full %>%
    rename_with(
        ~ paste0("gles_", .),
        -c(elec_year, gles_included, elec_district)
    )

# Merge candidate occupation data with GLES survey data ------------------

# Note: we don't need to merge on party since both datasets are restricted to AfD
cf <- cf %>%
    left_join(gles_full, by = c("elec_year", "elec_district")) %>%
    mutate(gles_included = ifelse(is.na(gles_included), 0, gles_included))

# Add local AfD candidate fielding data ----------------------------------

# Load data on whether local AfD candidates are fielded
local_afd_fielded <- fread("data/intermediate/county_afd_fielding.csv") %>%
    dplyr::select(county, election_year, afd_local_ran_prop) %>%
    filter(election_year > 2012)

# Merge local fielding data to candidate dataset
cf <- cf %>%
    left_join(local_afd_fielded, by = c("elec_year" = "election_year", "elec_district" = "county"))

# Save the merged candidate dataset --------------------------------------

write_rds(cf, "data/intermediate/candidates/candidates_and_gles_merged.rds")

### END