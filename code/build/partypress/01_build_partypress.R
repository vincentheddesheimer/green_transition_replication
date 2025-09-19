## German Party Press Release Data Preparation Script
## This script loads and combines German party press release data from various sources
## including weekly/monthly agendas, issue classifications, and text content for analysis
## of party communication patterns during the green transition.

# The raw data files are not publicly available.
# Contact Dr. Cornelius Erfort (cornelius.erfort@uni-wh.de) for access.
# The processed dataset was published in the following paper:
# Erfort, Cornelius, Lukas F. Stoetzer, and Heike Klüver. "The PARTYPRESS Database: A new comparative database of parties’ press releases." Research & Politics 10, no. 3 (2023).

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, quanteda, tidytext, data.table)

# Set data path
path <- "data/raw/partypress/"

# Load party press release metadata ---------------------------------------

# Load main party press release data
partypress <- fread(paste0(path, "partypress.csv")) |>
  filter(country_name == "germany") |>
  select(-country_name)

# Load weekly agenda data
weekly <- fread(paste0(path, "weekly_agendas.csv")) |>
  filter(country_name == "germany") |>
  select(-country_name)

# Load monthly agenda data
monthly <- fread(paste0(path, "monthly_agendas.csv")) |>
  filter(country_name == "germany") |>
  select(-country_name)

# Load issue classification data
issues <- fread(paste0(path, "partypress_issues.csv"))

# Load press release text content
text <- fread(paste0(path, "partypress_texts.csv")) |>
  filter(country_name == "germany") |>
  select(-country_name)

# Combine all datasets into single dataframe ------------------------------

# Merge issues and text content with main press release data
partypress <- partypress |>
  left_join(issues, by = c("issue_mono" = "issue")) |>
  left_join(text, by = "id") |>
  mutate(year = lubridate::year(date))

# Save combined dataset
fwrite(partypress, "data/final/not_to_be_shared/data_partypress.csv")

### END