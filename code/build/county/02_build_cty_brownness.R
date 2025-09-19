# =============================================================================
# County Level Brownness Scores Construction
# Vincent Heddesheimer
# 
# This script constructs county-level brownness scores from employment data
# across different time periods (1996-1998, 1999-2010, 2012-2022) using
# various classification schemes (brownweights, brown1, brown0.3, brown0.5).
# The data is harmonized to the 2021 county boundaries using crosswalks.
# =============================================================================

rm(list = ls())

# Scientific notation (important for excel sheet import and preserving correct AGS)
options(scipen = 999)

# Load required packages
pacman::p_load(tidyverse, readxl, data.table)

# =============================================================================
# 1996-1998 DATA PROCESSING
# =============================================================================

# -----------------------------------------------------------------------------
# Brown-weights construction
# -----------------------------------------------------------------------------

# Load brown weights data from Excel file
brown_weights <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1996-1998.xlsx", sheet = 2)

# Remove header rows and footer
brown_weights <- brown_weights[-c(1:11, nrow(brown_weights)), ]

# Assign column names
colnames(brown_weights) <- c("county", "total_1996", "brownweights_1996", 
                             "total_1997", "brownweights_1997", 
                             "total_1998", "brownweights_1998")

# Reshape to long format: county-year dataset with total and brown weights
brown_weights <- brown_weights %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Split county variable: digits are county code, letters are county name
brown_weights <- brown_weights %>%
  separate(county, into = c("county_code", "county_name"), sep = " ", extra = "merge")

# -----------------------------------------------------------------------------
# Brown_1 classification
# -----------------------------------------------------------------------------

# Load brown_1 data
brown_1 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1996-1998.xlsx", sheet = 3)

# Remove header rows and footer
brown_1 <- brown_1[-c(1:11, nrow(brown_1)), ]

# Assign column names
colnames(brown_1) <- c("county", "total_1996", "brown1_1996", 
                       "total_1997", "brown1_1997", 
                       "total_1998", "brown1_1998")

# Reshape to long format
brown_1 <- brown_1 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Combine brownweights and brown1 data
df96_98 <- bind_cols(brown_weights, brown_1 %>% select(brown1))

# -----------------------------------------------------------------------------
# Brown_0.3 classification
# -----------------------------------------------------------------------------

# Load brown_0.3 data
brown_03 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1996-1998.xlsx", sheet = 4)

# Remove header rows and footer
brown_03 <- brown_03[-c(1:11, nrow(brown_03)), ]

# Assign column names
colnames(brown_03) <- c("county", "total_1996", "brown0.3_1996", 
                        "total_1997", "brown0.3_1997", 
                        "total_1998", "brown0.3_1998")

# Reshape to long format
brown_03 <- brown_03 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.3 column to main dataset
df96_98 <- bind_cols(df96_98, brown_03 %>% select(brown0.3))

# -----------------------------------------------------------------------------
# Brown_0.5 classification
# -----------------------------------------------------------------------------

# Load brown_0.5 data
brown_05 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1996-1998.xlsx", sheet = 5)

# Remove header rows and footer
brown_05 <- brown_05[-c(1:11, nrow(brown_05)), ]

# Assign column names
colnames(brown_05) <- c("county", "total_1996", "brown0.5_1996", 
                        "total_1997", "brown0.5_1997", 
                        "total_1998", "brown0.5_1998")

# Reshape to long format
brown_05 <- brown_05 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.5 column to main dataset
df96_98 <- bind_cols(df96_98, brown_05 %>% select(brown0.5))

# Verify consistent number of observations per year
df96_98 %>%
  group_by(year) %>%
  summarise(n = n())

# =============================================================================
# 1999-2010 DATA PROCESSING
# =============================================================================

# -----------------------------------------------------------------------------
# Brown-weights construction
# -----------------------------------------------------------------------------

# Load brown weights data
brown_weights <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1999-2010.xlsx", sheet = 2)

# Remove header rows and last two rows
brown_weights <- brown_weights[-c(1:11, nrow(brown_weights), nrow(brown_weights) - 1), ]

# Assign column names for all years 1999-2010
colnames(brown_weights) <- c("county", "total_1999", "brownweights_1999", 
                             "total_2000", "brownweights_2000", 
                             "total_2001", "brownweights_2001", 
                             "total_2002", "brownweights_2002", 
                             "total_2003", "brownweights_2003", 
                             "total_2004", "brownweights_2004", 
                             "total_2005", "brownweights_2005", 
                             "total_2006", "brownweights_2006", 
                             "total_2007", "brownweights_2007", 
                             "total_2008", "brownweights_2008", 
                             "total_2009", "brownweights_2009", 
                             "total_2010", "brownweights_2010")

# Reshape to long format
brown_weights <- brown_weights %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Split county variable
brown_weights <- brown_weights %>%
  separate(county, into = c("county_code", "county_name"), sep = " ", extra = "merge")

# -----------------------------------------------------------------------------
# Brown_1 classification
# -----------------------------------------------------------------------------

# Load brown_1 data
brown_1 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1999-2010.xlsx", sheet = 3)

# Remove header rows and last two rows
brown_1 <- brown_1[-c(1:11, nrow(brown_1), nrow(brown_1) - 1), ]

# Assign column names for all years 1999-2010
colnames(brown_1) <- c("county", "total_1999", "brown1_1999", 
                       "total_2000", "brown1_2000", 
                       "total_2001", "brown1_2001", 
                       "total_2002", "brown1_2002", 
                       "total_2003", "brown1_2003", 
                       "total_2004", "brown1_2004", 
                       "total_2005", "brown1_2005", 
                       "total_2006", "brown1_2006", 
                       "total_2007", "brown1_2007", 
                       "total_2008", "brown1_2008", 
                       "total_2009", "brown1_2009", 
                       "total_2010", "brown1_2010")

# Reshape to long format
brown_1 <- brown_1 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Combine brownweights and brown1 data
df99_10 <- bind_cols(brown_weights, brown_1 %>% select(brown1))

# -----------------------------------------------------------------------------
# Brown_0.3 classification
# -----------------------------------------------------------------------------

# Load brown_0.3 data
brown_03 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1999-2010.xlsx", sheet = 4)

# Remove header rows and last two rows
brown_03 <- brown_03[-c(1:11, nrow(brown_03), nrow(brown_03) - 1), ]

# Assign column names for all years 1999-2010
colnames(brown_03) <- c("county", "total_1999", "brown0.3_1999", 
                        "total_2000", "brown0.3_2000", 
                        "total_2001", "brown0.3_2001", 
                        "total_2002", "brown0.3_2002", 
                        "total_2003", "brown0.3_2003", 
                        "total_2004", "brown0.3_2004", 
                        "total_2005", "brown0.3_2005", 
                        "total_2006", "brown0.3_2006", 
                        "total_2007", "brown0.3_2007", 
                        "total_2008", "brown0.3_2008", 
                        "total_2009", "brown0.3_2009", 
                        "total_2010", "brown0.3_2010")

# Reshape to long format
brown_03 <- brown_03 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.3 column to main dataset
df99_10 <- bind_cols(df99_10, brown_03 %>% select(brown0.3))

# -----------------------------------------------------------------------------
# Brown_0.5 classification
# -----------------------------------------------------------------------------

# Load brown_0.5 data
brown_05 <- read_excel("data/raw/crosswalks/346634_SvB_KldB88_1999-2010.xlsx", sheet = 5)

# Remove header rows and last two rows
brown_05 <- brown_05[-c(1:11, nrow(brown_05), nrow(brown_05) - 1), ]

# Assign column names for all years 1999-2010
colnames(brown_05) <- c("county", "total_1999", "brown0.5_1999", 
                        "total_2000", "brown0.5_2000", 
                        "total_2001", "brown0.5_2001", 
                        "total_2002", "brown0.5_2002", 
                        "total_2003", "brown0.5_2003", 
                        "total_2004", "brown0.5_2004", 
                        "total_2005", "brown0.5_2005", 
                        "total_2006", "brown0.5_2006", 
                        "total_2007", "brown0.5_2007", 
                        "total_2008", "brown0.5_2008", 
                        "total_2009", "brown0.5_2009", 
                        "total_2010", "brown0.5_2010")

# Reshape to long format
brown_05 <- brown_05 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.5 column to main dataset
df99_10 <- bind_cols(df99_10, brown_05 %>% select(brown0.5))

# Verify consistent number of observations per year
df99_10 %>%
  group_by(year) %>%
  summarise(n = n())

# =============================================================================
# 2012-2022 DATA PROCESSING
# =============================================================================

# -----------------------------------------------------------------------------
# Brown-weights construction
# -----------------------------------------------------------------------------

# Load brown weights data
brown_weights <- read_excel("data/raw/crosswalks/346634_SvB_ISCO-08_2012-2022.xlsx", sheet = 2)

# Remove header rows and last two rows
brown_weights <- brown_weights[-c(1:11, nrow(brown_weights), nrow(brown_weights) - 1), ]

# Assign column names for all years 2012-2022
colnames(brown_weights) <- c("county", "total_2012", "brownweights_2012", 
                             "total_2013", "brownweights_2013", 
                             "total_2014", "brownweights_2014", 
                             "total_2015", "brownweights_2015", 
                             "total_2016", "brownweights_2016", 
                             "total_2017", "brownweights_2017", 
                             "total_2018", "brownweights_2018", 
                             "total_2019", "brownweights_2019", 
                             "total_2020", "brownweights_2020", 
                             "total_2021", "brownweights_2021", 
                             "total_2022", "brownweights_2022")

# Reshape to long format
brown_weights <- brown_weights %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Split county variable
brown_weights <- brown_weights %>%
  separate(county, into = c("county_code", "county_name"), sep = " ", extra = "merge")

# -----------------------------------------------------------------------------
# Brown_1 classification
# -----------------------------------------------------------------------------

# Load brown_1 data
brown_1 <- read_excel("data/raw/crosswalks/346634_SvB_ISCO-08_2012-2022.xlsx", sheet = 3)

# Remove header rows and last two rows
brown_1 <- brown_1[-c(1:11, nrow(brown_1), nrow(brown_1) - 1), ]

# Assign column names for all years 2012-2022
colnames(brown_1) <- c("county", "total_2012", "brown1_2012", 
                       "total_2013", "brown1_2013", 
                       "total_2014", "brown1_2014", 
                       "total_2015", "brown1_2015", 
                       "total_2016", "brown1_2016", 
                       "total_2017", "brown1_2017", 
                       "total_2018", "brown1_2018", 
                       "total_2019", "brown1_2019", 
                       "total_2020", "brown1_2020", 
                       "total_2021", "brown1_2021", 
                       "total_2022", "brown1_2022")

# Reshape to long format
brown_1 <- brown_1 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Combine brownweights and brown1 data
df12_22 <- bind_cols(brown_weights, brown_1 %>% select(brown1))

# -----------------------------------------------------------------------------
# Brown_0.3 classification
# -----------------------------------------------------------------------------

# Load brown_0.3 data
brown_03 <- read_excel("data/raw/crosswalks/346634_SvB_ISCO-08_2012-2022.xlsx", sheet = 4)

# Remove header rows and last two rows
brown_03 <- brown_03[-c(1:11, nrow(brown_03), nrow(brown_03) - 1), ]

# Assign column names for all years 2012-2022
colnames(brown_03) <- c("county", "total_2012", "brown0.3_2012", 
                        "total_2013", "brown0.3_2013", 
                        "total_2014", "brown0.3_2014", 
                        "total_2015", "brown0.3_2015", 
                        "total_2016", "brown0.3_2016", 
                        "total_2017", "brown0.3_2017", 
                        "total_2018", "brown0.3_2018", 
                        "total_2019", "brown0.3_2019", 
                        "total_2020", "brown0.3_2020", 
                        "total_2021", "brown0.3_2021", 
                        "total_2022", "brown0.3_2022")

# Reshape to long format
brown_03 <- brown_03 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.3 column to main dataset
df12_22 <- bind_cols(df12_22, brown_03 %>% select(brown0.3))

# -----------------------------------------------------------------------------
# Brown_0.5 classification
# -----------------------------------------------------------------------------

# Load brown_0.5 data
brown_05 <- read_excel("data/raw/crosswalks/346634_SvB_ISCO-08_2012-2022.xlsx", sheet = 5)

# Remove header rows and last two rows
brown_05 <- brown_05[-c(1:11, nrow(brown_05), nrow(brown_05) - 1), ]

# Assign column names for all years 2012-2022
colnames(brown_05) <- c("county", "total_2012", "brown0.5_2012", 
                        "total_2013", "brown0.5_2013", 
                        "total_2014", "brown0.5_2014", 
                        "total_2015", "brown0.5_2015", 
                        "total_2016", "brown0.5_2016", 
                        "total_2017", "brown0.5_2017", 
                        "total_2018", "brown0.5_2018", 
                        "total_2019", "brown0.5_2019", 
                        "total_2020", "brown0.5_2020", 
                        "total_2021", "brown0.5_2021", 
                        "total_2022", "brown0.5_2022")

# Reshape to long format
brown_05 <- brown_05 %>%
  pivot_longer(
    cols = -county,
    names_to = c(".value", "year"),
    names_sep = "_"
  )

# Add brown0.5 column to main dataset
df12_22 <- bind_cols(df12_22, brown_05 %>% select(brown0.5))

# Verify consistent number of observations per year
df12_22 %>%
  group_by(year) %>%
  summarise(n = n())

# =============================================================================
# CROSSWALK PROCESSING FOR 1996-1998
# =============================================================================

# The counties for df96_98 are at the Gebietsstand of the respective Stichtag 
# (30.06.1996, 30.06.1997, 30.06.1998). Need to crosswalk to Nov. 2023 
# Gebietsstand. Crosswalks are always at 31.12. of the respective year.
# We will crosswalk to 31.12.1995 for 1996, 31.12.1996 for 1997, 31.12.1997 for 1998

# Load crosswalk data for 1996 (using 31.12.1995 boundaries)
cw96 <- read_excel("data/raw/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx", sheet = 6) |>
  select(
    county_code = `Kreise\r\n 31.12.1995`,
    pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
    area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
    county_code_21 = `Kreise\r\n 31.12.2021`,
    area = `Fläche am 31.12.1995 in km²`,
    population = `Bevölkerung am 31.12.1995 in 1000`
  ) |>
  mutate(year = 1996) # Use 1996 as year for crosswalk to 31.12.1995

# Load crosswalk data for 1997 (using 31.12.1996 boundaries)
cw97 <- read_excel("data/raw/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx", sheet = 7) |>
  select(
    county_code = `Kreise\r\n 31.12.1996`,
    pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
    area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
    county_code_21 = `Kreise\r\n 31.12.2021`,
    area = `Fläche am 31.12.1996 in km²`,
    population = `Bevölkerung am 31.12.1996 in 1000`
  ) |>
  mutate(year = 1997)

# Load crosswalk data for 1998 (using 31.12.1997 boundaries)
cw98 <- read_excel("data/raw/crosswalks/ref-kreise-umrech-2021-1990-2020.xlsx", sheet = 8) |>
  select(
    county_code = `Kreise\r\n 31.12.1997`,
    pop_cw = `bevölkerungs- \r\nproportionaler \r\nUmsteige- \r\nschlüssel`,
    area_cw = `flächen-\r\nproportionaler\r\nUmsteige-schlüssel`,
    county_code_21 = `Kreise\r\n 31.12.2021`,
    area = `Fläche am 31.12.1997 in km²`,
    population = `Bevölkerung am 31.12.1997 in 1.000`
  ) |>
  mutate(year = 1998)

# Combine all crosswalk data
cw <- bind_rows(cw96, cw97, cw98)

# Verify county codes end with three zeroes (standard format)
cw %>%
  filter(!str_ends(county_code, "000"))

cw %>%
  filter(!str_ends(county_code_21, "000"))
# All codes end with three zeroes as expected

# Transform county codes to standard format
cw <- cw %>%
  mutate(
    # Remove last three digits from county_code
    county_code = str_remove(county_code, "\\d{3}$"),
    # Pad with 0 if only 4 digits long
    county_code = str_pad(county_code, width = 5, side = "left", pad = "0"),
    # Apply same transformation to county_code_21
    county_code_21 = str_remove(county_code_21, "\\d{3}$"),
    county_code_21 = str_pad(county_code_21, width = 5, side = "left", pad = "0")
  )

# Check number of unique county_code_21 per year
cw %>%
  group_by(year) %>%
  distinct(county_code_21) %>%
  summarise(n = n())

# =============================================================================
# MERGE AND HARMONIZE 1996-1998 DATA
# =============================================================================

# Merge df96_98 with crosswalk data
df96_98_merged <- df96_98 |>
  mutate(year = as.numeric(year)) |>
  left_join(cw, by = c("county_code", "year")) |>
  select(-c(area, population))

# Check for missing crosswalk matches
df96_98_merged %>%
  summarise_all(~ sum(is.na(.)))
# 8 observations lack corresponding county_code_21

# Identify missing counties
df96_98_merged %>%
  dplyr::filter(is.na(county_code_21)) %>%
  select(county_code, year) %>%
  distinct()

# -----------------------------------------------------------------------------
# Handle special cases
# -----------------------------------------------------------------------------

# Berlin counties are split into West (11100) and East (11200) Berlin
# Crosswalk files only available for unified Berlin (11000)
# Solution: Sum the two Berlin counties
df96_98_berlin <- df96_98_merged %>%
  filter(county_code == "11100" | county_code == "11200") %>%
  mutate(county_code_21 = "11000") %>%
  # Convert all brownness variables to numeric
  mutate_at(vars(total:brown0.5), as.numeric) |>
  group_by(year) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    brownweights = sum(brownweights, na.rm = TRUE),
    brown1 = sum(brown1, na.rm = TRUE),
    brown0.3 = sum(brown0.3, na.rm = TRUE),
    brown0.5 = sum(brown0.5, na.rm = TRUE),
    pop_cw = sum(pop_cw, na.rm = TRUE),
    area_cw = sum(area_cw, na.rm = TRUE),
    county_code_21 = first(county_code_21)
  ) %>%
  ungroup() |>
  mutate(
    county_code = "11000",
    pop_cw = 1,
    area_cw = 1,
    county_name = "Berlin"
  )

# Merge Berlin data back to main dataset
df96_98_merged <- df96_98_merged %>%
  # Convert brownness variables to numeric
  mutate_at(vars(total:brown0.5), as.numeric) |>
  filter(county_code != "11100" & county_code != "11200") %>%
  bind_rows(df96_98_berlin) %>%
  arrange(year, county_code)

# Handle Kreis Eisenach (16056) missing crosswalk for 1996 & 1997
# Inspection of 1994 and 1998 files shows 1:1 crosswalk to 2021 Wartburgkreis (16063)
# Apply this crosswalk for 1996 & 1997
df96_98_merged <- df96_98_merged %>%
  mutate(
    county_code_21 = if_else(county_code == "16056", "16063", county_code_21),
    pop_cw = if_else(county_code == "16056", 1, pop_cw),
    area_cw = if_else(county_code == "16056", 1, area_cw)
  )

# Verify no missing crosswalks remain
df96_98_merged %>%
  dplyr::filter(is.na(county_code_21)) %>%
  select(county_code, year) %>%
  distinct()
# No counties missing crosswalks

# -----------------------------------------------------------------------------
# Harmonize to 2021 boundaries
# -----------------------------------------------------------------------------

# Harmonize to 2021 Gebietsstand using county_code_21
# Apply population-weighted crosswalk factors
df96_98_harm <- df96_98_merged |>
  group_by(county_code_21, year) |>
  summarise(
    total = sum(total * pop_cw, na.rm = TRUE),
    brownweights = sum(brownweights * pop_cw, na.rm = TRUE),
    brown1 = sum(brown1 * pop_cw, na.rm = TRUE),
    brown0.3 = sum(brown0.3 * pop_cw, na.rm = TRUE),
    brown0.5 = sum(brown0.5 * pop_cw, na.rm = TRUE)
  ) |>
  rename(county_code = county_code_21) |>
  ungroup()

# Create dataset tracking crosswalked counties
cw_counties <- df96_98_merged |>
  mutate(crosswalked = ifelse(pop_cw < 1, 1, 0)) |>
  filter(crosswalked == 1) |>
  select(county_code_21, year, crosswalked) |>
  distinct()

# Merge crosswalk indicator with harmonized data
df96_98_harm <- df96_98_harm |>
  left_join(cw_counties, by = c("county_code" = "county_code_21", "year")) |>
  mutate(crosswalked = ifelse(is.na(crosswalked), 0, crosswalked))

# Check crosswalk statistics
table(df96_98_harm$crosswalked, useNA = "ifany")
# 118 counties were crosswalked

# Verify no missing data
table(is.na(df96_98_harm$brownweights))
# No missing values

# Check observations per year
df96_98_harm %>%
  group_by(year) %>%
  summarise(n = n())

# =============================================================================
# COMBINE ALL PERIODS
# =============================================================================

# Convert columns to numeric for consistency
df99_10 <- df99_10 |>
  mutate_at(vars(year:brown0.5), as.numeric)

df12_22 <- df12_22 |>
  mutate_at(vars(year:brown0.5), as.numeric)

# Combine all time periods
df <- bind_rows(df96_98_harm, df99_10, df12_22) |>
  arrange(county_code, year)

# Extract county names for final dataset
county_names <- df |>
  distinct(county_code, county_name) |>
  dplyr::filter(!is.na(county_name))

# Final data cleaning
df <- df |>
  mutate(
    # Set crosswalked to 0 if NA
    crosswalked = ifelse(is.na(crosswalked), 0, crosswalked)
  ) |>
  select(-county_name) |>
  left_join(county_names, by = "county_code") |>
  # Remove rows with missing county codes
  dplyr::filter(!is.na(county_code))

# Rename total column for clarity
df <- df |> rename(total_employees = total)

# =============================================================================
# SAVE FINAL DATASET
# =============================================================================

# Write processed data to CSV
fwrite(df, "data/intermediate/cty_brownness.csv")

# Verify final dataset structure
print(
  df %>%
    group_by(year) %>%
    summarise(n = n()),
  n = 25
)
# Dataset successfully created

# Verify that there are no duplicates
print(df %>%
  group_by(county_code, year) %>%
  summarise(n = n()) %>%
  filter(n > 1))
# No duplicates found

# =============================================================================
# END OF SCRIPT
# =============================================================================