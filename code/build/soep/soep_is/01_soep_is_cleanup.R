## SOEP-IS Data Cleaning and Preparation Script
## This script processes raw SOEP-IS (German Socio-Economic Panel - Innovation Sample) data
## by loading, merging, and cleaning various datasets including employment and demographic information.

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, haven, data.table, readxl, conflicted)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Set the working directory to SOEP-IS data location
path <- "/Users/vincentheddesheimer/Princeton Dropbox/Vincent Heddesheimer/Data/Panel_Surveys/SOEP/soep-is.2021_stata_en/"
# path <- "data/raw/soep/soep_is/"



# Load and prepare root dataset (person-level information) -----------------

ppathl <- data.table(read_dta(paste0(path, "ppfad.dta")))
root <- ppathl[, .(pid, cid, eintritt, austritt, sex, gebjahr, germborn, corigin, migback)]

# Load survey information and interview dates ------------------------------

biol <- data.table(read_dta(paste0(path, "bio.dta")))
interview <- biol[, .(pid, syear, iyear, imonth, iday)]

# Load state/federal state information ------------------------------------

hbrutto <- data.table(read_dta(paste0(path, "hbrutto.dta")))
hbrutto <- hbrutto |> select(hid, syear, bula)

# Load status variables ---------------------------------------------------

inno <- data.table(read_dta(paste0(path, "inno.dta")))
status <- inno[, .(pid, syear, iss1, iss2)]

# Load employment and education variables ----------------------------------

# pglfs: labor force status
# pglabgro: current gross labor income in euro
# pglabnet: current net labor income in euro
# pgstib: occupational position
# pgjobch: occupational change
# pgis88: current occupational classification (ISCO-88 Com, 4 digits)
# pgis08: current occupational classification (ISCO-08, 4 digits)
# pgisced: ISCED-1997-Classification: "International Standard Classification of Education (ISCED)"
# pgisei08: Last Reached Isei Value
# pgisei88: Last Reached Isei Value
# pgnace: Industry Occupation (NACE Rev. 1.1, Sector)
# pgnace2: Industry Occupation [pbra] (NACE Rev. 2, Sector)

pgen <- data.table(read_dta(paste0(path, "pgen.dta")))
employment <- pgen[, .(pid, hid, syear, pglfs, pglabgro, pglabnet, pgstib, pgjobch,
                       pgis88, pgis08, pgisced, pgisei08, pgisei88,
                       pgnace, pgnace2, pgsbil)]

# Load other variables (income, migration concerns) -----------------------

p <- data.table(read_dta(paste0(path, "p.dta")))
other <- p[, .(pid, syear, plc0013, plj0046)]

# Merge all datasets into main dataframe ----------------------------------

df <- employment |>
  left_join(root, by = c("pid"), suffix=c("",".y")) |>
  left_join(interview, by = c("pid", "syear"), suffix=c("",".y")) |>
  left_join(hbrutto, by = c("hid", "syear"), suffix=c("",".y")) |>
  left_join(status, by = c("pid", "syear"), suffix=c("",".y")) |>
  left_join(other, by = c("pid", "syear"), suffix=c("",".y")) |>
  select(-ends_with(".y"))

# Sort data by person ID and survey year
df <- df |>
  arrange(pid, syear)

# Rename variables for clarity and consistency ----------------------------

df <- df |>
  rename(labor_force_status = pglfs,
         labor_income_gross = pglabgro,
         labor_income_net = pglabnet,
         occupational_position = pgstib,
         occupational_change = pgjobch,
         state = bula,
         monthly_income = plc0013,
         edu = pgsbil,
         birth_year = gebjahr,
         worried_migration = plj0046
  ) |>
  rename_all(~str_replace_all(.,"pg",""))

# Recode and clean variables ----------------------------------------------

df <- df |>
  mutate(
    state = ifelse(state < 0, NA, state),
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
    female = ifelse(sex == 2, 1, 0),
    male = ifelse(sex == 1, 1, 0),
    birth_year = na_if(birth_year, -1),
    monthly_income = ifelse(monthly_income < 0, NA, monthly_income),
    labor_force_status = ifelse(labor_force_status < 0, NA, labor_force_status),
    labor_income_gross = ifelse(labor_income_gross < 0, NA, labor_income_gross),
    labor_income_net = ifelse(labor_income_net < 0, NA, labor_income_net),
    occupational_change = ifelse(occupational_change > 2, 1, 
                                 ifelse(occupational_change == 2 | occupational_change == 1, 0, NA)),
    isco88 = ifelse(is88 < 0, NA, is88),
    isco08 = ifelse(is08 < 0, NA, is08),
    isced97 = ifelse(isced < 0, NA, isced),
    isei08 = ifelse(isei08 < 0, NA, isei08),
    isei88 = ifelse(isei88 < 0, NA, isei88),
    nace = ifelse(nace < 0, NA, nace),
    nace2 = ifelse(nace2 < 0, NA, nace2),
    edu = ifelse(edu > 4 | edu < 1, NA, edu),
    iss1 = ifelse(iss1 < 0, NA, iss1),
    iss2 = ifelse(iss2 < 0, NA, iss2),
    worried_migration = ifelse(worried_migration < 0, NA, abs(worried_migration - 3)),
    age = syear - birth_year,
    sex = ifelse(sex < 0, NA, sex-1),
    state = as.character(state),
    # Create employment status categories
    emp = case_when(
      labor_force_status %in% c(1:5, 7:9, 12) ~ "nilf",
      labor_force_status %in% c(10, 11) ~ "employed",
      labor_force_status %in% c(6) ~ "unemployed"
    ),
    migback = if_else(migback > 1, 1, 0),
    # Censor extreme income values
    monthly_income = if_else(monthly_income > quantile(monthly_income, probs = c(0.9999), na.rm = T), NA, monthly_income)
  )  |>
  # Standardize ISEI scores by year
  group_by(syear) %>%
  mutate(
    isei08 = (isei08 - mean(isei08, na.rm = T)) / sd(isei08, na.rm = T),
    isei88 = (isei88 - mean(isei88, na.rm = T)) / sd(isei88, na.rm = T)
  ) %>%
  ungroup() %>%
  # Combine ISEI scores from both versions
  mutate(isei_combined = coalesce(isei08, isei88))

# Create 2015 baseline variables for heterogeneous treatment effects ------

hte_vars <- df %>%
  filter(syear == 2015) %>%
  mutate(
    has_abitur_2015 = case_when(
      is.na(edu) ~ NA,
      edu == 4 ~ 1,
      TRUE ~ 0
    ),
    has_higher_ed_2015 = case_when(
      is.na(isced97) ~ NA,
      isced97 == 6 ~ 1,
      TRUE ~ 0
    ),
    east_2015 = case_when(
      is.na(state) ~ NA,
      as.numeric(state) > 11 ~ 1,
      TRUE ~ 0
    ),
    female_2015 = case_when(
      is.na(female) ~ NA,
      female == 1 ~ 1,
      TRUE ~ 0
    ),
    income_above_med_2015 = case_when(
      is.na(monthly_income) ~ NA,
      monthly_income > median(monthly_income, na.rm = T) ~ 1,
      TRUE ~ 0
    ),
    age_above_50_2015 = case_when(
      is.na(age) ~ NA,
      age > 50 ~ 1,
      TRUE ~ 0
    ),
    worried_migration_high_2015 = case_when(
      is.na(worried_migration) ~ NA,
      worried_migration > 1 ~ 1,
      T ~ 0
    ),
    manufacturing_job_2015 = case_when(
      is.na(nace2) ~ NA,
      nace2 %in% c(10:33) ~ 1,
      T ~ 0
    )
  ) %>%
  dplyr::select(
    pid, has_abitur_2015,
    has_higher_ed_2015, east_2015, 
    female_2015, income_above_med_2015, 
    age_above_50_2015, manufacturing_job_2015, worried_migration_high_2015
  )

## Merge 2015 baseline variables back to main dataframe

df <- df %>%
  left_join(hte_vars, by = "pid")

# Load occupation-level greenness/brownness data --------------------------

# Greeness / brownness classification by ISCO-08 occupation codes
vona <- fread("data/raw/occupations/Volna_ISCOCodes.csv") |>
  select(isco08 = ISCO08,
         Greenness, Brownness)

# Vona crosswalked to ISCO-88 for historical compatibility
vona88 <- fread("data/raw/occupations/brownness_isco88.csv") |>
  rename(Greenness_88 = Greenness, Brownness_88 = Brownness)

# ISCO correspondence table for code conversion
isco <- read_xls("data/raw/occupations/corrtab08-88.xls")

# Merge with greenness/brownness scores for both ISCO-08 and ISCO-88 ------

df_merged <- df |>
  left_join(vona, by = c("isco08" = "isco08")) |>
  left_join(vona88, by = c("isco88" = "isco88")) |>
  # combine greenness/brownness measures from both ISCO versions
  mutate(
    Greenness = ifelse(!is.na(isco08) & is.na(Greenness), 0, Greenness),
    Brownness = ifelse(!is.na(isco08) & is.na(Brownness), 0, Brownness),
    Greenness_88 = ifelse(!is.na(isco88) & is.na(Greenness_88), 0, Greenness_88),
    Brownness_88 = ifelse(!is.na(isco88) & is.na(Brownness_88), 0, Brownness_88),
    greenness = ifelse(!is.na(Greenness), Greenness, Greenness_88),
    brownness = ifelse(!is.na(Brownness), Brownness, Brownness_88),
    green_dummy = ifelse(greenness > 0.2, 1, 0),
    brown_dummy = ifelse(brownness > 0.3, 1, 0)
  )

# Remove intermediate greenness/brownness variables
df_merged <- df_merged |>
  select(-Greenness, -Greenness_88, -Brownness, -Brownness_88)

## Code brown employment status in 2015 (constant within person)
df_merged <- df_merged %>%
  group_by(pid) %>%
  mutate(observed_2015 = 1 * (any(syear == 2015, na.rm = T))) %>%
  mutate(brown_dummy_2015 = ifelse(
    observed_2015,
    brown_dummy[syear == 2015],
    NA
  )) %>%
  ungroup()

# Write final cleaned dataset to file
fwrite(df_merged, "data/final/not_to_be_shared/data_soep_is.csv")

### END