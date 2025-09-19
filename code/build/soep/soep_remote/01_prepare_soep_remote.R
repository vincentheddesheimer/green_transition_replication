## SOEP Remote Data Preparation Script
## This script prepares various datasets for SOEP remote access analysis by merging county-level
## covariates, creating brownness measures, and generating input files for remote analysis.

rm(list=ls())

pacman::p_load(tidyverse, data.table, janitor, readxl)

# Load brownness data (county-level brown employment measures)
df <- fread("data/intermediate/cty_brownness.csv") |>
  rename(county = county_code)

# Load covariate datasets for county-level analysis
area_pop_emp_cty <- fread("data/intermediate/area_pop_emp.csv") |>
  select(county = county_code_21, year:pop_density)

manu <- fread("data/intermediate/cty_employment_harm2021.csv")

un <- fread("data/intermediate/cty_unemploymed_harm2021.csv")

hh_inc <- fread("data/intermediate/cty_hh_income_harm2021.csv")

inkar <- fread("data/intermediate/cty_inkar_harm2021.csv")

inkar_add <- fread("data/intermediate/cty_inkar_add.csv")

ardeco <- fread("data/intermediate/cty_gdp_pc_ardeco.csv")

# Merge all covariate datasets with brownness data -------------------------

df <- df |>
  left_join(area_pop_emp_cty, by = c("county", "year")) |>
  left_join(manu, by = c("county", "year")) |>
  left_join(un, by = c("county", "year")) |>
  left_join(hh_inc, by = c("county", "year")) |>
  left_join(inkar, by = c("county", "year")) |>
  left_join(inkar_add, by = c("county", "year")) |>
  left_join(ardeco, by = c("county", "year")) |>
  select(-county_name)

# Create additional derived variables --------------------------------------

# Calculate unemployment rate from employment and unemployment counts
df <- df |> mutate(unemp_rate_regstat = unemployed_regstat / (working_population_regstat + unemployed_regstat))

# Create state variables from county codes (first two digits)
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
    )
  )

# Create post-treatment dummy variables for different time periods
df <- df %>%
  mutate(
    post_2014 = if_else(year > 2014, 1, 0),
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

# Select relevant variables for analysis ------------------------------

names(df)

df <- df |> select(
  county, year,
  brownness_share, brown1_share, brown0_3_share, brown0_5_share, 
  population, pop_density, share_manufacturing_regstat,
  hh_income_vgrdl,
  arbeitslosenquote_inkar,
  gdp_pc_ardeco
  )

# Create 2015 cross-section for analysis
df_2015 <- df |>
  filter(year == 2015) |>
  select(-year)

# Create 1999 cross-section for historical comparison
df_1999 <- df |>
  filter(year == 1999) |>
  select(-year)

# Generate input file for SOEP remote: 2015 brownness and covariates ------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/cty_brownness_covars.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("county brownness_share_2015 brown1_share_2015 brown0_3_share_2015 brown0_5_share_2015 population_2015 pop_density_2015 share_manufacturing_2015 hh_income_2015 gross_avg_wage_2015 unemp_rate_2015 asylumseekers_2015 gdppc_2015\n", file = file_conn)

# Write data rows to the output file
write.table(df_2015, file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")

# Generate input file for SOEP remote: 1999 brownness data ----------------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/cty_brownness_1999.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("county brown0_3_share_1999\n", file = file_conn)

# Write data rows to the output file
write.table(df_1999 |> select(county, brown0_3_share), file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")

# Load Vona data for occupation-level greenness/brownness measures --------

# Greeness / brownness classification by ISCO-08 occupation codes
vona <- fread("data/raw/occupations/Volna_ISCOCodes.csv") |>
  select(isco08 = ISCO08,
         Greenness, Brownness)

# Generate input file for SOEP remote: ISCO-08 greenness/brownness --------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/brownness_isco08.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("isco08 greenness_08 brownness_08\n", file = file_conn)

# Write data rows to the output file
write.table(vona, file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")

# Load ISCO correspondence tables for code conversion ----------------------

## Multiple corresponding isco08 codes for some isco88 codes --> create multiple columns
isco <- read_excel("data/raw/occupations/corrtab08-88.xls")

# Transform ISCO-88 scores to ISCO-08 for compatibility
isco <- isco  |>
  select(
    isco08_ilo = `ISCO 08 Code`,
    isco88 = `ISCO-88 code`
  ) |>
  # Convert all columns to numeric for consistency
  mutate(across(everything(), as.numeric))

# Generate input file for SOEP remote: ISCO correspondence table ----------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/isco_correspondence.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("isco08_ilo isco88\n", file = file_conn)

# Write data rows to the output file
write.table(isco, file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")

# Load and prepare survey weights for SOEP analysis ----------------------

w_df <- read_rds("data/final/not_to_be_shared/data_soep_weights.rds")

# Generate input file for SOEP remote: Survey weights --------------------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/weights.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("double pid syear weights\n", file = file_conn)

# Write data rows to the output file
write.table(w_df, file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")

# Load and prepare ISCO-88 brownness data -------------------------------

# Load brownness_isco88 data for historical occupation analysis
brownness_isco88 <- fread("data/raw/occupations/brownness_isco88.csv")

# Generate input file for SOEP remote: ISCO-88 brownness data ------------

# Specify the file path where you want to save the external dataset
output_file <- "data/intermediate/soep_remote/brownness_isco88.txt"

# Open the output file for writing
file_conn <- file(output_file, "w")

# Write column names to the output file
cat("isco88 greenness_88 brownness_88\n", file = file_conn)

# Write data rows to the output file
write.table(brownness_isco88, file = file_conn, append = TRUE, row.names = FALSE, col.names = FALSE, sep = " ", na = ".")

# Close the output file
close(file_conn)

# Print a message indicating where the output file was saved
cat("External dataset saved as:", output_file, "\n")


### END