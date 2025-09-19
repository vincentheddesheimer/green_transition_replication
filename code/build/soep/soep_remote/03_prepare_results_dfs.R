## SOEP Remote Results Data Preparation Script
## This script prepares and combines results from SOEP remote analysis.
## It processes voting, attitudes, and status results from remote analysis and creates
## standardized dataframes for further analysis and visualization.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table)

# Load and process voting results -----------------------------------------

# Load voting results from SOEP remote analysis
df_voting <- fread("data/intermediate/soep_remote/voting.csv") |>
  filter(str_detect(iv, "c.treat")) %>%
  mutate(year = as.numeric(str_replace(iv, ".syearc.treat", ""))) %>%
  # create column indicating whether coefficient is significant based on column coef and column se
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) %>%
  filter(year > 2009) |>
  filter(dv %in% c("AfD", "CDUCSU", "far_right")) |>
  mutate(
    dv = ifelse(dv == "far_right", "Far-right parties", dv),
    analysis = "voting"
  )

glimpse(df_voting)

# Load and process attitudes results --------------------------------------

# Load attitudes results from SOEP remote analysis
df_attitudes <- fread("data/intermediate/soep_remote/attitudes.csv") |>
  filter(str_detect(iv, "c.treat")) %>%
  mutate(year = as.numeric(str_replace(iv, ".syearc.treat", ""))) %>%
  # create column indicating whether coefficient is significant based on column coef and column se
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) |>
  filter(year > 2009) |>
  filter(dv %in% c("worried_econ_dev", "worried_finances", "worried_job_security")) |>
  mutate(
    dv = case_when(
      dv == "satisf_hh_income" ~ "Satisfaction: household income",
      dv == "satisf_ind_income" ~ "Satisfaction: individual income",
      dv == "worried_econ_dev" ~ "Worried: general\neconomic situation",
      dv == "worried_finances" ~ "Worried: personal\neconomic situation",
      dv == "worried_job_security" ~ "Worried: job security"
    ),
    analysis = "attitudes"
  )

glimpse(df_attitudes)

# Load and process 1999 attitudes results (without immigration) ----------

# read parsed results df
df_attitudes_1999 <- fread("data/intermediate/soep_remote/attitudes_1999_woimmig.csv") |>
  filter(str_detect(iv, "c.treat")) %>%
  mutate(year = as.numeric(str_replace(iv, ".syearc.treat", ""))) %>%
  # create column indicating whether coefficient is significant based on column coef and column se
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) |>
  filter(year > 1995) |>
  filter(dv %in% c("worried_econ_dev", "worried_finances", "worried_job_security")) |>
  mutate(
    dv = case_when(
      dv == "satisf_hh_income" ~ "Satisfaction: household income",
      dv == "satisf_ind_income" ~ "Satisfaction: individual income",
      dv == "worried_econ_dev" ~ "Worried: general\neconomic situation",
      dv == "worried_finances" ~ "Worried: personal\neconomic situation",
      dv == "worried_job_security" ~ "Worried: job security"
    ),
    analysis = "attitudes_1999"
  )

glimpse(df_attitudes_1999)

# Load and process status results -----------------------------------------

# read status data
df_status <- fread("data/intermediate/soep_remote/status_pooled.csv") |>
  filter(str_detect(iv, "treat")) %>%
  # create column indicating whether coefficient is significant based on column coef and column se
  mutate(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) |>
  mutate(
    analysis = "status"
  ) |>
  mutate(
    spec = case_when(
      str_detect(dv, "bivariate") ~ "Bivariate",
      str_detect(dv, "basecov") ~ "Base controls & state FE",
      str_detect(dv, "addcov") ~ "Additional controls & state FE"
    ),
    dv = case_when(
      str_detect(dv, "iss1") ~ "iss1",
      str_detect(dv, "iss2") ~ "iss2"
    )
  )

## Create labels for status variables

lab_df <- data.frame(
  dv = c("iss1", "iss2"),
  label = c(
    "Status in social environment",
    "Status in Germany"
  )
)

# Merge labels with status data
df_status <- df_status |>
  left_join(lab_df, by = "dv")

# Combine all datasets into single results dataframe ----------------------

# Combine all data
df_all <- bind_rows(df_voting, df_attitudes, df_attitudes_1999, df_status)

glimpse(df_all)

# Save combined results
fwrite(df_all, "data/final/not_to_be_shared/data_soep_remote_results.csv")



# Create pooled status dataframe for detailed analysis --------------------

# Load pooled status data
data_table <- "data/intermediate/soep_remote/status_pooled_r2.csv" %>%
    read_csv() %>%
    filter(!str_detect(iv, "bula"))

# Process specification and dependent variable names
data_table <- data_table %>%
    mutate(
        spec = case_when(
            str_detect(dv, "bivariate") ~ "Bivariate",
            str_detect(dv, "basecov") ~ "Base controls & state FE",
            str_detect(dv, "addcov") ~ "Additional controls & state FE"
        ),
        dv = case_when(
            str_detect(dv, "iss1") ~ "iss1",
            str_detect(dv, "iss2") ~ "iss2"
        )
    )

# Merge with labels
data_table <- data_table %>%
    left_join(lab_df, by = "dv")

# Get unique independent variables for proper naming
data_table$iv %>%
    unique() %>%
    dput()

# Create proper variable names for LaTeX output
proper_names_df <- data.frame(
    iv = c(
        "treat", "_cons", "age", "0.isced", "1.isced", "2.isced", "3.isced",
        "4.isced", "5.isced", "6.isced", "migback", "sex", "2016.syear",
        "2018.syear", "isei_combined", "monthly_income"
    ),
    variable = c(
        "County-level brown share", "Constant", "Age", "ISCED 0", "ISCED 1", "ISCED 2", "ISCED 3",
        "ISCED 4", "ISCED 5", "ISCED 6", "Migration background", "Female", "Year-2016",
        "Year=2018", "ISEI score", "Monthly income"
    )
)

# Merge proper names and calculate p-values
data_table <- data_table %>%
    left_join(proper_names_df, by = "iv") %>%
    dplyr::select(-iv, -dv) %>%
    mutate(p.value = ((1 - pnorm(abs(coef / se))) * 2)) %>%
    dplyr::select(label, variable, spec, everything()) %>%
    filter(!coef == 0) %>%
    filter(!variable == "Constant") %>%
    dplyr::select(label, variable, spec, coef, se, p.value, obs, r2) %>%
    mutate(spec = str_replace_all(spec, "&", "\\\\&"))

# Save pooled status results
fwrite(data_table, "data/final/not_to_be_shared/data_soep_remote_pooled_status.csv")

# END