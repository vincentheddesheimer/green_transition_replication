## SOEP Descriptive Statistics Generation Script
## This script creates descriptive statistics for SOEP data, focusing on brown employment
## characteristics and demographic variables. It generates summary tables comparing
## brown vs non-brown employment across various demographic and economic indicators.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, pbapply, MatchIt, data.table, kableExtra, conflicted)

# Resolve package conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Load helper functions
source("code/helper_functions/func_theme_custom.R")

# Load and prepare SOEP data ---------------------------------------------

# Load main SOEP dataset
df <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
  as_tibble() %>%
  mutate(state = as.character(state)) %>% # Convert to character since CSV loading assumes numeric
  mutate(post_2015 = 1 * (syear > 2015))

# Clean income data by removing extreme outliers
q9999 <- quantile(df$monthly_income, probs = c(0.9999), na.rm = T)
df <- df %>%
  mutate(monthly_income = if_else(monthly_income > q9999, NA, monthly_income))

# Create employment status variable
df <- df %>%
  mutate(emp = case_when(
    labor_force_status %in% c(1:5, 7:9, 12) ~ "nilf", # Not in labor force
    labor_force_status %in% c(10, 11) ~ "employed", # Employed
    labor_force_status %in% c(6) ~ "unemployed" # Unemployed
  ))

# Recode migration background to binary (0/1)
df <- df %>%
  mutate(migback = if_else(migback > 1, 1, 0))

# Standardize ISEI (International Socio-Economic Index) variables
# Standardize both ISEI variables by year (z-score) since classification changes over time
df <- df %>%
  group_by(syear) %>%
  mutate(
    isei08 = (isei08 - mean(isei08, na.rm = T)) / sd(isei08, na.rm = T),
    isei88 = (isei88 - mean(isei88, na.rm = T)) / sd(isei88, na.rm = T)
  ) %>%
  ungroup() %>%
  # Combine ISEI variables, using ISEI08 if available, otherwise ISEI88
  mutate(isei_combined = coalesce(isei08, isei88))

# Create brown employment dummy variable
df <- df %>% mutate(brown_dummy = if_else(brownness > .3, 1, 0))

# Generate descriptive statistics for 2015 --------------------------------

# Filter to 2015 data for descriptive analysis
df15 <- df %>%
  filter(syear == 2015) %>%
  mutate(manufacturing = if_else(nace2 %in% c(10:33), 1, 0)) %>% # Manufacturing sector indicator
  filter(emp == "employed") %>% # Only employed individuals
  filter(!is.na(brown_dummy)) # Only those with brown employment data

# Define variables for descriptive analysis
vlist <- c(
  "female",
  "age", "monthly_income", "manufacturing", "isced97"
)

# Define proper variable names for output
vlist_proper <- c(
  "Female",
  "Age", "Monthly income",
  "Manufacturing job", "ISCED97"
)

# Generate summary statistics by brown employment status
df_agg <- df15 %>%
  filter(emp == "employed") %>%
  group_by(brown_dummy) %>%
  summarise(across(
    all_of(vlist),
    list(
      mean = ~ mean(., na.rm = T),
      n = ~ sum(!is.na(.))
    )
  )) %>%
  # Reshape data for easier analysis
  pivot_longer(cols = -brown_dummy, names_to = "var", values_to = "value") %>%
  separate(var, into = c("var", "what"), sep = "(?<=_)(?!.*_)") %>%
  pivot_wider(names_from = what, values_from = value) %>%
  arrange(var, brown_dummy) %>%
  mutate(var = str_sub(var, 1, (nchar(var) - 1))) %>% # Remove trailing underscore
  mutate(brown_dummy = ifelse(brown_dummy == 1, "Yes", "No"))

# Add proper variable names for output
dict_df <- tibble(var = vlist, var_proper = vlist_proper)

df_agg <- df_agg %>%
  left_join(dict_df, by = "var") %>%
  mutate(var = var_proper) %>%
  select(-var_proper) %>%
  # Round means to 2 decimal places
  mutate(across(all_of(c("mean")), ~ round(., 2)))

# Format sample sizes with thousands separators
df_agg$n <- formatC(df_agg$n, format = "d", big.mark = ",")

# Save descriptive statistics
fwrite(df_agg, "data/final/not_to_be_shared/data_soep_descriptive.csv")

### END