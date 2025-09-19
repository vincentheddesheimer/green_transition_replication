# =============================================================================
# Outputs:
# - Table G.7: Additional individual-level specifications – heterogeneity
# - Table G.8: Additional individual-level specifications – heterogeneity by prior attitudes
#
# Note: This replication uses the cleaned SOEP data with brownness scores.
# Users need to obtain access to the original SOEP data themselves.
# =============================================================================

rm(list = ls())

# Load required packages
pacman::p_load(
  tidyverse,
  pbapply,
  MatchIt,
  data.table,
  kableExtra,
  conflicted,
  fixest
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Load helper functions
source("code/helper_functions/func_fixest_dict_soep.R")

# Load data
df <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
  as_tibble() %>%
  mutate(state = as.character(state)) %>% # have to do that manually because loading csv always assumes class == numeric
  filter(syear > 2010) %>%
  mutate(emp = ifelse(emp == "", NA, emp)) %>%
  filter(emp == "employed")

# Create 2015 values for some more moderators
df15 <- df %>%
  filter(syear == 2015) %>%
  mutate(
    worried_job_security_above_med_2015 = case_when(
      is.na(worried_job_security) ~ NA,
      worried_job_security %in% c(2, 3) ~ 1,
      TRUE ~ 0
    ),
    worried_finances_above_med_2015 = case_when(
      is.na(worried_finances) ~ NA,
      worried_finances %in% c(2, 3) ~ 1,
      TRUE ~ 0
    ),
    worried_migration_above_med_2015 = case_when(
      is.na(worried_migration) ~ NA,
      worried_migration %in% c(2, 3) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(
    pid, worried_job_security_above_med_2015,
    worried_finances_above_med_2015,
    worried_migration_above_med_2015
  )

# Merge back to main data
df <- df %>%
  left_join(df15, by = "pid")

# Political variables
pvars <- c(
  "SPD", "FDP", "Gruene",
  "Linke",
  "CDUCSU", "far_right", "partisan"
)

# Proper labels for the pvars
ndf_parties <- data.frame(v = pvars, lab = c(
  "SPD", "FDP", "Green Party",
  "Left Party",
  "CDU/CSU", "Far-right parties\n(incl. AfD)",
  "Supports any party"
))

# List of variables w/ proper names
vlist <- c(
  "age", "monthly_income", "sex", "isced97",
  "worried_migration",
  "migback"
)

df <- df %>%
  mutate(across(
    all_of(vlist),
    ~ ifelse(. < 0, NA, .)
  ))

# =============================================================================
# Table G.7: Additional individual-level specifications – heterogeneity
# =============================================================================

# SOEP heterogeneity models
did_gender <- feols(
  far_right ~ brown_dummy * post_2015 * female_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

did_age <- feols(
  far_right ~ brown_dummy * post_2015 * age_above_50_2015 +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

did_educ <- feols(
  far_right ~ brown_dummy * post_2015 * has_abitur_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

did_income <- feols(
  far_right ~ brown_dummy * post_2015 * income_above_med_2015 + age +
    as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

# List of models for Table G.7
mlist <- list(
  did_gender,
  did_age,
  did_educ,
  did_income
)

# Prepare table formatting
sdf <- style.tex(
  depvar.title = "",
  fixef.title = "",
  fixef.suffix = " FE",
  var.title = "",
  stats.title = "",
  model.title = "",
  yesNo = c("Yes", "No"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05)
)

# Create Table G.7
(etable(mlist,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Additional individual-level specifications – heterogeneity",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  file = "results/tab_G7.tex"
))

# =============================================================================
# Table G.8: Additional individual-level specifications – heterogeneity by prior attitudes
# =============================================================================

# Models for the attitudinal variables
did_worry_job <- feols(
  far_right ~ brown_dummy * post_2015 * worried_job_security_above_med_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined + worried_migration | pid + syear,
  df %>% filter(emp == "employed"),
  cluster = ~pid
)

did_worry_fin <- feols(
  far_right ~ brown_dummy * post_2015 * worried_finances_above_med_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined + worried_migration | pid + syear,
  df %>% filter(emp == "employed"),
  cluster = ~pid
)

did_worry_mig <- feols(
  far_right ~ brown_dummy * post_2015 * worried_migration_above_med_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined + worried_migration | pid + syear,
  df %>% filter(emp == "employed"),
  cluster = ~pid
)

# List of models for Table G.8
mlist_worry <- list(
  did_worry_job,
  did_worry_fin,
  did_worry_mig
)

# Create Table G.8
(etable(mlist_worry,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Additional individual-level specifications – heterogeneity by prior attitudes",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  file = "results/tab_G8.tex"
))

### END
