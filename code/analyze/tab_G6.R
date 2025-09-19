## SOEP Analysis: Individual-level Attitudes and Green Transition Effects
## This script generates Table G.6 analyzing the relationship between brown employment
## and individual-level attitudes using SOEP data.

# Outputs:
# 1. Table G.6: Effects on individual-level attitudes
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load packages
pacman::p_load(
  tidyverse,
  data.table,
  conflicted,
  fixest
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::between)

# Load helper functions
source("code/helper_functions/func_fixest_dict_soep.R")

# =============================================================================
# 1. Data loading and preparation
# =============================================================================

# Load cleaned SOEP data and prepare for analysis
df <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
  as_tibble() %>%
  mutate(state = as.character(state)) %>% # have to do that manually because loading csv always assumes class == numeric
  filter(syear > 2010) %>%
  mutate(emp = ifelse(emp == "", NA, emp)) %>%
  filter(emp == "employed")

# Define control variables for regression analysis
vlist <- c(
  "age", "monthly_income", "sex", "isced97",
  "worried_migration"
)

# Clean negative values (set to NA)
df <- df %>%
  mutate(across(
    all_of(vlist),
    ~ ifelse(. < 0, NA, .)
  ))

# Define individual-level attitude outcome variables
vars_use <- c(
  "satisf_work", "worried_job_security", "worried_econ_dev",
  "worried_finances", "satisf_hh_income", "satisf_ind_income"
)

# =============================================================================
# 2. Table G.6: Effects on individual-level attitudes
# =============================================================================

# Specification with covariates (no weights)
did_cov <- feols(
  .[vars_use] ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

# Define table styling for LaTeX output
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

# Generate individual-level attitudes table
(etable(did_cov,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Effects on individual-level attitudes",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  extralines = list(
    Covariates = rep("Yes", length(vars_use))
  ),
  file = "results/tab_G6.tex"
))

### END
