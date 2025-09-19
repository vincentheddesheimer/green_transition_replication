# Outputs:
# - Table G.3: Main SOEP results table
# - Figure 6: SOEP event study
# - Table G.1: SOEP event study as table
# - Figure G.1: SOEP event study with economic outcomes
# - Table G.4: Weights and constant job treatment specifications
# - Table G.5: Respondents observed in 2015 robustness check
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load packages
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
source("code/helper_functions/func_tidy_feols.R")
source("code/helper_functions/func_theme_custom.R")

# =============================================================================
# 1. Data loading and preparation
# =============================================================================

# Load cleaned SOEP data and prepare for analysis
df_all <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
  as_tibble() %>%
  mutate(state = as.character(state)) %>% # have to do that manually because loading csv always assumes class == numeric
  filter(syear > 2010) %>%
  mutate(emp = ifelse(emp == "", NA, emp))

# Subset to employed individuals for main analysis
df <- df_all %>%
  filter(emp == "employed")

# Load and merge survey weights
w_df <- read_rds("data/final/not_to_be_shared/data_soep_weights.rds")

# Merge weights to main dataset
df <- df %>%
  left_join(w_df,
    by = c("pid", "syear")
  )

# Define political outcome variables
pvars <- c(
  "SPD", "FDP", "Gruene",
  "Linke",
  "CDUCSU", "far_right", "partisan"
)

# Create proper labels for political parties
ndf_parties <- data.frame(v = pvars, lab = c(
  "SPD", "FDP", "Green Party",
  "Left Party",
  "CDU/CSU", "Far-right parties\n(incl. AfD)",
  "Supports any party"
))

# Define control variables for regression analysis
vlist <- c(
  "age", "monthly_income",
  "sex", "isced97",
  "worried_migration",
  "migback"
)

# Clean negative values (set to NA)
df <- df %>%
  mutate(across(
    all_of(vlist),
    ~ ifelse(. < 0, NA, .)
  ))

# =============================================================================
# 2. Table G.3: Individual-level specifications
# =============================================================================

# Base specification without covariates
did <- feols(
  .[pvars] ~ brown_dummy * post_2015 | pid + syear,
  df,
  cluster = ~pid
)

# Specification with covariates (no weights)
did_cov <- feols(
  .[pvars] ~ brown_dummy * post_2015 + age +
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

# Generate main results table
(etable(did_cov,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Main specification",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  extralines = list(
    Covariates =
      rep("Yes", length(pvars))
  ),
  file = "results/tab_G3.tex"
))

# =============================================================================
# 3. Figure 6: SOEP event study
# =============================================================================

# Remove partisan variable from outcomes for event study
pvars <- pvars[!pvars == "partisan"]

# Event study regression with covariates
did_cov_event <- feols(
  .[pvars] ~ i(syear, brown_dummy, ref = 2015) + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

# Order parties for consistent plotting
ndf_parties2 <- ndf_parties %>%
  mutate(order = c(
    3, 5, 2,
    1, 4, 6, 7
  )) %>%
  arrange(desc(order))

# Set factor levels for consistent ordering
levels(ndf_parties2$lab) <- ndf_parties2$lab

# Process event study results for plotting
out_full_event <- did_cov_event %>%
  map_dfr(tidy_feols) %>%
  mutate(period = as.numeric(str_extract(term, "\\b\\d{4}\\b"))) %>%
  left_join(ndf_parties2, by = c("dv" = "v")) %>%
  bind_rows(
    # Add reference year (2015) with zero effect
    expand.grid(
      period = 2015,
      lab = unique(ndf_parties2$lab)
    ) %>%
      mutate(estimate = 0, conf.low = 0, conf.high = 0)
  )

# Create event study plot
ggplot(
  out_full_event %>%
    mutate(lab = str_replace(lab, "\n", " ")) %>%
    dplyr::filter(period > 2010) %>%
    dplyr::filter(str_detect(lab, "AfD|Green Party|CDU|SPD|FDP|Left")),
  aes(period, estimate * 100)
) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_errorbar(
    aes(
      ymin = conf.low * 100,
      ymax = conf.high * 100,
    ),
    width = 0.0
  ) +
  geom_point(shape = 21, fill = "white") +
  xlab("Year") +
  ylab("Change in party support rel. to 2015 (p.p.)") +
  theme_custom() +
  facet_wrap(~lab, ncol = 2) +
  scale_x_continuous(breaks = seq(2009, 2021, 1)) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  ))

ggsave("results/fig_6.pdf", width = 7.5, height = 6)

# =============================================================================
# 4. Table G.1: SOEP event study as table
# =============================================================================

# Generate event study table
(etable(did_cov_event,
  cluster = ~pid,
  tex = T,
  drop = "2006|2007|2008|2009|2010",
  style.tex = sdf,
  digits = 3,
  title = "Event study",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  extralines = list(
    Covariates =
      rep("Yes", length(pvars))
  ),
  file = "results/tab_G1.tex"
))

# =============================================================================
# 5. Figure G.1: SOEP event study with economic outcomes
# =============================================================================

# Add employed variable for economic outcomes analysis
df <- df %>%
  mutate(employed = case_when(
    emp == "employed" ~ 1,
    is.na(emp) ~ NA,
    TRUE ~ 0
  ))

# Define economic outcome variables
outcomes_econ <- c(
  "monthly_income"
)

# Event study regression for economic outcomes
did_econ <- feols(
  .[outcomes_econ] ~ i(syear, brown_dummy_2015, ref = 2015) | pid + syear,
  df,
  cluster = ~pid
)

# Extract coefficients for economic outcomes
out_econ <- did_econ %>%
  tidy_feols() %>%
  filter(str_detect(term, "brown_dummy")) %>%
  mutate(period = str_extract(term, "\\b\\d{4}\\b")) %>%
  mutate(period = as.numeric(period))

# Create economic outcomes event study plot
ggplot(
  out_econ %>% dplyr::filter(period > 2010),
  aes(period, estimate)
) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high,
    ),
    width = 0
  ) +
  geom_point(
    shape = 21, size = 2,
    fill = "white"
  ) +
  xlab("Year") +
  ylab("Change in income\nrel. to 2015 (p.p.)") +
  theme_custom() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2009, 2021, 1)) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  ))

ggsave("results/fig_G1.pdf", width = 7.5, height = 3.5)

# =============================================================================
# 6. Table G.4: Weights and constant job treatment specifications
# =============================================================================

# Base specification for far-right support
did_base <- feols(
  far_right ~ brown_dummy * post_2015 | pid + syear,
  df,
  cluster = ~pid
)

# Specification with covariates (no weights)
did_cov <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df,
  cluster = ~pid
)

# Specification with covariates and survey weights
did_cov_weight <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  weights = ~weights,
  df,
  cluster = ~pid
)

# Specification with covariates and state*year fixed effects
did_cov_state <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear^state,
  df,
  cluster = ~pid
)

### Time invariant treatment specifications

# Base specification with time-invariant treatment
did_invariant <- feols(
  far_right ~ brown_dummy_2015 * post_2015 | pid + syear,
  df_all,
  cluster = ~pid
)

# Specification with covariates and time-invariant treatment
did_cov_invariant <- feols(
  far_right ~ brown_dummy_2015 * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df_all,
  cluster = ~pid
)

# Specification with covariates, state*year FE, and time-invariant treatment
did_cov_state_invariant <- feols(
  far_right ~ brown_dummy_2015 * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear^state,
  df_all,
  cluster = ~pid
)

# Combine all specifications for table
mlist <- list(
  did_base, did_cov, did_cov_weight, did_cov_state,
  did_invariant, did_cov_invariant, did_cov_state_invariant
)

# Generate robustness table
(etable(mlist,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Additional specifications",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  extralines = list(
    Weights = c(rep("No", 2), "Yes", rep("No", 4)),
    Covariates = c("No", "Yes", "Yes", "Yes", "No", "Yes", "Yes")
  ),
  file = "results/tab_G4.tex"
))

# =============================================================================
# 7. Table G.5: Respondents observed in 2015 robustness check
# =============================================================================

# Base specification for respondents observed in 2015
did15_base_2015 <- feols(
  far_right ~ brown_dummy * post_2015 | pid + syear,
  df %>% filter(!is.na(brown_dummy_2015)) %>%
    filter(emp == "employed"),
  cluster = ~pid
)

# Specification with covariates for 2015 respondents
did15_cov_2015 <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df %>% filter(!is.na(brown_dummy_2015)) %>%
    filter(emp == "employed"),
  cluster = ~pid
)

# Specification with covariates and weights for 2015 respondents
did15_cov_weight_2015 <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  weights = ~weights,
  df %>% filter(!is.na(brown_dummy_2015)) %>%
    filter(emp == "employed"),
  cluster = ~pid
)

# Specification with covariates and state*year FE for 2015 respondents
did15_cov_state_2015 <- feols(
  far_right ~ brown_dummy * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear^state,
  df %>% filter(!is.na(brown_dummy_2015)) %>%
    filter(emp == "employed"),
  cluster = ~pid
)

### Time invariant treatment for 2015 respondents

# Base specification with time-invariant treatment
did15_invariant_2015 <- feols(
  far_right ~ brown_dummy_2015 * post_2015 | pid + syear,
  df_all,
  cluster = ~pid
)

# Specification with covariates and time-invariant treatment
did15_cov_invariant_2015 <- feols(
  far_right ~ brown_dummy_2015 * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear,
  df_all,
  cluster = ~pid
)

# Specification with covariates, state*year FE, and time-invariant treatment
did15_cov_state_invariant_2015 <- feols(
  far_right ~ brown_dummy_2015 * post_2015 + age +
    monthly_income + as.factor(isced97) +
    isei_combined +
    worried_migration | pid + syear^state,
  df_all,
  cluster = ~pid
)

# Combine all 2015 respondent specifications for table
mlist15 <- list(
  did15_base_2015, did15_cov_2015, did15_cov_weight_2015, did15_cov_state_2015,
  did15_invariant_2015, did15_cov_invariant_2015, did15_cov_state_invariant_2015
)

# Generate robustness table for 2015 respondents
(etable(mlist15,
  keep = "times",
  cluster = ~pid,
  tex = T,
  style.tex = sdf,
  digits = 3,
  title = "Additional specifications",
  fitstat = ~ r2 + n,
  digits.stats = 3,
  extralines = list(
    Weights = c(rep("No", 2), "Yes", rep("No", 4)),
    Covariates = c("No", "Yes", "Yes", "Yes", "No", "Yes", "Yes")
  ),
  file = "results/tab_G5.tex"
))

### END
