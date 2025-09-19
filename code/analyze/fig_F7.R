# Outputs:
# - Figure F.7: Event study with changes in brown employment share as treatment
# =============================================================================

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, fixest, conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Load functions
source("code/helper_functions/func_tidy_feols.R")
source("code/helper_functions/func_theme_custom.R")

# Function to clean results and get CIs
clean_results <- function(mod, term_find = "share_2013") {
  mod %>%
    map_dfr(tidy_feols) %>%
    dplyr::filter(str_detect(term, term_find)) %>%
    mutate(term = str_extract(term, "\\d+"))
}

# Load and prepare data
df <- fread("data/final/data_main.csv",
  colClasses = c("county" = "character")
) %>%
  mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
  mutate(state_id = substr(county, 1, 2)) %>%
  filter(election_year %in% c(1998, 2002, 2005, 2009, 2013, 2017, 2021)) %>%
  dplyr::filter(
    state_id != "11",
    state_id != "02",
    state_id != "04"
  )

# Calculate brown share changes
df <- df %>%
  group_by(county) %>%
  mutate(
    decline_brown_98_13 = -(brown0_3_share[election_year == 2013] - brown0_3_share[election_year == 1998])
  ) %>%
  ungroup()

# Set up model data
df <- df %>%
  group_by(county) %>%
  mutate(
    observed_2013 = any(election_year == 2013),
    pop_density_2013 = ifelse(observed_2013, pop_density[election_year == 2013], NA),
    brown_share_2013 = ifelse(observed_2013, brown0_3_share[election_year == 2013], NA),
    share_manufacturing_2013 = ifelse(observed_2013,
      share_manufacturing_regstat[election_year == 2013], NA
    )
  ) %>%
  ungroup()

# Define variables
pvars <- c("spd", "fdp", "gruene", "linke_pds", "afd", "cdu_csu", "far_right")
ndf_parties <- data.frame(v = pvars, lab = c(
  "SPD", "FDP", "Green Party", "Left Party", "AfD", "CDU/CSU", "Far-right parties"
)) %>%
  mutate(party_order = c(5, 3, 6, 7, 1, 4, 2)) %>%
  arrange(party_order) %>%
  mutate(lab = fct_reorder(lab, party_order))

# Standardize covariates
covars_to_standardize <- c(
  "hh_income_vgrdl", "gdp_pc_ardeco", "share_manufacturing_regstat",
  "pop_density", "population", "schulabganger_mit_allgemeiner_hochschulreife_inkar",
  "arbeitslosenquote_inkar", "auslanderanteil_inkar",
  "pop_density_2013", "share_manufacturing_2013"
)

df <- df %>%
  mutate(across(
    all_of(covars_to_standardize),
    ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
  ))

# Estimate model
m1_98_13 <- feols(
  .[pvars] ~ i(election_year, decline_brown_98_13, ref = 2013) +
    i(election_year, brown_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    arbeitslosenquote_inkar | county + election_year^regbez,
  cluster = ~county,
  data = df
)

# Prepare plot data
plot_df <- clean_results(m1_98_13, term_find = "decline_brown_98_13") %>%
  mutate(treatment = "decline_brown_98_13") %>%
  mutate(term = as.numeric(term)) %>%
  left_join(ndf_parties, by = c("dv" = "v")) %>%
  mutate(lab = as.factor(lab)) %>%
  mutate(lab = fct_reorder(lab, party_order))

# Create plot
plot_df |>
  filter(term > 2002) %>%
  ggplot(aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2013, linetype = "dotted") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0) +
  geom_point(
    size = 2,
    position = position_dodge(2),
    shape = 21, fill = "white"
  ) +
  ylab("Estimated effect of a one-percentage\npoint decline in brown employment\nbetween 1998 and 2013 (p.p.)") +
  xlab("Election") +
  theme_custom() +
  theme(legend.position = "bottom") +
  facet_wrap(~lab, ncol = 3) +
  scale_x_continuous(
    breaks = c(1998, 2002, 2005, 2009, 2013, 2017, 2021),
    labels = c("1998", "2002", "2005", "2009", "2013", "2017", "2021")
  )

ggsave(
  filename = "results/fig_F7.pdf",
  width = 8.5, height = 5
)

### END
