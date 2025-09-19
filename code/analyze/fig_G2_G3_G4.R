# Outputs:
# - Figure G.2: SOEP remote: County brownness share and individual-level partisan support over time
# - Figure G.3: SOEP remote: County brownness share and economic insecurity
# - Figure G.4: SOEP remote: County brownness share and economic insecurity after 1999
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.
# For producing the figures, we build the data frame in the script
# code/build/soep/soep_remote/03_prepare_results_dfs.R

# Clear workspace and load packages
rm(list = ls())

pacman::p_load(tidyverse, data.table)

# Load SOEP remote results dataframe
df <- fread("data/final/not_to_be_shared/data_soep_remote_results.csv")

# Custom theme
source("code/helper_functions/func_theme_custom.R")

# =============================================================================
# 1. Figure G.2: SOEP remote: County brownness share and individual-level partisan support over time
# =============================================================================

df |>
  filter(analysis == "voting") |>
  mutate(dv = ifelse(dv == "far_right", "Far-right parties", dv)) |>
  ggplot(aes(year, coef)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_errorbar(aes(ymin = (coef - 1.96 * se), ymax = (coef + 1.96 * se)), width = 0) +
  geom_point(size = 2, shape = 21, fill = "white") +
  xlab("Year") +
  ylab("Change in party support\nrel. to 2015 (p.p.)") +
  theme_custom() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(2009, 2021, 1)) +
  # scale_fill_manual(values = c("p<0.05" = "black", "p≥0.05" = "white")) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  )) +
  facet_wrap(~dv, ncol = 3)

ggsave(
    filename = "results/fig_G2.pdf",
    width = 8.5, height = 3.5
)

# =============================================================================
# 2. Figure G.3: SOEP remote: County brownness share and economic insecurity
# =============================================================================

df |>
  filter(analysis == "attitudes") |>
  mutate(dv = factor(dv, levels = c("Worried: general\neconomic situation", "Worried: personal\neconomic situation", "Worried: job security"))) |>
  ggplot(aes(year, coef)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  geom_errorbar(aes(ymin = (coef - 1.96 * se), ymax = (coef + 1.96 * se)), width = 0) +
  geom_point(shape = 21, size = 2, fill = "white") +
  xlab("Year") +
  ylab("Estimated effect") +
  theme_custom() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(2009, 2021, 1)) +
  # scale_fill_manual(values = c("p<0.05" = "black", "p≥0.05" = "white")) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  )) +
  facet_wrap(~dv, ncol = 3)

ggsave(
    filename = "results/fig_G3.pdf",
    width = 7.5, height = 3.25
)

# =============================================================================
# 3. Figure G.4: SOEP remote: County brownness share and economic insecurity after 1999
# =============================================================================

df |>
  filter(analysis == "attitudes_1999") |>
  mutate(dv = fct_relevel(dv, "Worried: general\neconomic situation", "Worried: personal\neconomic situation", "Worried: job security")) |>
  ggplot(aes(year, coef)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 1999, linetype = "dotted") +
  geom_errorbar(aes(
    ymin = (coef - 1.96 * se),
    ymax = (coef + 1.96 * se)
  ), width = 0) +
  geom_point(shape = 21, size = 2, fill = "white") +
  xlab("Year") +
  ylab("Estimated effect") +
  theme_custom() +
  scale_x_continuous(breaks = c(1995, 1999, 2000, 2005, 2010, 2015, 2020)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  )) +
  facet_wrap(~dv, ncol = 3)

ggsave(
    filename = "results/fig_G4.pdf",
    width = 7.5, height = 3.25
)

### END
