# Outputs:
# - Figure C.1: Far-right parties competing in German federal elections
# =============================================================================

rm(list = ls())

pacman::p_load(tidyverse, data.table)

# Load function
source("code/helper_functions/func_theme_custom.R")

# Load data
df <- fread("data/raw/county/federal_cty_harm.csv")

# See which far-right parties gained votes over time
plot_df <- df |>
  select(election_year, afd:dsu) |>
  # get total votes by election_year for each party
  group_by(election_year) |>
  summarise(across(afd:dsu, mean, na.rm = TRUE)) |>
  pivot_longer(
    cols = c(afd:dsu),
    names_to = "party",
    values_to = "votes"
  ) |>
  ungroup()

# Plot
plot_df |>
  # First filter to only keep parties that have at least one non-NA value
  group_by(party) |>
  filter(any(!is.na(votes) & votes != 0)) |>
  ungroup() |>
  # rename parties
  mutate(party = case_when(
    party == "afd" ~ "AfD",
    party == "iii_weg" ~ "III. Weg", 
    party == "npd" ~ "NPD",
    party == "rep" ~ "Republikaner", 
    party == "die_rechte" ~ "Die Rechte",
    party == "dvu" ~ "DVU",
    party == "bf_b" ~ "BF/B",
    party == "ddd" ~ "DDD",
    party == "dsu" ~ "DSU"
  )) |>
  ggplot(aes(x = as.character(election_year), y = party, fill = !is.na(votes) & votes != 0)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
  theme_custom() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(x = "Election", y = "Party") +
  scale_y_discrete(limits = c("Republikaner", "NPD", "III. Weg", "DVU", "DSU", "Die Rechte", "DDD", "BF/B", "AfD"))

ggsave(
    filename = "results/fig_C1.pdf",
    width = 7, height = 4
)

### END
