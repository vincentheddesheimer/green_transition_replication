# Outputs:
# - Figure A.1: Number of party press releases mentioning the green transition
# - Figure A.2: Sentiment in party press releases mentioning the green transition
# - Figure A.3: Share of selected issues in party press releases over time
# - Figure B.1: Salience differences: AfD vs. Other Parties
# - Figure B.2: Sentiment-weighted differences: AfD vs. Other Parties
# =============================================================================

# The data files are not publicly available.
# Contact Dr. Cornelius Erfort (cornelius.erfort@uni-wh.de) for access.
# The processed datasets were published in the following paper:
# Erfort, Cornelius, Lukas F. Stoetzer, and Heike Klüver. "The PARTYPRESS Database: A new comparative database of parties’ press releases." Research & Politics 10, no. 3 (2023).
# Refer to code/build/partypress/01_build_partypress.R for more details.

rm(list = ls())

pacman::p_load(tidyverse, data.table)

# Custom theme
source("code/helper_functions/func_theme_custom.R")

# =============================================================================
# 1. Data loading and preparation
# =============================================================================

# Load data
partypress <- fread("data/final/not_to_be_shared/data_partypress.csv")
partypress_sentiment <- fread("data/final/not_to_be_shared/data_partypress_sentiment.csv")
partypress_frames <- fread("data/final/not_to_be_shared/data_partypress_frames.csv")

# =============================================================================
# 2. Figure A.1: Number of party press releases mentioning the green transition
# =============================================================================

# Share of issue "energy" by party over time
share_energy <- partypress %>%
  group_by(party, year) %>%
  summarise(m = 100 * mean(issue_name == "Energy")) %>%
  mutate(what = "Share press releases\nclassified as energy-related")

# Add column for issue "Energiewende": binary if text contains "Energiewende"
partypress <- partypress %>%
  mutate(energiewende = ifelse(str_detect(text, "Energiewende"), 1, 0))

# Plot number of issue "Energiewende" over time
plot_df <- partypress %>%
  group_by(party, year) %>%
  summarise(
    n_energy = sum(energiewende),
    # count number of all texts
    n = n(),
    # calculate ratio
    m = n_energy / n * 100
  ) %>%
  mutate(what = "Share press releases mentioning\n\"energy transition\"") %>%
  bind_rows(share_energy)

# Plot
plot_df |>
  ggplot(aes(x = year, y = m / 100, color = what)) +
  geom_vline(xintercept = 2015.5, linetype = "dotted", color = "black") +
  geom_line() +
  labs(y = "Share of press releases") +
  # x axis should be in years
  scale_x_continuous(breaks = seq(2010, 2021, 1)) +
  theme_custom() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )
  ) +
  facet_wrap(~party, scales = "fixed") +
  scale_color_brewer(type = "qual") +
  labs(color = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

ggsave("results/fig_A1.pdf", width = 7.5, height = 5.25)


# =============================================================================
# 3. Figure A.2: Sentiment in party press releases mentioning the green transition
# =============================================================================

# Plot polarity2 for Energiewende
partypress_sentiment %>%
  filter(energiewende == 1) |>
  ggplot(aes(x = date, y = polarity2.norm)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = as.Date("2015-07-01"), linetype = "dotted") +
  # geom_point(shape = 21) +
  stat_summary_bin(aes(group = 1), fun = mean, bins = length(unique(format(partypress_sentiment$date, "%Y"))), fill = "black", size = 0.4, alpha = 0.3, shape = 19, color = "black") +
  geom_smooth(color = "black", span = 2) +
  labs(y = "Sentiment") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_custom() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )
  ) +
  facet_wrap(~party, scales = "fixed")

ggsave("results/fig_A2.pdf", width = 7.5, height = 4.75)


# =============================================================================
# 4. Figure A.3: Share of selected issues in party press releases over time
# =============================================================================

# Create plot data
plot_df <- partypress |>
  # share of issue_name over time
  group_by(party, issue_name, year) |>
  summarise(
    n = n()
  ) |>
  group_by(party, year) |>
  mutate(
    share = n / sum(n)
  ) |>
  ungroup()


# Plot share across parties over time
plot_df %>%
  filter(issue_name %in% c(
    "European Integration", "Immigration", "Government Operations",
    "Civil Rights"
  )) |>
  mutate(
    issue_name = ifelse(issue_name == "European Integration", "European\nIntegration", issue_name),
    issue_name = ifelse(issue_name == "Government Operations", "Government\nOperations", issue_name)
  ) |>
  ggplot(aes(x = year, y = share)) +
  geom_line() +
  geom_vline(xintercept = 2015.5, linetype = "dotted") +
  theme_custom() +
  labs(y = "Share of articles") +
  scale_x_continuous(breaks = seq(2010, 2021, 1)) +
  theme(legend.position = "bottom", axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )
  ) +
  # facet grid: x = party, y = issue_name
  facet_grid(party ~ issue_name, scales = "fixed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 0.4, 0.2))

ggsave("results/fig_A3.pdf", width = 7.5, height = 6.5)


# =============================================================================
# Figures B.1 + B.2: Frame Salience Differences: AfD vs. Other Parties
# =============================================================================


# Create separate plots for salience and weighted differences
# First, prepare the data
plot_data <- partypress_frames %>%
  select(year, contains("_diff"), token_ratio) %>%
  pivot_longer(
    cols = -c(year, token_ratio),
    names_to = "frame",
    values_to = "difference"
  ) %>%
  mutate(
    # Extract frame name and type
    frame_type = ifelse(str_detect(frame, "_weighted"), "Weighted (Sentiment)", "Salience"),
    frame = str_remove(frame, "_diff"),
    frame = str_remove(frame, "_weighted"),
    frame = case_when(
      frame == "Economic" ~ "Economic",
      frame == "Labor Market" ~ "Labor Market",
      frame == "Energy Security" ~ "Energy Security",
      frame == "Sovereignty" ~ "Sovereignty",
      frame == "Identity" ~ "Identity",
      frame == "Climate" ~ "Climate",
      frame == "Urgency" ~ "Urgency"
    )
  ) %>%
  # Add interpolation for missing 2015 data
  group_by(frame, frame_type) %>%
  complete(year = seq(2013, 2019, 1)) %>%
  mutate(
    difference = ifelse(
      is.na(difference) & year == 2015,
      (difference[year == 2014] + difference[year == 2016]) / 2,
      difference
    )
  ) %>%
  ungroup()

# Plot for salience differences

### Figure B.1: Salience differences

plot_data %>%
  dplyr::filter(frame_type == "Salience") %>%
  ggplot(aes(x = year, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2015.5, linetype = "dotted") +
  geom_line(color = "black") +
  theme_custom() +
  labs(
    y = "Difference in Frame Usage\n(AfD - Other Parties)",
    x = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2013, 2019, 1), limits = c(2013, 2019)) +
  theme(
    axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )
  ) +
  facet_wrap(~frame, scales = "fixed", ncol = 2) +
  theme(legend.position = "none")

ggsave("results/fig_B1.pdf", width = 7.5, height = 5.75)

### Figure B.2: Sentiment-weighted differences

plot_data %>%
  dplyr::filter(frame_type == "Weighted (Sentiment)") %>%
  ggplot(aes(x = year, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2015.5, linetype = "dotted") +
  geom_line(color = "black") +
  theme_custom() +
  labs(
    y = "Difference in Sentiment-Weighted Frame Usage\n(AfD - Other Parties)",
    x = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2013, 2019, 1), limits = c(2013, 2019)) +
  theme(
    axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )
  ) +
  facet_wrap(~frame, scales = "fixed", ncol = 2) +
  theme(legend.position = "none")

ggsave("results/fig_B2.pdf", width = 7.5, height = 5.75)

### END
