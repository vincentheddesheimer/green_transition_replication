## Party Press Frame Analysis Script
## This script performs frame analysis on German party press releases using
## a custom dictionary to identify different thematic frames in energy and
## environment communications, with particular focus on AfD vs other parties.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, quanteda)

# Load sentiment-analyzed party press data -------------------------------

# Load data
partypress_sentiment <- fread("data/final/not_to_be_shared/data_partypress_sentiment.csv")

# Define frame dictionary for thematic analysis ---------------------------

# Define frame_dict dictionary with thematic categories and German keywords
frame_dict <- dictionary(list(
  Economic = c("kosten*", "euro", "strompreis*", "teuer*", "schulden", "haushalt*", "steuer*", "bezahlbar", "sparen", "ausgaben", "subvention*", "belastung", "finanz*", "preis*"),
  `Labor Market` = c("arbeitsplatz*", "industrie*", "mittelstand", "wettbewerb*", "beschäftig*", "strukturwandel", "wirtschaft*", "produktion*", "innovation*", "technolog*", "forschung", "konzern*", "invest*"),
  `Energy Security` = c("energieversorgung", "netz*", "strom*", "speicher*", "infrastruktur", "energieeffizienz", "energiepreis*", "energieträger", "energiewirtschaft", "energieintensiv*", "planungssicherheit"),
  `Sovereignty` = c("national*", "souverän*", "europa*", "eu", "bundes*", "volkswirtschaft", "unabhängig", "grenzen", "integration", "öffentlichkeit", "autonomie"),
  `Identity` = c("gesellschaft*", "kultur", "identität", "heimat", "angst", "tradition", "werte", "elite", "afd", "überfremdung", "migration", "integration", "fremd*", "bürger*", "sozial*"),
  `Climate` = c("klimaschutz", "klimawandel", "klimaziele", "co2", "emission*", "erneuerbar*", "nachhalt*", "umweltschutz", "öko*", "solar*", "wind*", "klimakrise", "atom*", "fossil*", "pariser", "emissionshandel"),
  `Urgency` = c("jetzt", "dringend", "zukunft*", "krise", "verpassen", "versagen", "verzögerung", "scheitern", "generationen", "risiko", "kippunkt", "verlangsamen", "verlieren", "realität", "langfristig", "warnung")
))

# Create corpus and document-feature matrix ------------------------------

# Create corpus and dfm for frame analysis
partypress_corpus <- corpus(partypress_sentiment$text, docvars = data.frame(
  id = partypress_sentiment$id,
  date = partypress_sentiment$date,
  party = partypress_sentiment$party,
  issue_name = partypress_sentiment$issue_name,
  energiewende = partypress_sentiment$energiewende,
  polarity2.norm = partypress_sentiment$polarity2.norm
))

# Tokenize corpus
partypress_tokens <- tokens(partypress_corpus)

# Create document-feature matrix
partypress_dfm <- dfm(partypress_tokens)

# Apply dictionary lookup for frame detection
partypress_frames <- dfm_lookup(partypress_dfm, dictionary = frame_dict)

# Convert to data frame and calculate frame shares ----------------------

# Convert to data frame for analysis
partypress_frames_df <- convert(partypress_frames, to = "data.frame") %>%
  mutate(
    id = partypress_sentiment$id,
    date = partypress_sentiment$date,
    party = partypress_sentiment$party,
    issue_name = partypress_sentiment$issue_name,
    energiewende = partypress_sentiment$energiewende,
    polarity2.norm = partypress_sentiment$polarity2.norm,
    total_words = ntoken(partypress_tokens)
  ) %>%
  select(-doc_id) %>%
  # Calculate frame shares as percentage of total words
  mutate(across(Economic:Urgency, ~ .x / total_words * 100))

# Calculate weighted frame scores (frame share * sentiment) --------------

# Calculate weighted frame scores (frame share * sentiment)
partypress_frames_df <- partypress_frames_df %>%
  mutate(
    Economic_weighted = Economic * polarity2.norm,
    `Labor Market_weighted` = `Labor Market` * polarity2.norm,
    `Energy Security_weighted` = `Energy Security` * polarity2.norm,
    Sovereignty_weighted = Sovereignty * polarity2.norm,
    Identity_weighted = Identity * polarity2.norm,
    Climate_weighted = Climate * polarity2.norm,
    Urgency_weighted = Urgency * polarity2.norm
  )

# Create comparison between AfD and other parties -----------------------

# Create comparison between AfD and other parties
frame_comparison <- partypress_frames_df %>%
  filter(issue_name %in% c("Energy", "Environment") | energiewende == 1) %>%
  mutate(
    party_group = ifelse(party == "AfD", "AfD", "Other Parties"),
    year = lubridate::year(date)
  ) %>%
  # Calculate total tokens per year for other parties
  group_by(year) %>%
  mutate(
    total_tokens_other = sum(total_words[party != "AfD"]),
    # Calculate weight for each party (share of total tokens in other parties)
    weight = ifelse(party == "AfD", 1, total_words / total_tokens_other)
  ) %>%
  ungroup() %>%
  # Calculate weighted means by group and year
  group_by(party_group, year) %>%
  summarise(
    across(
      c(Economic:Urgency, Economic_weighted:Urgency_weighted),
      ~ weighted.mean(.x, w = weight, na.rm = TRUE)
    ),
    total_tokens = sum(total_words, na.rm = TRUE)
  ) %>%
  ungroup() |>
  arrange(party_group, year)

# Calculate differences between AfD and other parties --------------------

# Calculate differences between AfD and other parties
frame_differences <- frame_comparison %>%
  pivot_wider(
    names_from = party_group,
    values_from = c(Economic:Urgency, Economic_weighted:Urgency_weighted, total_tokens),
    names_sep = "_"
  ) %>%
  mutate(
    Economic_diff = Economic_AfD - `Economic_Other Parties`,
    `Labor Market_diff` = `Labor Market_AfD` - `Labor Market_Other Parties`,
    `Energy Security_diff` = `Energy Security_AfD` - `Energy Security_Other Parties`,
    Sovereignty_diff = Sovereignty_AfD - `Sovereignty_Other Parties`,
    Identity_diff = Identity_AfD - `Identity_Other Parties`,
    Climate_diff = Climate_AfD - `Climate_Other Parties`,
    Urgency_diff = Urgency_AfD - `Urgency_Other Parties`,
    Economic_weighted_diff = Economic_weighted_AfD - `Economic_weighted_Other Parties`,
    `Labor Market_weighted_diff` = `Labor Market_weighted_AfD` - `Labor Market_weighted_Other Parties`,
    `Energy Security_weighted_diff` = `Energy Security_weighted_AfD` - `Energy Security_weighted_Other Parties`,
    Sovereignty_weighted_diff = Sovereignty_weighted_AfD - `Sovereignty_weighted_Other Parties`,
    Identity_weighted_diff = Identity_weighted_AfD - `Identity_weighted_Other Parties`,
    Climate_weighted_diff = Climate_weighted_AfD - `Climate_weighted_Other Parties`,
    Urgency_weighted_diff = Urgency_weighted_AfD - `Urgency_weighted_Other Parties`,
    # Add token ratio for reference
    token_ratio = `total_tokens_AfD` / `total_tokens_Other Parties`
  )

# Save frame analysis results
fwrite(frame_differences, "data/final/not_to_be_shared/data_partypress_frames.csv")

# END