## Party Press Sentiment Analysis Script
## This script performs sentiment analysis on German party press releases using
## the ED8 emotion dictionary to analyze emotional content in energy and environment
## related communications during the green transition.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, tidytext, tm)

# Load party press release data and emotion dictionary --------------------

# read data 
partypress <- fread("data/final/not_to_be_shared/data_partypress.csv")
load("data/raw/partypress/ed8.RData")

# Create Energiewende indicator variable ----------------------------------

# Add column for issue "Energiewende": binary if text contains "Energiewende"
partypress <- partypress %>%
  mutate(energiewende = ifelse(str_detect(text, "Energiewende"), 1, 0))

# Text preprocessing and transformation ------------------------------------

## Transform data ----------------------------------------------------------

# Build comprehensive German stopwords list
# Stopwords
# Build vector of German stopwords
stopwords <- get_stopwords(language = "de", "snowball") |>
  bind_rows(get_stopwords(language = "de", "stopwords-iso")) |>
  bind_rows(get_stopwords(language = "de", "marimo")) |>
  bind_rows(get_stopwords(language = "de", "nltk")) |>
  select(word) |>
  unique() |>
  pull()

# Clean and preprocess text (this takes a while)
partypress <- partypress %>%
  mutate(
    # Lowercase
    text_clean = tolower(text),
    # create column without stopwords & punctuation
    text_clean_sw = removeWords(text_clean, stopwords),
    # Remove everything that is not a number or letter
    # text_clean = str_replace_all(text_clean,"[^a-zA-Zäöüß\\s]", " "),
    text_clean_sw = str_replace_all(text_clean_sw,"[^a-zA-Zäöüß\\s]", " "),
    # Shrink down to just one white space
    # text_clean = str_replace_all(text_clean,"[\\s]+", " "),
    text_clean_sw = str_replace_all(text_clean_sw,"[\\s]+", " "),
    # Trim whitespace
    # text_clean = str_trim(text_clean),
    text_clean_sw = str_trim(text_clean_sw),
    # Tokenize
    tokens = str_split(text_clean_sw, "\\s+")
  )

# Filter to relevant press releases --------------------------------------

# filter out relevant press releases
partypress <- partypress %>%
  filter(issue_name %in% c("Energy", "Environment") | energiewende == 1)

# Count number of terms in each text
partypress$terms <- sapply(partypress$tokens, length)

# Prepare ED8 emotion dictionary -----------------------------------------

# remove whitespace from ed8 columns
ed8 <- ed8 %>%
  mutate_all(str_trim)

# create List of vectors for each emotion category:
# Example: anger should be all words in ed8$feature that equals 1 in ed8$anger
ed8 <- list(
  anger = ed8$feature[ed8$anger == 1],
  fear = ed8$feature[ed8$fear == 1],
  disgust = ed8$feature[ed8$disgust == 1],
  sadness = ed8$feature[ed8$sadness == 1],
  joy = ed8$feature[ed8$joy == 1],
  enthusiasm = ed8$feature[ed8$enthusiasm == 1],
  pride = ed8$feature[ed8$pride == 1],
  hope = ed8$feature[ed8$hope == 1]
)

# Calculate emotion word counts ------------------------------------------

# Step 1: Count occurrences of words associated with each emotion
partypress <- partypress %>%
  mutate(
    anger = map_dbl(tokens, ~sum(. %in% ed8$anger)),
    fear = map_dbl(tokens, ~sum(. %in% ed8$fear)),
    disgust = map_dbl(tokens, ~sum(. %in% ed8$disgust)),
    sadness = map_dbl(tokens, ~sum(. %in% ed8$sadness)),
    joy = map_dbl(tokens, ~sum(. %in% ed8$joy)),
    enthusiasm = map_dbl(tokens, ~sum(. %in% ed8$enthusiasm)),
    pride = map_dbl(tokens, ~sum(. %in% ed8$pride)),
    hope = map_dbl(tokens, ~sum(. %in% ed8$hope))
  )

# Calculate emotion shares and polarity measures --------------------------

# Step 2: Calculate the share of words associated with each emotion
partypress <- partypress %>%
  mutate(
    anger_share = anger / terms,
    fear_share = fear / terms,
    disgust_share = disgust / terms,
    sadness_share = sadness / terms,
    joy_share = joy / terms,
    enthusiasm_share = enthusiasm / terms,
    pride_share = pride / terms,
    hope_share = hope / terms,
    # create positive2 and negative2 based on these 8 emotions
    positive2 = joy + enthusiasm + pride + hope,
    negative2 = anger + fear + disgust + sadness,
    polarity2 = positive2 - negative2,
    polarity2.norm = polarity2 / terms
  )

# Save sentiment analysis results
fwrite(partypress, "data/final/not_to_be_shared/data_partypress_sentiment.csv")

# END