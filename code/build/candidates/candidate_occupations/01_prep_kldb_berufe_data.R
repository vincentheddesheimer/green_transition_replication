## KLDB Occupational Data Preprocessing Script
## This script processes German occupational classification data (KLDB) to prepare
## standardized occupational titles for candidate occupation matching and analysis.
## It handles German gender notation, text normalization, and creates n-grams for
## improved matching accuracy.

rm(list = ls())

pacman::p_load(tidyverse, readxl, text2vec)

# Set data path
path_excel <- "data/raw/candidate_occupations/Alphabetisches-Verzeichnis-Berufsbenennungen-Stand01012019.xlsx"

# Load KLDB reference data from Excel ------------------------------------

kldb_reference <- readxl::read_excel(
  path_excel,
  sheet = 2, # second sheet (1-indexed in R)
  skip = 4,
  col_names = c("kldb_title", "kldb_code5")
) %>%
  filter(!is.na(kldb_title))

# Basic data exploration
kldb_reference %>%
  filter(str_detect(kldb_title, "beamte"))

# Text normalization and cleaning ----------------------------------------

# Normalize KLDB titles and store original values
kldb_reference <- kldb_reference %>%
  mutate(
    original_title = kldb_title, # Store the original title
    kldb_title = tolower(trimws(as.character(kldb_title)))
  )

# Additional text cleaning and normalization
kldb_reference <- kldb_reference %>%
  mutate(
    # Clean the text
    kldb_title = str_replace_all(kldb_title, "[0-9]", " "),
    kldb_title = str_squish(kldb_title)
  )

# Remove German stopwords
german_stopwords <- c("und", "der", "die", "das", "in", "für", "von", "mit", "bei", "im", "an", "zu", "auf")
kldb_reference <- kldb_reference %>%
  mutate(
    kldb_title = map_chr(kldb_title, function(text) {
      words <- unlist(str_split(text, "\\s+"))
      words <- words[!words %in% german_stopwords]
      paste(words, collapse = " ")
    })
  )

# Handle abbreviations and standardize terms
common_abbrev <- c(
  "u\\." = "und",
  "techn\\." = "technisch",
  "ing\\." = "ingenieur",
  "dipl\\." = "diplom"
)

for (pattern in names(common_abbrev)) {
  kldb_reference$kldb_title <- str_replace_all(kldb_reference$kldb_title, pattern, common_abbrev[pattern])
}

# Normalize job categories
kldb_reference <- kldb_reference %>%
  mutate(
    category = case_when(
      str_detect(kldb_title, "ingenieur|techniker") ~ "technical",
      str_detect(kldb_title, "verkäufer|berater|kaufmann|kauffrau") ~ "sales",
      str_detect(kldb_title, "lehrer|pädagog") ~ "education",
      TRUE ~ "other"
    )
  )

# Process German gender notation -----------------------------------------

# Function to process each title and code pair
process_title <- function(title, code, original_title = NULL) {
  # Use the original title if available for gender processing, otherwise use the pre-cleaned title
  process_text <- if (!is.null(original_title)) tolower(original_title) else title

  # Extract domain information first
  domain_info <- ""
  if (grepl("\\(.*\\)", process_text)) {
    domain_match <- str_extract(process_text, "\\(.*\\)")
    # Clean domain info extracted (remove brackets and squish)
    domain_info <- str_replace_all(domain_match, "[\\(\\)]", " ")
    domain_info <- str_replace_all(domain_info, "[[:punct:]]", " ") # Clean other punct within domain
    domain_info <- str_squish(domain_info)
    # Remove domain part from main title for further processing
    process_text <- str_replace(process_text, fixed(domain_match), "")
    process_text <- str_squish(process_text)
  }

  # Default flag, set to TRUE if a gender pattern is matched and split
  gender_split_done <- FALSE

  # Process based on gender pattern - check most complex first
  if (grepl("/r\\b", process_text) && grepl("/in\\b", process_text)) {
    # Assume complex case like "Zweite/r Nautische/r Schiffsoffizier/in"
    # Masculine: Replace ALL "/r" with "er", remove "/in"
    masculine_form <- str_replace_all(process_text, "/r\\b", "er")
    masculine_form <- str_replace_all(masculine_form, "/in\\b", "")
    # Feminine: Replace ALL "/r" with "e", replace "/in" with "in"
    feminine_form <- str_replace_all(process_text, "/r\\b", "e")
    feminine_form <- str_replace_all(feminine_form, "/in\\b", "in")

    # Clean forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form # Use masculine as base for return structure
    gender_split_done <- TRUE
  }
  # Handle parenthesized gender notations
  else if (grepl("\\w+\\(er/in\\)", process_text)) {
    # Pattern: "Word(er/in)" -> Word+er, Word+in
    base_part <- gsub("(\\w+)\\(er/in\\)", "\\1", process_text)
    masculine_form <- trimws(paste0(base_part, "er"))
    feminine_form <- trimws(paste0(base_part, "in"))

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  } else if (grepl("\\w+\\(r\\)", process_text)) {
    # Pattern: "Word(r)" -> Word+r, Word
    base_part <- gsub("(\\w+)\\(r\\)", "\\1", process_text)
    masculine_form <- trimws(paste0(base_part, "r"))
    feminine_form <- trimws(base_part) # Feminine is the base word

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  }
  # Handle other standard slash/hyphen notations
  else if (grepl("(?<![/-])beamter/-beamtin", process_text, perl = TRUE)) {
    # Special case for "beamter/-beamtin" pattern
    base_part <- gsub("(\\w+)beamter/-beamtin.*", "\\1", process_text)
    masculine_form <- trimws(paste0(base_part, "beamter"))
    feminine_form <- trimws(paste0(base_part, "beamtin"))

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  } else if (grepl("(?<![/-])ingenieu.*/-in", process_text, perl = TRUE)) {
    # Special case for "ingenieur/-in" pattern
    masculine_form <- "ingenieur"
    feminine_form <- "ingenieurin"

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  } else if (grepl("(?<![/-])beamte/beamtin", process_text, perl = TRUE)) {
    # Special case for "beamte/beamtin" pattern (without hyphen)
    masculine_form <- "beamter"
    feminine_form <- "beamtin"

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  } else if (grepl("\\w+er/-in\\b", process_text)) {
    # Handle patterns like "verkäufer/-in"
    base_part <- gsub("(\\w+)er/-in.*", "\\1", process_text)
    masculine_form <- trimws(paste0(base_part, "er"))
    feminine_form <- trimws(paste0(base_part, "in"))

    # Clean these forms
    masculine_form <- str_replace_all(masculine_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    masculine_form <- str_squish(masculine_form)
    feminine_form <- str_squish(feminine_form)
    base_form <- masculine_form
    gender_split_done <- TRUE
  } else if (grepl("/in\\b", process_text, perl = TRUE)) {
    # Pattern: "lehrer/in" (will only be reached if complex patterns above didn't match)
    base_form <- gsub("/in\\b", "", process_text)
    feminine_form <- gsub("/in\\b", "in", process_text)

    # Clean these forms
    base_form <- str_replace_all(base_form, "[[:punct:]]", " ")
    feminine_form <- str_replace_all(feminine_form, "[[:punct:]]", " ")
    base_form <- str_squish(base_form)
    feminine_form <- str_squish(feminine_form)
    gender_split_done <- TRUE
  } else {
    # Fallback: No specific gender pattern matched
    cleaned_title <- str_replace_all(process_text, "[[:punct:]]", " ") # Final punctuation clean
    cleaned_title <- str_squish(cleaned_title)

    # Add back domain info if it was present
    if (domain_info != "") {
      cleaned_title <- paste(cleaned_title, domain_info)
      cleaned_title <- str_squish(cleaned_title)
    }

    return(list(
      tibble(
        kldb_title = cleaned_title,
        kldb_code5 = code
      )
    ))
  }

  # Check if gender split was done
  if (gender_split_done) {
    # Add back domain information if it was present
    if (domain_info != "") {
      base_form <- paste(base_form, domain_info)
      feminine_form <- paste(feminine_form, domain_info)
      # Squish again after paste
      base_form <- str_squish(base_form)
      feminine_form <- str_squish(feminine_form)
    }

    return(list(
      tibble(
        kldb_title = c(base_form, feminine_form),
        kldb_code5 = c(code, code)
      )
    ))
  }
}

# Apply gender processing to all titles ----------------------------------

# Use mapply with progress bar
results <- pmap_dfr(
  list(
    kldb_reference$kldb_title,
    kldb_reference$kldb_code5,
    kldb_reference$original_title
  ),
  function(title, code, orig) {
    process_title(title, code, orig)
  },
  .progress = TRUE
)

# Combine all results
expanded_titles <- results
# Apply final squish to remove any leading/trailing/double spaces from processing
expanded_titles <- expanded_titles %>%
  mutate(kldb_title = str_squish(kldb_title))
kldb_reference <- expanded_titles

# Display sample results
results %>%
  sample_n(100) %>%
  print(n = 100)

# Create n-grams for multi-word occupational concepts --------------------

create_ngrams <- function(text, n = 2) {
  words <- unlist(str_split(text, "\\s+"))
  if (length(words) < n) {
    return(text)
  }

  ngrams <- character(length(words) - n + 1)
  for (i in 1:(length(words) - n + 1)) {
    ngrams[i] <- paste(words[i:(i + n - 1)], collapse = "_")
  }

  return(paste(c(words, ngrams), collapse = " "))
}

kldb_reference <- kldb_reference %>%
  mutate(
    text_with_ngrams = map_chr(kldb_title, ~ create_ngrams(.x, n = 2))
  )

# Handle sector/domain information with brackets
kldb_reference <- kldb_reference %>%
  mutate(
    has_domain = str_detect(kldb_title, "\\(.*\\)"),
    domain = str_extract(kldb_title, "(?<=\\().*(?=\\))"),
    clean_title = str_replace(kldb_title, "\\s*\\(.*\\)", "")
  )

# Save preprocessed data for embedding -----------------------------------

write_csv(kldb_reference, "data/intermediate/candidate_occupations/preprocessed_kldb_for_embedding.csv")
