"""
KLDB Occupational Matching Script
This script processes candidate occupation data and matches it to KLDB classifications
using sentence embeddings. It handles German text processing, compound word splitting,
and computes similarity scores for occupation matching.
"""

import re
import os
import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer, util
from german_compound_splitter import comp_split

# Load and prepare data ---------------------------------------------------


def load_csv_with_encoding(file_path):
    """Try different encodings to load the CSV file."""
    encodings = ["utf-8", "latin1", "iso-8859-1", "cp1252"]

    for encoding in encodings:
        try:
            print(f"Trying to load with encoding: {encoding}")
            df = pd.read_csv(
                file_path,
                encoding=encoding,
                quoting=1,  # csv.QUOTE_ALL
                escapechar="\\",
                na_values=[""],
                on_bad_lines="skip",  # Skip problematic lines
            )
            print(f"Successfully loaded with {encoding} encoding")
            return df
        except UnicodeDecodeError as e:
            print(f"Failed with {encoding}: {e}")
            continue
        except Exception as e:
            print(f"Other error with {encoding}: {e}")
            continue

    # If all encodings fail, try with error handling
    try:
        print("Trying with error handling...")
        df = pd.read_csv(
            file_path,
            encoding="utf-8",
            quoting=1,
            escapechar="\\",
            na_values=[""],
            on_bad_lines="skip",
            encoding_errors="replace",  # Replace problematic characters
        )
        print("Successfully loaded with error handling")
        return df
    except Exception as e:
        print(f"All attempts failed: {e}")
        raise


# Load the main dataset
cf = load_csv_with_encoding("data/intermediate/candidate_occupations/prepped_data.csv")
print(f"Loaded main dataset with {len(cf)} records.")

# Check for umlauts in main dataset
umlaut_chars = ["ä", "ö", "ü", "ß"]
print("\nChecking umlauts in main dataset:")
for char in umlaut_chars:
    count = sum(
        cf[col].astype(str).str.contains(char, na=False).sum()
        for col in ["occ_1", "occ_2", "occ_3", "occ_4"]
    )
    print(f"Found {count} occurrences of '{char}'")

# Text processing configuration -------------------------------------------

# Optional: Split compound words
SPLIT_COMPOUNDS = True  # Set this to True to enable compound word splitting
ONLY_NOUNS = True  # Set this to True to only split nouns
MASK_UNKNOWN = True  # Set this to True to mask unknown words

if SPLIT_COMPOUNDS:
    input_file = "data/raw/candidate_occupations/german_utf8.dic"  # Using UTF-8 version of the dictionary
    ahocs = comp_split.read_dictionary_from_file(input_file)


def safe_dissect(word, ahocs):
    """Safely dissect a compound word, returning the original word if no split is found."""
    try:
        results = comp_split.dissect(
            word,
            ahocs,
            make_singular=True,
            only_nouns=ONLY_NOUNS,
            mask_unknown=MASK_UNKNOWN,
        )
        if not results:  # If no split was found
            return word
        # Remove "__unknown__" from results and join with spaces (this is from the mask_unknown option)
        results = [r for r in results if r != "__unknown__"]
        if not results:  # If all parts were unknown
            return word
        return " ".join(results).lower()
    except Exception as e:
        print(f"Error processing word '{word}': {str(e)}")
        return word


# Process occupation data -------------------------------------------------

# Combine all occupation fields (occ_1, occ_2, occ_3, occ_4) into a single list
all_occupations = []
original_occupations = []  # Store original titles
for col in ["occ_1", "occ_2", "occ_3", "occ_4"]:
    # Filter out missing values and add non-empty occupations to the list
    occupations = cf[col].dropna().astype(str)
    occupations = occupations[occupations != "nan"].str.strip()  # Don't lowercase yet

    # Store original titles before any processing
    original_occupations.extend(occupations.tolist())

    # Now lowercase for processing
    occupations = occupations.str.lower()

    # Optionally split compound words
    if SPLIT_COMPOUNDS:
        occupations = occupations.apply(lambda x: safe_dissect(x, ahocs))

    all_occupations.extend(occupations.tolist())

# Create a mapping of processed to original titles
title_mapping = dict(zip(all_occupations, original_occupations))

# Extract unique occupations from all occupation fields
unique_occupations = pd.Series(all_occupations).drop_duplicates().reset_index(drop=True)
print(
    f"Found {len(unique_occupations)} unique occupations across all occupation fields."
)

# Load KLDB reference data ------------------------------------------------

# Load preprocessed KLDB reference data (should contain columns "kldb_title" and "kldb_code5")
kldb = load_csv_with_encoding("data/raw/candidate_occupations/kldb_for_embedding.csv")

# Check for umlauts in KLDB dataset
print("\nChecking umlauts in KLDB dataset:")
for char in umlaut_chars:
    count = kldb["kldb_title"].astype(str).str.contains(char, na=False).sum()

# Store original KLDB titles
kldb["original_kldb_title"] = kldb["kldb_title"]

# Optionally split compound words in KLDB titles
if SPLIT_COMPOUNDS:
    kldb["kldb_title"] = kldb["kldb_title"].apply(lambda x: safe_dissect(x, ahocs))

# Compute embeddings and match occupations --------------------------------

# Use a German-capable model; here we use xlm-roberta-large
model = SentenceTransformer("xlm-roberta-large")

# File paths for embeddings
kldb_embeddings_file = "data/raw/candidate_occupations/kldb_embeddings.npy"

# Compute or load KLDB embeddings
if os.path.exists(kldb_embeddings_file):
    kldb_embeddings = np.load(kldb_embeddings_file)
    print("Loaded KLDB embeddings from disk.")
else:
    kldb_list = kldb["kldb_title"].tolist()
    kldb_embeddings = model.encode(kldb_list, batch_size=32, show_progress_bar=True)
    np.save(kldb_embeddings_file, kldb_embeddings)
    print("Computed and saved KLDB embeddings.")

# Encode unique occupations from the combined occupation fields
occ_list = unique_occupations.tolist()
occ_embeddings = model.encode(occ_list, batch_size=32, show_progress_bar=True)

# Compute cosine similarity between occupations and KLDB titles
cos_sim_matrix = util.cos_sim(occ_embeddings, kldb_embeddings)
cos_sim_matrix = cos_sim_matrix.cpu().numpy()  # Convert to numpy array

# Get the best match for each occupation
best_indices = np.argmax(cos_sim_matrix, axis=1)
best_similarities = np.max(cos_sim_matrix, axis=1)

# Create matching results dataframe ----------------------------------------

# Create a dataframe with the matching results
matching_results = pd.DataFrame(
    {
        "occupation": unique_occupations,
        "original_occupation": [title_mapping[occ] for occ in unique_occupations],
        "matched_kldb_code": kldb["kldb_code5"].iloc[best_indices].values,
        "matched_kldb_title": kldb["kldb_title"].iloc[best_indices].values,
        "original_matched_kldb_title": kldb["original_kldb_title"]
        .iloc[best_indices]
        .values,
        "similarity_score": best_similarities,
    }
)

# Sort by similarity score (descending) to see best matches first
matching_results = matching_results.sort_values(by="similarity_score", ascending=False)

# Save matching results ---------------------------------------------------

matching_results.to_csv(
    "data/intermediate/candidate_occupations/unique_occupation_matches.csv",
    index=False,
    encoding="utf-8",
)
