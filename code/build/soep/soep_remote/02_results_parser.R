## SOEP Remote Results Parser
## This script parses .txt files from SOEP remote access log output and converts them to CSV format
## for further analysis. It handles multiple regression tables with different output formats.

rm(list = ls())

pacman::p_load(tidyverse, data.table, pbapply)

# Specify directory that contains the text files
path <- "data/intermediate/soep_remote/"

# Functions ---------------------------------------------------------------

## Function to extract all tables from log output
## Handles different table formats (with/without R² statistics)

extract_all_tables <- function(log_file) {
  
  # Check if the log file includes R² statistics by looking for "N r2" pattern
  stats_included <- any(grepl("N r2", log_file))
  
  if (stats_included) {
    # Find table boundaries when R² statistics are included
    print("Stats included")
    table_starts <- str_which(log_file, pattern = fixed("            &           b&          se\\\\"))
    table_ends <- str_which(log_file, pattern = fixed("r2          &"))
  } else {
    # Find table boundaries when R² statistics are not included
    print("Stats not included")
    table_starts <- str_which(log_file, pattern = fixed("            &           b&          se\\\\"))
    table_ends <- str_which(log_file, pattern = fixed("_cons       &"))
  }
  
  # Iterate over the table starts and ends, extracting and parsing each table
  parsed_tables <- list()
  for (i in seq_along(table_starts)) {
    # Extract table text between start and end markers
    table_text <- text[table_starts[i]:table_ends[i]]
    # Remove hash symbols that might interfere with parsing
    table_text <- str_replace_all(table_text, "#", "")
    # Create temporary CSV file for parsing
    table_file <- tempfile(fileext = ".csv")
    writeLines(table_text, table_file)
    # Parse table using read.table with pipe separator
    parsed_table <- read.table(table_file, sep = "&", header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
    parsed_tables[[i]] <- parsed_table
    unlink(table_file) # Delete the temporary file
  }
  
  ## Validate table parsing: first cell should be "=" for properly parsed tables
  
  for (i in seq_along(parsed_tables)) {
    temp <- parsed_tables[[i]]
    
    if (temp[1,1] != "            ") {
      message("Error: table not parsed correctly (", i, ")")
    }
  }
  
  ## Return parsed tables
  
  return(parsed_tables)
}

## Function to extract table information (dependent variable names)
## Extracts the descriptive text that identifies what each table represents

extract_table_info <- function(log_file) {
  
  # Check if the log file has the R² statistic
  stats_included <- any(grepl("N r2", log_file))
  
  if (stats_included) {
    # Find table boundaries when R² statistics are included
    table_starts <- str_which(log_file, pattern = fixed("            &           b&          se\\\\"))
    table_ends <- str_which(log_file, pattern = fixed("r2          &"))
  } else {
    # Find table boundaries when R² statistics are not included
    table_starts <- str_which(log_file, pattern = fixed("            &           b&          se\\\\"))
    table_ends <- str_which(log_file, pattern = fixed("_cons       &"))
  }
  
  ## Extract the table info (dependent variable descriptions)
  
  table_info <- list()
  
  for (i in seq_along(table_starts)) {
    # Extract the line 3 lines before each table start (contains table description)
    table_info[[i]] <- log_file[table_starts[i] - 3]
  }
  
  # Convert list to vector
  table_info <- unlist(table_info)
  
  ## Extract relevant part of the description (starting from character 19)
  
  table_info <- substr(table_info, 19, nchar(table_info))
  
  ## Return table information
  
  return(table_info)
}

## Function to parse a single table into a structured data frame
## Handles different table formats and extracts coefficients, standard errors, R², and observations

parse_one_table <- function(one_table) {
  
  # Check if the last row contains R² statistic or number of observations
  last_row <- one_table[nrow(one_table), ]
  
  # Identify if the last row contains R² or observations
  if (grepl("r2", last_row[1], ignore.case = TRUE)) {
    # Extract R² value and remove the R² row from table
    r2_value <- as.numeric(str_replace_all(last_row[2], "\\\\", ""))
    one_table <- one_table[-nrow(one_table), ]
    
    # Check if the new last row contains the number of observations
    last_row <- one_table[nrow(one_table), ]
    if (grepl("N", last_row[1], ignore.case = TRUE)) {
      obs_value <- as.numeric(str_replace_all(last_row[2], "\\\\", ""))
      one_table <- one_table[-nrow(one_table), ]
    } else {
      obs_value <- NA
    }
    
    # Parse the remaining table with R² and observations
    df <- one_table %>%
      dplyr::rename(iv = 1, coef = 2, se = 3) %>%
      # Remove first line (header)
      slice(-1) %>%
      mutate(se = as.numeric(str_replace_all(se, "\\\\", "")),
             coef = as.numeric(coef),
             r2 = r2_value,
             obs = obs_value)
    
  } else if (grepl("N", last_row[1], ignore.case = FALSE)) {
    # Extract number of observations and remove the observations row
    obs_value <- as.numeric(str_replace_all(last_row[2], "\\\\", ""))
    one_table <- one_table[-nrow(one_table), ]
    
    # Parse the remaining table with observations only
    df <- one_table %>%
      dplyr::rename(iv = 1, coef = 2, se = 3) %>%
      # Remove first line (header)
      slice(-1) %>%
      mutate(se = as.numeric(str_replace_all(se, "\\\\", "")),
             coef = as.numeric(coef),
             obs = obs_value)
    
  } else {
    # Parse table without R² or observations
    df <- one_table %>%
      dplyr::rename(iv = 1, coef = 2, se = 3) %>%
      # Remove first line (header)
      slice(-1) %>%
      mutate(se = as.numeric(str_replace_all(se, "\\\\", "")),
             coef = as.numeric(coef))
  }
  
  return(df)
}

## Main function to orchestrate the entire parsing process
## Extracts tables, table info, parses each table, and combines results

main <- function(file_content) {
  
  ## Extract all tables from the log file
  
  tables <- extract_all_tables(file_content)
  
  ## Extract table information (dependent variable descriptions)
  
  table_info <- extract_table_info(file_content)
  
  ## Parse each table into structured data frames
  
  parsed_tables <- pblapply(tables, parse_one_table)
  
  ## Combine all tables and add table information (dependent variable names)
  
  parsed_tables_final <- lapply(seq_along(parsed_tables), function(i) {
    temp_tab <- parsed_tables[[i]]
    
    # Add dependent variable information to each table
    temp_tab <- temp_tab %>%
      mutate(dv = table_info[i])
    
    return(temp_tab)
  }) %>% reduce(bind_rows)
  
  ## Return combined results
  
  return(parsed_tables_final)
}

# Process voting data -----------------------------------------------------

# Read the voting results text file
file_path <- paste0(path, "voting.txt")
text <- readLines(file_path)

# Parse voting results
out <- main(text)

# Write voting results to CSV
fwrite(out, paste0(path, "voting.csv"))

# Process attitudes data --------------------------------------------------

# Read the attitudes results text file
file_path <- paste0(path, "attitudes.txt")
text <- readLines(file_path)

# Parse attitudes results
out <- main(text)

# Write attitudes results to CSV
fwrite(out, paste0(path, "attitudes.csv"))

# Process 1999 attitudes data (without immigration) ----------------------

# Read the 1999 attitudes results text file
file_path <- paste0(path, "attitudes_1999_woimmig.txt")
text <- readLines(file_path)

# Parse 1999 attitudes results
out <- main(text)

# Write 1999 attitudes results to CSV
fwrite(out, paste0(path, "attitudes_1999_woimmig.csv"))

# Process status pooled data ---------------------------------------------

# Read the status pooled results text file
file_path <- paste0(path, "status_pooled.txt")
text <- readLines(file_path)

# Parse status pooled results
out <- main(text)

# Write status pooled results to CSV
fwrite(out, paste0(path, "status_pooled.csv"))


# Process status pooled data with R² statistics --------------------------

# Read the text file
file_path <- "data/intermediate/soep_remote/status_pooled_r2.txt"
text <- readLines(file_path)

out <- main(text)

# write
fwrite(out, "data/intermediate/soep_remote/status_pooled_r2.csv")


### END