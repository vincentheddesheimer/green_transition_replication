## Parser for SOEP Remote Crosstab Results
## This script parses ISEI (International Socio-Economic Index) data from SOEP remote crosstab output
## and converts it into a structured CSV format for further analysis

rm(list = ls())

# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)

# Functions ---------------------------------------------------------------

parse_isei_table <- function(log_file) {
  # Find the lines where the table begins and ends
  # Look for the separator line pattern that marks table boundaries
  table_start <- str_which(log_file, pattern = fixed("---------+--------------------------------------------------"))
  table_end <- str_which(log_file, pattern = fixed("---------+--------------------------------------------------"))
  
  # Extract the table content between the first and second separator lines
  table_content <- log_file[(table_start[1] + 1):(table_end[2] - 1)]
  
  # Remove empty lines and header rows that don't contain actual data
  table_content <- table_content[!str_detect(table_content, "county")]
  table_content <- table_content[!str_detect(table_content, "---------")]
  table_content <- table_content[!str_detect(table_content, "Total")]
  
  # Parse each line of the table content
  parsed_data <- lapply(table_content, function(line) {
    # Split the line by pipe character to separate county code from ISEI data
    parts <- str_split(line, "\\|")[[1]]
    
    # Extract county code (first part before the pipe)
    county <- str_trim(parts[1])
    
    # Extract ISEI value (second part after the pipe)
    isei_part <- str_trim(parts[2])
    # Split by whitespace and take the second value (after the "1" indicator)
    isei <- str_split(isei_part, "\\s+")[[1]][2]
    
    # Convert values to appropriate data types
    county <- as.integer(county)
    isei <- as.numeric(isei)
    
    # Return a data frame with the parsed values
    return(data.frame(county = county, isei_combined_2015 = isei))
  })
  
  # Combine all parsed rows into a single data frame
  df <- bind_rows(parsed_data)
  
  return(df)
}

# Main -------------------------------------------------------------------

# Read the text file containing the SOEP remote crosstab output
file_path <- "data/intermediate/soep_remote/isei_2015.txt"
text <- readLines(file_path)

# Parse the table to extract county-level ISEI data
out <- parse_isei_table(text)

# Write the parsed data to CSV for use in subsequent analysis
fwrite(out, "data/intermediate/county_isei_2015.csv")

### END