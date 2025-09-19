# =============================================================================
# Sanity Checks for Main County Dataset
#
# This script performs several sanity checks on the final merged dataset
# (data_main.csv) to ensure data integrity, consistency, and plausibility.
# =============================================================================

# Load required packages
pacman::p_load(tidyverse, data.table, testthat)

# Load the main dataset
df <- fread("data/final/data_main.csv")

# =============================================================================
# CHECK 1: MISSING VALUES IN KEY VARIABLES
# =============================================================================
# Description: This check examines the extent of missing values (NAs) in key
# outcome, treatment, and control variables for each election year. Excessive
# missingness could indicate issues with data sources or the merging process.

key_vars <- c(
    "afd", "brownness_share", "unemp_rate_regstat", "hh_income_vgrdl",
    "share_manufacturing_regstat", "auslanderanteil_inkar", "gdp_pc_ardeco"
)

missing_summary <- df %>%
    group_by(election_year) %>%
    summarise(across(all_of(key_vars), ~ sum(is.na(.))))

cat("\nCheck 1: Missing values summary by year:\n")
print(missing_summary)

# =============================================================================
# CHECK 2: UNIQUE COUNTIES PER YEAR
# =============================================================================
# Description: This check verifies that the number of unique counties is
# consistent across election years. The expected number is around 400-401.
# Significant deviations could point to problems with data filtering or joins.

county_counts <- df %>%
    group_by(election_year) %>%
    summarise(n_counties = n_distinct(county))

cat("\nCheck 2: Number of unique counties per election year:\n")
print(county_counts)

# =============================================================================
# CHECK 3: DUPLICATE COUNTY-YEAR OBSERVATIONS
# =============================================================================
# Description: This check ensures that there are no duplicate rows for any
# county-year combination, which would violate the assumption of unique
# observations for panel data analysis.

duplicate_rows <- df %>%
    count(county, election_year) %>%
    filter(n > 1)

cat("\nCheck 3: Duplicate county-election_year rows:\n")
if (nrow(duplicate_rows) == 0) {
    cat("No duplicate county-year rows found.\n")
} else {
    print(duplicate_rows)
}

# =============================================================================
# CHECK 4: RANGE AND DISTRIBUTION OF KEY VARIABLES
# =============================================================================
# Description: This check provides summary statistics for key variables to
# identify any implausible values, such as percentages outside the 0-100 range,
# or extreme outliers that might skew the analysis.

range_summary <- df %>%
    select(
        election_year, afd, gruene,
        brownness_share, brown1_share,
        unemp_rate_regstat
    ) %>%
    group_by(election_year) %>%
    summarise(across(everything(), list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE)
    )), .groups = "drop") %>%
    pivot_longer(-election_year) %>%
    separate(name, into = c("variable", "stat"), sep = "_", extra = "merge") %>%
    pivot_wider(names_from = stat, values_from = value)


cat("\nCheck 4: Range and distribution of key variables:\n")
print(range_summary, n = 50)

# Specific tests for ranges
# test_that("Vote shares are within [0, 100]", {
#   expect_gte(min(df$afd, na.rm = TRUE), 0)
#   expect_lte(max(df$afd, na.rm = TRUE), 100)
#   expect_gte(min(df$gruene, na.rm = TRUE), 0)
#   expect_lte(max(df$gruene, na.rm = TRUE), 100)
# })
#
# test_that("Brownness shares are within [0, 1]", {
#   expect_gte(min(df$brownness_share, na.rm = TRUE), 0)
#   expect_lte(max(df$brownness_share, na.rm = TRUE), 1)
# })
#
# test_that("Unemployment rate is plausible", {
#   expect_gte(min(df$unemp_rate_regstat, na.rm = TRUE), 0)
#   # A max of 1 would be extreme but technically possible.
#   expect_lte(max(df$unemp_rate_regstat, na.rm = TRUE), 1)
# })


# =============================================================================
# CHECK 5: CROSS-VALIDATION OF VARIABLES FROM DIFFERENT SOURCES
# =============================================================================
# Description: This check compares similar variables from different data sources
# to ensure they are consistent. Here, we correlate two different measures of
# unemployment rate. A high correlation would increase confidence in the data.

unemployment_correlation <- df %>%
    summarise(correlation = cor(unemp_rate_regstat, arbeitslosenquote_inkar, use = "pairwise.complete.obs"))

cat("\nCheck 5: Correlation between unemployment rate from RegStat and INKAR:\n")
print(unemployment_correlation)

# test_that("Unemployment measures are highly correlated", {
#   # We expect a very high correlation
#   expect_gt(unemployment_correlation$correlation, 0.9)
# })

# =============================================================================
# CHECK 6: CONSISTENCY OF COUNTY IDENTIFIERS
# =============================================================================
# Description: This check verifies that each county code (`county`) maps to
# a single, unique county name (`county_name`). Inconsistencies could arise
# from merging errors or issues in the raw data.

county_name_consistency <- df %>%
    group_by(county) %>%
    summarise(n_distinct_names = n_distinct(county_name)) %>%
    filter(n_distinct_names > 1)

cat("\nCheck 6: County codes with more than one county name:\n")
if (nrow(county_name_consistency) == 0) {
    cat("Each county code maps to a single county name.\n")
} else {
    print(county_name_consistency)
}
# test_that("County codes map to unique names", {
#   expect_equal(nrow(county_name_consistency), 0)
# })


# =============================================================================
# CHECK 7: EAST-WEST GERMANY DIFFERENCES
# =============================================================================
# Description: A quick check to see if there are systematic differences
# between East and West Germany for key variables, which is often expected in
# German socio-economic data. This is more of a plausibility check.

df <- df %>%
    mutate(east_germany = ifelse(state %in% c("12", "13", "14", "15", "16", "11"), 1, 0))

east_west_summary <- df %>%
    filter(election_year == 2021) %>%
    group_by(east_germany) %>%
    summarise(
        avg_afd_vote = mean(afd, na.rm = TRUE),
        avg_brownness_share = mean(brownness_share, na.rm = TRUE),
        avg_unemp_rate = mean(unemp_rate_regstat, na.rm = TRUE),
        avg_hh_income = mean(hh_income_vgrdl, na.rm = TRUE)
    )

cat("\nCheck 7: East-West differences in 2021:\n")
print(east_west_summary)
cat("\n(1 = East Germany, 0 = West Germany)\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================
