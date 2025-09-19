# =============================================================================
# This script creates Table 1 showing the summary statistics for the county-level
# variables in 2013.
#
# Key outputs:
# - Table 1 showing summary statistics for county-level variables in 2013
# =============================================================================

rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, data.table, kableExtra)

# Load data
df <- fread("data/final/data_main.csv",
    colClasses = c("county" = "character")
) %>%
    mutate(county = str_pad(county, 5, side = "left", pad = "0"))

# Create 2013 brown employment variables for each county
df <- df %>%
    group_by(county) %>%
    mutate(
        observed_2013 = any(election_year == 2013),
        brownness_share_2013 = ifelse(observed_2013, brownness_share[election_year == 2013], NA),
        brown1_share_2013 = ifelse(observed_2013, brown1_share[election_year == 2013], NA),
        brown0_3_share_2013 = ifelse(observed_2013, brown0_3_share[election_year == 2013], NA),
        brown0_5_share_2013 = ifelse(observed_2013, brown0_5_share[election_year == 2013], NA)
    ) %>%
    ungroup()


# =============================================================================
# 1. SUMMARY STATISTICS TABLE
# =============================================================================

# Define party variables and convert to percentages
party_vars <- c("spd", "fdp", "gruene", "linke_pds", "afd", "cdu_csu", "far_right")
df <- df %>%
    mutate(across(all_of(party_vars), ~ .x * 100))


# Create labels for party variables
party_labels <- data.frame(
    v = party_vars,
    lab = c("SPD", "FDP", "Green Party", "Left Party", "AfD", "CDU/CSU", "Far-right parties")
) %>%
    mutate(order = c(5, 3, 6, 7, 1, 4, 2)) %>%
    arrange(order) %>%
    mutate(lab = fct_reorder(lab, order))

# Define socioeconomic variables
socio_vars <- c(
    "brown0_3_share_2013",
    "hh_income_vgrdl", "gdp_pc_ardeco",
    "arbeitslosenquote_inkar", "share_manufacturing_regstat",
    "pop_density", "population",
    "schulabganger_mit_allgemeiner_hochschulreife_inkar",
    "auslanderanteil_inkar"
)

# Create labels for socioeconomic variables
socio_labels <- data.frame(
    v = socio_vars,
    lab = c(
        "Employees in brown occupations (%)",
        "Household income", "GDP per capita",
        "Unemployment rate (%)",
        "Share manufacturing (%)", "Population density", "Population",
        "Share of high school graduates", "Share of foreigners"
    )
) %>%
    mutate(order = c(1, 4, 3, 2, 5, 6, 7, 8, 9)) %>%
    arrange(order) %>%
    mutate(lab = fct_reorder(lab, order))

# Prepare 2013 data for summary statistics
df_2013 <- df %>%
    filter(election_year == 2013) %>%
    dplyr::select(county, state, election_year, all_of(party_vars), all_of(socio_vars)) %>%
    mutate(brown0_3_share_2013 = brown0_3_share_2013 * 100)
# Calculate summary statistics
sum_df <- df_2013 %>%
    dplyr::select(all_of(c(socio_vars, party_vars))) %>%
    map_dfr(function(vector) {
        mean_v <- mean(vector, na.rm = T)
        median_v <- median(vector, na.rm = T)
        sd_v <- sd(vector, na.rm = T)
        n_obs <- length(vector[!is.na(vector)])
        min_v <- min(vector, na.rm = T)
        max_v <- max(vector, na.rm = T)
        data.frame(cbind(mean_v, median_v, sd_v, n_obs, min_v, max_v))
    }, .id = "variable")

# Merge with proper labels
label_df <- socio_labels %>%
    mutate(order = paste0("1_", order)) %>%
    bind_rows(party_labels %>% mutate(order = paste0("2_", order)))

sum_df <- sum_df %>%
    left_join(label_df, by = c("variable" = "v")) %>%
    dplyr::select(-variable) %>%
    dplyr::rename(variable = lab) %>%
    arrange(order) %>%
    dplyr::select(variable, everything(), -order) %>%
    mutate(across(-variable, ~ round(., 2)))

# Create LaTeX table
(table <- kbl(sum_df,
    linesep = "",
    booktabs = TRUE,
    format = "latex",
    col.names = c("", "Mean", "Median", "SD", "N", "Min", "Max"),
    caption = "Summary statistics for county-level variables in 2013",
    label = "sum",
    position = "h"
) %>%
    kable_paper(full_width = FALSE) %>%
    footnote(
        general = "The table shows summary statistics for aggregate-level data in 2013.",
        general_title = "Notes: ",
        alphabet_title = "Type II: ",
        footnote_as_chunk = TRUE,
        title_format = c("italic")
    ) %>%
    kable_styling(font_size = 8))

# Save table
write_lines(table, "results/tab_1.tex")

### END
