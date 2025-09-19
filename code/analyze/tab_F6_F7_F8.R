# Outputs:
# - Table F.6: AfD candidate fielding and occupation data availability by election year
# - Table F.7: Summary statistics for district-level candidate variables by election year
# - Table F.8: Effect of Brown Occupation Share on AfD Candidate Fielding and Characteristics
# =============================================================================

# The raw data files are not publicly available.
# Refer to the README file and code/build/candidates/01_build_candidates.R
# for details on how to access the data.


rm(list = ls())

# Load packages
pacman::p_load(tidyverse, ISCO08ConveRsions, conflicted, data.table, fixest, kableExtra)

# Resolve package conflicts
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)

# Load helper functions
source("code/helper_functions/func_tidy_feols.R")
source("code/helper_functions/func_fixest_dict_cty.R")

# Load and prepare candidate data -----------------------------------------

# Load candidate dataset and prepare for analysis
df <- read_rds("data/final/not_to_be_shared/data_candidates.rds") %>%
    mutate(state_id = as.character(state)) %>%
    mutate(state_id = str_pad(state_id, 2, side = "left", pad = "0")) %>%
    filter(election_year %in% c(2009, 2013, 2017, 2021)) %>%
    # Filter out specific states (Berlin, Hamburg, Bremen)
    filter(
        state_id != "11",
        state_id != "02",
        state_id != "04"
    ) %>%
    dplyr::rename(elec_district = wk_id) %>%
    group_by(elec_district) %>%
    # Create education indicator for GLES data
    mutate(gles_at_least_college = ifelse(gles_educ_highest %in% c("MA", "BA", "Fachhochschulreife", "PhD"), 1, 0)) %>%
    mutate(gles_at_least_college = ifelse(is.na(gles_educ_highest), NA, gles_at_least_college)) %>%
    ungroup()

# Create brown employment indicators --------------------------------------

# Code brown employment share in 2013 for analysis
df <- df %>%
    group_by(elec_district) %>%
    # Create 2013 brown employment share variable
    mutate(brown0_3_share_2013 = brown0_3_share[election_year == 2013]) %>%
    ungroup() %>%
    group_by(election_year) %>%
    # Create binary indicator for high vs low brown employment districts
    mutate(brown0_3_share_binary = ifelse(brown0_3_share_2013 > median(brown0_3_share_2013, na.rm = TRUE), 1, 0)) %>%
    ungroup()

glimpse(df)

# Load brown occupation classification data
bscores <- "data/raw/occupations/Volna_ISCOCodes.csv" %>%
    fread() %>%
    select(isco08 = ISCO08, brown_score = Brownness) %>%
    filter(brown_score > 0.0) %>%
    # Create binary indicators for different brown employment thresholds
    mutate(
        cand_brown_above_0_3 = ifelse(brown_score > 0.3, 1, 0),
        cand_brown_above_0 = ifelse(brown_score > 0, 1, 0)
    )

# Merge brown occupation indicators with candidate data
df <- df %>%
    left_join(bscores %>%
        dplyr::select(isco08, cand_brown_above_0_3, cand_brown_above_0), by = c("isco08_code_1" = "isco08")) %>%
    # Handle missing occupation codes appropriately
    mutate(
        cand_brown_above_0_3 =
            case_when(
                is.na(isco08_code_1) & afd_fields_candidate == 1 ~ NA,
                is.na(isco08_code_1) & afd_fields_candidate == 0 ~ 0,
                !is.na(isco08_code_1) & afd_fields_candidate == 1 ~ 0,
                !is.na(isco08_code_1) & !afd_fields_candidate == 0 ~ cand_brown_above_0_3
            ),
        cand_brown_above_0 =
            case_when(
                is.na(isco08_code_1) & afd_fields_candidate == 1 ~ NA,
                is.na(isco08_code_1) & afd_fields_candidate == 0 ~ 0,
                !is.na(isco08_code_1) & afd_fields_candidate == 1 ~ 0,
                !is.na(isco08_code_1) & !afd_fields_candidate == 0 ~ cand_brown_above_0
            )
    )

# Calculate ISEI (International Socio-Economic Index) scores for candidates
isei_vec <- sapply(
    df$isco08_code_1,
    function(x) {
        if (is.na(x)) {
            return(NA)
        } else {
            return(ISCO08ConveRsions::isco08toisei08(x))
        }
    }
)

# Add ISEI scores back to dataset
df <- df %>%
    mutate(candidate_isei = isei_vec)

# Define outcome variables for analysis
olist_base <- c(
    "cand_brown_above_0_3",
    "cand_brown_above_0",
    "afd_fields_candidate",
    "candidate_isei"
)
olist_all <- c(olist_base)
proper_names <- data.frame(v = olist_all, lab = c(
    "Candidate in brown occupation\n(binary, 0.3 threshold)",
    "Candidate in brown occupation\n(binary, 0 threshold)",
    "AfD fields candidate\n(binary)",
    "ISEI (occ. prestige) score\n(0-100)"
))


# =============================================================================
# Table F.6: AfD candidate fielding and occupation data availability by election year
# =============================================================================

# Calculate share of candidates meeting criteria by year
share_afd_isei_by_year <- df %>%
    group_by(election_year) %>%
    summarise(
        n_total = n(),
        n_afd_fields = sum(afd_fields_candidate == 1, na.rm = TRUE),
        n_afd_fields_and_isei_not_na = sum(afd_fields_candidate == 1 & !is.na(candidate_isei), na.rm = TRUE),
        .groups = "drop" # Drop grouping after summarising
    ) %>%
    filter(!election_year == 2021)

(table <- kbl(share_afd_isei_by_year,
    format = "latex",
    booktabs = T,
    linesep = "",
    digts = 2, col.names = c(
        "Election year", "Total candidates", "AfD candidates", "AfD candidates with ISEI"
    )
) %>%
    kable_styling(latex_options = c(
        "hold_position"
    ), full_width = F))

# Save table
write_lines(table, "results/tab_F6.tex")

# =============================================================================
# Table F.7: Summary statistics for district-level candidate variables by election year
# =============================================================================

# Generate summary statistics for district-level outcomes by year (2013, 2017)
# Summary statistics for district-level outcomes by year (2013, 2017)
summary_stats_district <- df %>%
    filter(election_year %in% c(2013, 2017)) %>%
    select(election_year, all_of(olist_all)) %>%
    group_by(election_year) %>%
    summarise(across(
        all_of(olist_all),
        list(
            mean = ~ mean(.x, na.rm = TRUE),
            share_missing = ~ mean(is.na(.x))
        ),
        .names = "{.col}_{.fn}" # Rename columns for clarity
    )) %>%
    pivot_longer(
        cols = -election_year,
        names_to = c("variable", ".value"),
        names_pattern = "(.*)_(mean|share_missing)"
    ) %>%
    pivot_wider(
        names_from = election_year,
        values_from = c(mean, share_missing),
        names_glue = "{election_year}_{.value}"
    ) %>%
    mutate(across(-variable, ~ round(.x, 3)))

rownames <- c("Candidate occupation brownness > 0.3 (binary)", "Candidate occupation brownness > 0 (binary)", "AfD fields candidate (binary)", "Candidate ISEI (0-100)")

summary_stats_district[, 1] <- rownames


(table <- kbl(summary_stats_district,
    format = "latex",
    booktabs = T,
    linesep = "",
    digts = 2, col.names = c(
        "Variable description", "Mean (2013)", "Mean (2017)", "Share missing (2013)", "Share missing (2017)"
    )
) %>%
    kable_styling(latex_options = c(
        "hold_position"
    ), full_width = F))

# Save table
write_lines(table, "results/tab_F7.tex")


# =============================================================================
# Table F.8: Effect of Brown Occupation Share on AfD Candidate Fielding and Characteristics
# =============================================================================


# Load county-level data for comparison analysis -------------------------

df_cty <- fread("data/final/data_main.csv") %>%
    mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
    mutate(state_id = substr(county, 1, 2)) %>%
    filter(election_year %in% c(1998, 2002, 2005, 2009, 2013, 2017, 2021)) %>%
    # Filter out specific states (Berlin, Hamburg, Bremen)
    dplyr::filter(
        state_id != "11",
        state_id != "02",
        state_id != "04"
    ) %>%
    group_by(county) %>%
    # Create 2013 baseline variables for analysis
    mutate(brown0_3_share_2013 = brown0_3_share[election_year == 2013]) %>%
    mutate(share_manufacturing_2013 = share_manufacturing_regstat[election_year == 2013]) %>%
    mutate(pop_density_2013 = pop_density[election_year == 2013]) %>%
    ungroup()

# Statistical inference and regression analysis --------------------------

# Multiply share brown by 100 for percentage interpretation
df_cty <- df_cty %>%
    mutate(brown0_3_share_2013 = brown0_3_share_2013 * 100)
df <- df %>%
    mutate(brown0_3_share_2013 = brown0_3_share_2013 * 100)

# Helper function to clean regression results
clean_results <- function(mod, term_find = "share_2013") {
    mod %>%
        map_dfr(tidy_feols) %>%
        dplyr::filter(str_detect(term, term_find)) %>%
        mutate(term = str_extract(term, "\\d+"))
}

olist_models <- c(
    "afd_fields_candidate",
    "candidate_isei"
)

# Model: electoral district level results
# Fixed effects regression with district and state-year fixed effects
mod <- feols(
    .[olist_models] ~ i(election_year, brown0_3_share_2013, ref = 2013) | elec_district + election_year^state_id,
    cluster = ~elec_district,
    data = df %>%
        filter(!election_year == 2021)
)

# Note that the treatment is 0-1, so coefficient is change from 0 to 1
mod_county <- feols(
    afd_local_ran_prop ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl +
        gdp_pc_ardeco +
        share_manufacturing_regstat +
        pop_density +
        population +
        schulabganger_mit_allgemeiner_hochschulreife_inkar +
        arbeitslosenquote_inkar | county + election_year^regbez,
    cluster = ~county,
    data = df_cty %>%
        filter(!election_year == 2021)
)

# Generate LaTeX table with regression results
# Display formatted table in console with common options
(etable(list(mod, mod_county),
    tex = TRUE,
    signif.code = c("**" = 0.01, "*" = 0.05, "." = 0.1), # Significance stars
    fitstat = ~ n + r2, # Show N and R-squared
    keep = "%brown0_3_share_2013", # Show only interaction terms of interest
    digits = 3, # Standard number of digits for estimates/SEs
    digits.stats = 3, # Digits for fit statistics (like N)
    file = "results/tab_F8.tex"
)
)

### END