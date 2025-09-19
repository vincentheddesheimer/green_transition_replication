## SOEP Survey Weight Generation Script
## This script generates survey weights for SOEP data using propensity score matching
## to create balanced samples across years for analysis of the green transition effects.

rm(list = ls())

# Load required packages for matching and data manipulation
pacman::p_load(
    "tidyverse", "pbapply", "furrr",
    "future", "CBPS", "data.table",
    "survey" # ← NEW
)

# Load cleaned SOEP data and filter to employed individuals -----------------

df <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
    as_tibble() %>%
    filter(emp == "employed")

# Define matching parameters and covariates ---------------------------------

## Matching parameters
sample_year <- 2015
target_year <- 2011
which <- "treated"

## Covariates for propensity score matching
cvar_list <- c(
    "age", "monthly_income",
    "sex", "isced97", "worried_migration",
    "isei_combined"
)

## Censor extreme weights
censor_weights <- FALSE

## Function to perform propensity score matching and generate weights

subset_and_weight <- function(
    sample_year,
    target_year, which = "treated",
    cvar_list = c(
        "age", "monthly_income",
        "sex", "isced97", "worried_migration"
    )) {
    ## Get the target data set (reference year for matching)

    df_target <- df %>%
        filter(syear == target_year) %>%
        filter(!is.na(brown_dummy)) %>%
        dplyr::select(all_of(c(
            "brown_dummy",
            cvar_list, "syear",
            "pid", "post_2019", "emp"
        )))

    ## Get the sample data set (year to be weighted)

    # Subset the data
    df_sample <- df %>%
        filter(syear == sample_year) %>%
        filter(!is.na(brown_dummy)) %>%
        dplyr::select(all_of(c(
            "brown_dummy",
            cvar_list, "syear",
            "post_2015", "pid", "post_2019", "emp"
        )))

    ## Combine target and sample datasets for matching

    df_match <- bind_rows(
        df_target %>%
            mutate(target = 1),
        df_sample %>%
            mutate(target = 0)
    )

    ## Subset to treated or control based on argument

    if (which == "treated") {
        df_match <- df_match %>%
            filter(brown_dummy == 1)
    } else if (which == "control") {
        df_match <- df_match %>%
            filter(brown_dummy == 0)
    } else {
        stop("which must be either treated or control")
    }

    # Create formula for propensity score matching
    fmla <- paste0(
        "target ~ ",
        paste0(cvar_list, collapse = " + ")
    )

    ## Replace isced97 with factor for proper treatment

    fmla <- str_replace(
        fmla, "isced97",
        "factor(isced97)"
    )

    ## Convert to formula object

    fmla <- as.formula(fmla)

    ## Print diagnostic information before matching

    cat(paste0("\nFor ", sample_year), "\n")

    # Remove observations with missing covariates
    df_match <- df_match[complete.cases(df_match[, cvar_list]), ]

    # Print sample sizes for matching

    cat("Number of obs in target: ", sum(df_match$target == 1), "\n")
    cat("Number of obs in sample: ", sum(df_match$target == 0), "\n\n")

    # Perform Covariate Balancing Propensity Score (CBPS) matching
    cbps <- suppressMessages(CBPS::CBPS(
        fmla,
        data = df_match,
        ATT = FALSE, method = "over"
    ))


    ## Extract propensity scores from CBPS model
    df_match$pscores_cbps <- predict(cbps, type = "response")

    ## Calculate stabilised inverse-probability weights
    p_ref <- mean(df_match$target == 1) # Pr(T = 1) in this matched set
    df_match$weights <- ifelse(
        df_match$target == 0,
        p_ref / (1 - df_match$pscores_cbps), # stabilised weight
        1 # reference‐year observations
    )

    ## Censor extreme weights to prevent undue influence

    if (censor_weights) {
        # Build a one-stage (no clustering) survey design
        svy_tmp <- survey::svydesign(
            ids = ~1,
            data = df_match,
            weights = ~weights
        )

        # Trim and redistribute excess mass; strict=TRUE repeats until all ≤ ubound
        svy_trim <- survey::trimWeights(svy_tmp, type = "logit", bounds = c(.5, 2))


        # Overwrite the original weights
        df_match$weights <- weights(svy_trim)
    }

    # Extract only the sample data (target = 0) with weights
    match_data <- df_match %>%
        filter(target == 0)

    ## Normalize weights to sum to 1

    match_data <- match_data %>%
        mutate(
            weights = weights / sum(weights)
        )

    # Return matched data with weights

    return(match_data)
}

# Generate weights for all years relative to 2011 baseline -----------------

target_year <- 2011
sample_year_list <- c(1999:2010, 2012:2021)

# Set up parallel processing
plan(multisession)

## Generate weights for treated group (brown employment)
matched_data_treat <- future_map(
    sample_year_list,
    subset_and_weight,
    target_year = 2011,
    which = "treated",
    cvar_list = cvar_list,
    .progress = TRUE
)

## Generate weights for control group (non-brown employment)
## This can take a bit longer due to larger sample sizes
matched_data_control <- future_map(
    sample_year_list,
    subset_and_weight,
    target_year = 2011,
    which = "control",
    cvar_list = cvar_list,
    .progress = TRUE
)

# Reset to sequential processing
plan(sequential)

## Combine treated and control matched datasets
df_m <- bind_rows(
    matched_data_treat,
    matched_data_control
)

## Add 2011 baseline year with weights = 1 (no reweighting needed)
df_m_final <- bind_rows(
    df_m,
    df %>%
        filter(syear == 2011) %>%
        filter(!is.na(brown_dummy)) %>%
        dplyr::select(all_of(c(
            "brown_dummy",
            cvar_list, "syear",
            "post_2015", "pid", "post_2019", "emp"
        ))) %>%
        mutate(weights = 1)
)

# Normalize weights to sum to 1

df_m_final <- df_m_final %>%
    group_by(brown_dummy, syear) %>%
    mutate(weights = weights / sum(weights)) %>%
    ungroup()

## Save final dataset with weights in multiple formats

# Save as RDS file
write_rds(
    df_m_final %>%
        dplyr::select(pid, syear, weights),
    "data/final/not_to_be_shared/data_soep_weights.rds"
)

# Save as Stata format for compatibility
haven::write_dta(
    df_m_final %>%
        dplyr::select(pid, syear, weights),
    "data/final/not_to_be_shared/data_soep_weights.dta"
)

### END
