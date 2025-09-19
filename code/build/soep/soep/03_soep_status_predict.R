## SOEP Status Prediction Script
## This script predicts innovation status variables (iss1, iss2) for SOEP respondents
## using models trained on SOEP-IS data. It requires the SOEP-IS data to be cleaned first
## by running 01_soep_is_cleanup.R to generate the necessary input files.
##
## Prerequisites:
## - Run 01_soep_is_cleanup.R first to generate data_soep_is.csv
## - This script uses SOEP-IS data to train prediction models
## - Then applies these models to predict status for main SOEP respondents

# Load required packages
pacman::p_load(tidyverse, data.table, fixest, ranger)


# Load SOEP-IS data for training prediction models -------------------------

df <- fread("data/final/not_to_be_shared/data_soep_is.csv") %>%
    as_tibble() %>%
    mutate(
        state = as.character(state),
        monthly_income = monthly_income / 1000 # Scale income to thousands
    )

# Standardize innovation status variables within years ----------------------

# Standardize iss1 and iss2 within years (as done in original analysis)
df <- df %>%
    group_by(syear) %>%
    mutate(
        iss1 = (iss1 - mean(iss1, na.rm = T)) / sd(iss1, na.rm = T),
        iss2 = (iss2 - mean(iss2, na.rm = T)) / sd(iss2, na.rm = T)
    ) %>%
    ungroup()

# Define comprehensive prediction formula with relevant variables ------------

# Create more comprehensive prediction models with additional relevant variables
status_formula <- formula(
    . ~ age + sex + monthly_income + as.factor(isced97) + isei_combined +
        as.factor(migback) + as.factor(labor_force_status) +
        as.factor(occupational_position) + has_abitur_2015 + has_higher_ed_2015 +
        east_2015 + as.factor(germborn)
)

# Prepare clean data for random forest models -----------------------------

# Clean data by removing NAs for model fitting
model_data <- df %>%
    select(
        iss1, iss2, age, sex, monthly_income, isced97, isei_combined,
        migback, labor_force_status, occupational_position,
        has_abitur_2015, has_higher_ed_2015, east_2015, germborn
    ) %>%
    na.omit()

# Train random forest models for status prediction ------------------------

# Update random forest models with more predictors - using explicit formula
rf_status_models <- list(
    iss1 = ranger(
        formula = iss1 ~ age + sex + monthly_income + isced97 + isei_combined +
            migback + labor_force_status + occupational_position +
            has_abitur_2015 + has_higher_ed_2015 + east_2015 + germborn,
        data = model_data,
        num.trees = 500,
        importance = "impurity",
        seed = 123
    ),
    iss2 = ranger(
        formula = iss2 ~ age + sex + monthly_income + isced97 + isei_combined +
            migback + labor_force_status + occupational_position +
            has_abitur_2015 + has_higher_ed_2015 + east_2015 + germborn,
        data = model_data,
        num.trees = 500,
        importance = "impurity",
        seed = 123
    )
)

# Load main SOEP data for prediction -------------------------------------

# Load the SOEP data
df <- fread("data/final/not_to_be_shared/data_soep.csv") %>%
    mutate(state = as.character(state)) %>%
    filter(syear > 2010) %>%
    mutate(emp = ifelse(emp == "", NA, emp))

# Subset to employed individuals for prediction
df <- df %>%
    filter(emp == "employed")

# Scale income to thousands (as done in the original model)
df <- df %>%
    mutate(monthly_income = monthly_income / 1000)

# Apply prediction models to main SOEP data ------------------------------

# Make predictions using both models
df <- df %>%
    mutate(
        # Random Forest predictions for innovation status
        iss1_pred_rf = predict(rf_status_models$iss1, data = .)$predictions,
        iss2_pred_rf = predict(rf_status_models$iss2, data = .)$predictions
    )

# Save predictions to file
fwrite(df, "data/final/not_to_be_shared/data_soep_predstatus.csv")

### END