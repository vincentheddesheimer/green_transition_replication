# Output:
# - Figure D.1: Differences between respondents in brown and non-brown occupations over time
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

pacman::p_load("tidyverse", "pbapply", "MatchIt", "conflicted", "data.table")

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Custom theme
source("code/helper_functions/func_theme_custom.R")

# =============================================================================
# 1. Data loading and preparation
# =============================================================================

# Main SOEP data set
df <- fread("data/final/not_to_be_shared/data_soep.csv")

# Weights
w_df <- read_rds("data/final/not_to_be_shared/data_soep_weights.rds")

# Variables to analyze
vlist <- c(
    "age", "monthly_income", "sex", "isced97",
    "worried_migration"
)

# Merge
df <- df %>%
    left_join(w_df, by = c("pid", "syear"))

# Subset to obs w/ weights
df_use <- df %>%
    filter(!is.na(weights))

# Differece between brown and non-brown jobs
diff_df <- df_use %>%
    filter(emp == "employed") %>%
    filter(!is.na(brown_dummy)) %>%
    group_by(syear, brown_dummy) %>%
    summarise(across(all_of(vlist),
        list(
            mean = ~ mean(., na.rm = T),
            sd = ~ sd(., na.rm = T)
        ),
        .names = "{.col}.{.fn}"
    )) %>%
    ungroup() %>%
    pivot_longer(cols = -c("syear", "brown_dummy")) %>%
    separate(name, into = c("variable", "what"), sep = "\\.") %>%
    pivot_wider(
        id_cols = c("syear", "brown_dummy", "variable"), names_from = "what",
        values_from = "value"
    ) %>%
    mutate(across(all_of(c("mean", "sd")), ~ ifelse(. == 0, NA, .))) %>%
    pivot_wider(
        id_cols = c("syear", "variable"), names_from = c("brown_dummy"),
        values_from = "mean"
    ) %>%
    mutate(diff = `1` - `0`) %>%
    select(-c(`0`, `1`))

# Weighted diff df
diff_df_weighted <- df_use %>%
    filter(emp == "employed") %>%
    filter(!is.na(brown_dummy)) %>%
    group_by(syear, brown_dummy) %>%
    summarise(across(all_of(vlist),
        list(
            mean = ~ weighted.mean(., weights, na.rm = T),
            sd = ~ sd(., na.rm = T)
        ),
        .names = "{.col}.{.fn}"
    )) %>%
    ungroup() %>%
    pivot_longer(cols = -c("syear", "brown_dummy")) %>%
    separate(name, into = c("variable", "what"), sep = "\\.") %>%
    pivot_wider(
        id_cols = c("syear", "brown_dummy", "variable"), names_from = "what",
        values_from = "value"
    ) %>%
    mutate(across(all_of(c("mean", "sd")), ~ ifelse(. == 0, NA, .))) %>%
    pivot_wider(
        id_cols = c("syear", "variable"), names_from = c("brown_dummy"),
        values_from = "mean"
    ) %>%
    mutate(diff = `1` - `0`) %>%
    select(-c(`0`, `1`)) %>%
    mutate(weighted = 1)

# Combine
diff_df_full <- diff_df %>%
    mutate(weighted = 0) %>%
    bind_rows(diff_df_weighted %>%
        mutate(weighted = 1))

# Proper names
ndf <- data.frame(v = vlist, lab = c(
    "Age (years)", "Income\n(Euro)",
    "Female (0/1)",
    "ISCED97\n(standardized)",
    "Immigration worries (0-2)"
))

# Merge to get proper names
diff_df_full <- diff_df_full %>%
    left_join(ndf, by = c("variable" = "v"))


# =============================================================================
# 2. Figure D.1: Differences between respondents in brown and non-brown occupations over time
# =============================================================================

# Plot
ggplot(diff_df_full, aes(syear, diff, factor(weighted))) +
    geom_point(shape = 21, aes(
        color = factor(weighted),
        fill = factor(weighted)
    )) +
    geom_smooth(span = 1.6, aes(color = factor(weighted)), se = F) +
    xlab("Year") +
    ylab("Difference between respondents in\nbrown vs. non-brown occupations") +
    facet_wrap(~lab, scales = "free_y", ncol = 2) +
    theme_custom() +
    theme(legend.position = "bottom") +
    scale_color_manual(
        name = "",
        labels = c("Unweighted", "Weighted"),
        values = c("grey60", "black")
    ) +
    scale_fill_manual(
        name = "",
        labels = c("Unweighted", "Weighted"),
        values = c("grey60", "black")
    )

ggsave(
    filename = "results/fig_D1.pdf",
    width = 10, height = 8.5
)

### END
