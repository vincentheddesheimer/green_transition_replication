# Outputs:
# - Figure G.5: Effect of brown employment on predicted status
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load packages
pacman::p_load(
    tidyverse,
    data.table,
    fixest,
    ranger,
    conflicted
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Custom theme
source("code/helper_functions/func_theme_custom.R")

# Load data
df <- fread("data/final/not_to_be_shared/data_soep_predstatus.csv")

# Political vars
pvars <- c(
    "SPD", "FDP", "Gruene",
    "Linke",
    "CDUCSU", "far_right"
)

# Create base models with covariates (without status)
did_cov <- feols(
    .[pvars] ~ brown_dummy * post_2015 +
        age + monthly_income + as.factor(isced97) +
        isei_combined + worried_migration |
        pid + syear,
    df,
    cluster = ~pid
)

# Create base models with status predictions
did_cov_status <- feols(
    .[pvars] ~ brown_dummy * post_2015 +
        age + monthly_income + as.factor(isced97) +
        isei_combined + worried_migration +
        iss1_pred_rf + iss2_pred_rf |
        pid + syear,
    df,
    cluster = ~pid
)


# Create party labels dataframe (similar to county analysis)
ndf_parties <- data.frame(
    v = pvars[!pvars == "partisan"],
    lab = c(
        "SPD", "FDP", "Green Party",
        "Left Party", "CDU/CSU", "Far-right parties\n(incl. AfD)"
    )
) %>%
    mutate(
        order = c(3, 5, 2, 1, 4, 6)
    ) %>%
    arrange(desc(order))

# Set levels in the correct order
ndf_parties$lab <- factor(ndf_parties$lab, levels = ndf_parties$lab)

# Extract coefficients for main effects
get_coef <- function(x) {
    x %>%
        map_dfr(broom::tidy,
            conf.int = T,
            cluster = "pid"
        ) %>%
        filter(str_detect(
            term,
            "brown_dummy"
        )) %>%
        mutate(p = rep(pvars,
            each = 2
        )) %>%
        mutate(post_2015 = ifelse(!str_detect(term, "post_2015"),
            "Brown job", "Brown job * 2016 or later"
        ))
}

# Get coefficients for both models
coef_original <- get_coef(did_cov) %>%
    mutate(what = "without status controls")
coef_status <- get_coef(did_cov_status) %>%
    mutate(what = "with status controls")

# Combine and add labels
coef_combined <- bind_rows(coef_original, coef_status) %>%
    left_join(ndf_parties, by = c("p" = "v"))

# Create coefficient plot with correct factor levels
pd <- position_dodge(0.4)

# Plot
coef_combined %>%
    filter(post_2015 == "Brown job * 2016 or later") %>%
    mutate(lab = factor(lab, levels = levels(ndf_parties$lab))) %>%
    ggplot(aes(y = lab, x = estimate * 100, group = what, color = what, fill = what)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_errorbarh(
        aes(
            xmin = conf.low * 100,
            xmax = conf.high * 100
        ),
        position = pd,
        height = 0
    ) +
    geom_point(
        size = 2,
        position = pd
    ) +
    xlab("Estimated effect of brown employment (p.p.)") +
    ylab("") +
    theme_custom() +
    theme(legend.position = "bottom") +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "",
        labels = c(
            "Original estimates",
            "Controlling for predicted status"
        )
    ) +
    scale_fill_grey(
        start = 0, end = 0.6,
        name = "",
        labels = c(
            "Original estimates",
            "Controlling for predicted status"
        )
    )

ggsave(
    filename = "results/fig_G5.pdf",
    width = 7.5, height = 4.75
)

### END
