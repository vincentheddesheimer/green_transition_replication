# Outputs:
# - Figure 7: SOEP remote: County brownness share and individual-level status in social environment and Germany
# - Table G.9: Individual-level results
# - Table G.10: Relationship between aggregate-level brown employment and status perceptions
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load packages
pacman::p_load(
    tidyverse,
    data.table,
    kableExtra,
    conflicted,
    fixest
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Load functions
source("code/helper_functions/func_fixest_dict_soep.R")
source("code/helper_functions/func_tidy_feols.R")
source("code/helper_functions/func_theme_custom.R")


# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load SOEP IS data
df <- fread("data/final/not_to_be_shared/data_soep_is.csv") %>%
    as_tibble() %>%
    mutate(state = as.character(state)) %>% # have to do that manually because loading csv always assumes class == numeric
    filter(!is.na(iss1) | !is.na(iss2)) %>%
    filter(emp == "employed")

# Variables
vlist <- c(
    "age", "monthly_income",
    "sex", "isced97",
    "worried_migration",
    "migback", "isei_combined"
)

#  variables
vars_iss <- c(
    "iss1", "iss2"
)

# Standardize iss1 and iss2 w/in years
df <- df %>%
    group_by(syear) %>%
    mutate(
        iss1 = (iss1 - mean(iss1, na.rm = T)) / sd(iss1, na.rm = T),
        iss2 = (iss2 - mean(iss2, na.rm = T)) / sd(iss2, na.rm = T)
    ) %>%
    ungroup()

# Divide income by 1000
df <- df %>%
    mutate(monthly_income = monthly_income / 1000)

# Estimate models
m1_both <- feols(
    .[vars_iss] ~ brown_dummy | 0,
    data = df
)

m2_both <- feols(
    .[vars_iss] ~ brown_dummy + age +
        as.factor(isced97) +
        sex |
        state + syear,
    se = "iid",
    data = df
)

m3_both <- feols(
    .[vars_iss] ~ brown_dummy + age +
        as.factor(isced97) +
        isei_combined + monthly_income +
        sex |
        state + syear,
    se = "iid",
    data = df
)

# Make list of models
models <- list(
    m1_both, m2_both, m3_both
)

# Tidy
plot_df <- models %>%
    lapply(function(x) map_dfr(.x = x, .f = tidy_feols)) %>%
    reduce(bind_rows) %>%
    filter(term == "brown_dummy") %>%
    mutate(spec = rep(rep(c("Bivariate", "Base controls & state FE", "Additional controls & state FE"), times = 2), 1)) %>%
    mutate(what = "Treatment: respondent\nhas brown job (0/1)")

# Load SOEP remote data --------------------------------------------------------------
df_v <- "data/final/not_to_be_shared/data_soep_remote_results.csv" %>%
    fread() %>%
    filter(analysis == "status") %>%
    dplyr::rename(estimate = coef, conf.low = ymin, conf.high = ymax) %>%
    dplyr::select(estimate, conf.low, conf.high, spec, label) %>%
    mutate(across(c(estimate, conf.low, conf.high), ~ . / 100)) %>%
    mutate(what = "Treatment: county-level\nbrown share (0-100%)")

lab_df <- data.frame(
    dv = c("iss1", "iss2"),
    label = c(
        "Status in social environment",
        "Status in Germany"
    )
)

# Add labels
plot_df <- plot_df %>%
    left_join(lab_df, by = "dv")

# Combine the two dataframes
plot_df_final <- bind_rows(plot_df, df_v)

# =============================================================================
# 2. Figure 7: Status in social environment and Germany
# =============================================================================

# Plot (x: spec, y: estimate, color: year)
pd <- position_dodge(0.35)

plot_df_final <- plot_df_final %>%
    mutate(label = case_when(
        label == "Status in social environment" ~ "Status in\nsocial environment",
        label == "Status in Germany" ~ "Status in\nGermany",
        TRUE ~ label
    ))

plot_df_final %>%
    dplyr::select(spec, what, label, estimate, conf.low, conf.high) %>%
    filter(spec == "Bivariate")

plot_df_final %>%
    ggplot(aes(x = label, y = estimate, color = as.factor(spec))) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_point(position = pd, aes(shape = spec, color = spec, fill = spec), size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
        width = 0.0,
        position = pd
    ) +
    facet_wrap(~what, scales = "free_x") +
    theme_custom() +
    labs(
        x = NULL,
        y = "Coefficient estimate (standard deviations)"
    ) +
    coord_flip() +
    theme(
        legend.position = "bottom",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(color = "black"),
        axis.ticks = element_line(color = "black")) +
    scale_color_manual(
        values = c("black", "grey40", "grey60"),
        name = ""
    ) +
    scale_fill_manual(
        values = c("black", "grey40", "grey60"),
        name = ""
    ) +
    scale_shape_manual(
        values = c(21, 22, 23),
        name = ""
    ) +
    guides(
        color = guide_legend(reverse = TRUE),
        shape = guide_legend(reverse = TRUE),
        fill = guide_legend(reverse = TRUE)
    )

ggsave(
    filename = "results/fig_7.pdf",
    width = 8.5, height = 5
)

# ----------------------------------------------------------------------------------------------
# 3. Table G.9: Individual-level results
# ----------------------------------------------------------------------------------------------

# Combine models
mlist <- list(
    m1_both, m2_both, m3_both
)

# Style
sdf <- style.tex(
    depvar.title = "",
    fixef.title = "",
    fixef.suffix = " FE",
    var.title = "",
    stats.title = "",
    model.title = "",
    yesNo = c("Yes", "No"),
    signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05)
)

# Table results
(etable(mlist,
    tex = T,
    drop = "Constant",
    style.tex = sdf,
    digits = 3,
    title = "Regression Results: Individual-Level Analysis",
    fitstat = ~ r2 + n,
    digits.stats = 3,
    file = "results/tab_G9.tex"
))


# ----------------------------------------------------------------------------------------------
# 4. Table G.10: Relationship between aggregate-level brown employment and status perceptions
# ----------------------------------------------------------------------------------------------

# Load data
data_table <- "data/final/not_to_be_shared/data_soep_remote_pooled_status.csv" %>%
    fread()

# Make table
(table <- data_table %>%
    kable(
        linesep = "",
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        col.names = c("DV", "IV", "Specification", "Coefficient", "Std. Error", "p-value", "Observations", "R-squared"),
        digits = c(0, 0, 0, 3, 3, 3, 0, 3),
        caption = "Regression Results"
    ) %>%
    kable_styling(
        latex_options = c("scale_down"),
        font_size = 9
    ) %>%
    add_header_above(c(" " = 3, "Statistics" = 5)) %>%
    footnote(
        general = "Note: This table presents regression results for different specifications.",
        threeparttable = TRUE,
        footnote_as_chunk = TRUE
    ) %>%
    kable_classic(full_width = FALSE))

# Save table
write_lines(table, "results/tab_G10.tex")

### END
