# Outputs:
# - Figure 3: County-level brown employment in 2013 and electoral results (main plot)
# - Figure 5: East / West Germany comparison
# - Table F.1: Main effects table for all parties
# - Figure F.1: Event study of brown employment on all party vote shares (2013-2021)
# - Table F.4: Different fixed effects specifications for far-right vote share
# - Figure F.2: Sun Abraham event study for all parties
# - Tables F.2 & F.3: East / West Germany main effects tables
# - Figure F.3: Alternative treatment definitions (continuous, binary thresholds)
# - Figure F.4: State exclusion robustness (AfD vote share)
# - Figure F.5: Honest DiD confidence sets for all parties except AfD
# - Table F.5: Economic outcomes (Income, GDP, unemployment) with 2WFE
# - Figure F.6: Suedekum treatment (CO2 emissions-based treatment)
# - Figure F.8: Population-weighted vs unweighted estimates
# - Figure F.9: Manufacturing employment controls (levels vs changes)
# - Figure F.10: Anti-refugee violence and prior far-right vote controls
# - Figure F.11: Peripherality (dialectal distance controls)
# - Figure F.12: Status accounting (ISEI interaction effects)
# - Figures F.13 & F.14: Effect size comparisons (standardized variables)
# =============================================================================

# Clear workspace and load packages
rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, fixest, HonestDiD)

source("code/helper_functions/func_tidy_feols.R")
source("code/helper_functions/func_dict_covars.R")

# Functions ------------------------------------------------------------

# Function to clean results and get CIs
clean_results <- function(mod, term_find = "share_2013") {
    mod %>%
        map_dfr(tidy_feols) %>%
        dplyr::filter(str_detect(term, term_find)) %>%
        mutate(term = str_extract(term, "\\d+"))
}

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

df <- fread("data/final/data_main.csv",
    colClasses = c("county" = "character")
) %>%
    mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
    mutate(state_id = substr(county, 1, 2)) %>%
    filter(election_year %in% c(1998, 2002, 2005, 2009, 2013, 2017, 2021)) %>%
    dplyr::filter(
        state_id != "11",
        state_id != "02",
        state_id != "04"
    )

# Define treatments
list_treatments_2013 <- c(
    "brownness_share_2013", "brown1_share_2013",
    "brown0_3_share_2013", "brown0_5_share_2013"
)

# Brown variables
brownvars <- c(
    "brownness_share", "brown1_share",
    "brown0_3_share", "brown0_5_share"
)

# Create more variables
df <- df %>%
    group_by(county) %>%
    mutate(
        observed_2009 = any(election_year == 2009),
        observed_2013 = any(election_year == 2013),
        observed_2017 = any(election_year == 2017),
        observed_13_17 = observed_2013 & observed_2017,
        observed_09_13 = observed_2013 & observed_2009,
        brownness_share_2013 =
            ifelse(observed_2013, brownness_share[election_year == 2013], NA),
        brown1_share_2013 = ifelse(observed_2013, brown1_share[election_year == 2013], NA),
        brown0_3_share_2013 = ifelse(observed_2013, brown0_3_share[election_year == 2013], NA),
        brown0_5_share_2013 = ifelse(observed_2013, brown0_5_share[election_year == 2013], NA),
        pop_density_2013 = ifelse(observed_2013, pop_density[election_year == 2013], NA),
        share_manufacturing_2013 = ifelse(observed_2013,
            share_manufacturing_regstat[election_year == 2013], NA
        ),
        manuf_change_13_17 = ifelse(observed_13_17,
            share_manufacturing_regstat[election_year == 2017] - share_manufacturing_regstat[election_year == 2013], NA
        ),
    ) %>%
    ungroup() %>%
    mutate(population_weights = population)


# Outcome variables
pvars <- c(
    "spd", "fdp", "gruene",
    "linke_pds", "afd",
    "cdu_csu", "far_right"
)

# Proper labels for the outcome variables
ndf_parties <- data.frame(v = pvars, lab = c(
    "SPD", "FDP", "Green Party",
    "Left Party", "AfD",
    "CDU/CSU", "Far-right parties"
)) %>%
    mutate(party_order = c(5, 3, 6, 7, 1, 4, 2)) %>%
    arrange(party_order) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Scale covariates ------------------------------------------------------------

covar_list <-
    c(
        "hh_income_vgrdl", "gdp_pc_ardeco", "share_manufacturing_regstat",
        "pop_density", "population",
        "schulabganger_mit_allgemeiner_hochschulreife_inkar",
        "arbeitslosenquote_inkar", "auslanderanteil_inkar",
        "pop_density_2013", "share_manufacturing_2013"
    )

df <- df %>%
    mutate(across(all_of(covar_list), scale, .names = "scaled_{.col}")) %>%
    mutate(across(all_of(paste0("scaled_", covar_list)), as.numeric))

# =============================================================================
# 2. Figure 3: County-level brown employment in 2013 and electoral results
# =============================================================================

# Main specification
mod_twfe <- feols(
    .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
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
    data = df
)

out_twfe <- mod_twfe %>%
    clean_results(term_find = "brown0_3_share") %>%
    mutate(term = as.numeric(term))

# Create plot data
plot_df <- out_twfe %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

pd <- position_dodge(0.4)

# Plot function
do_plot_13_17 <- function(plot_df) {
    plot_df |>
        mutate(term = as.factor(term)) %>%
        ggplot(aes(lab, estimate, group = term, color = term, fill = term)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_errorbar(
            aes(
                ymin = conf.low,
                ymax = conf.high
            ),
            position = pd,
            width = 0
        ) +
        geom_point(
            aes(fill = term),
            size = 2,
            position = pd
        ) +
        xlab("Outcome") +
        ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
        theme_bw() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 15),
            legend.position = "bottom"
        ) +
        coord_flip() +
        scale_color_grey(
            start = 0, end = 0.6,
            name = "Year effect estimated in"
        ) +
        scale_fill_grey(
            start = 0, end = 0.6,
            name = "Year effect estimated in"
        ) +
        scale_shape_discrete(
            name = "Year effect estimated in"
        )
}

# Plot
do_plot_13_17(plot_df)

ggsave(
    filename = "results/fig_3.pdf",
    width = 7.5, height = 4.75
)

# =============================================================================
# 3. Figure 5: East / West
# =============================================================================

# Fit models for East and West Germany
mod_twfe_adjusted_state <- list(
    west = df %>% filter(state_id < 11),
    east = df %>% filter(state_id > 10)
) %>%
    map(~ feols(
        .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
            i(election_year, scaled_share_manufacturing_2013, ref = 2013) +
            i(election_year, scaled_pop_density_2013, ref = 2013) +
            scaled_hh_income_vgrdl + scaled_gdp_pc_ardeco + scaled_share_manufacturing_regstat +
            scaled_pop_density + scaled_population + scaled_schulabganger_mit_allgemeiner_hochschulreife_inkar +
            scaled_auslanderanteil_inkar + scaled_arbeitslosenquote_inkar |
            county + election_year^regbez,
        cluster = ~county,
        data = .
    ))

# Clean results and prepare plot data
out_twfe_eastwest <- mod_twfe_adjusted_state %>%
    map(clean_results) %>%
    bind_rows(.id = "spec") %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(
        lab = fct_reorder(as.factor(lab), party_order),
        spec = ifelse(spec == "west", "West Germany", "East Germany")
    )

# Plot
do_plot_13_17(out_twfe_eastwest) +
    facet_wrap(~spec, ncol = 2)

ggsave(
    filename = "results/fig_5.pdf",
    width = 7.5, height = 4
)


# =============================================================================
# 3. Table F.1: Main effects table
# =============================================================================

# Main specification
mod_twfe_for_table <- feols(
    .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, scaled_share_manufacturing_2013, ref = 2013) +
        i(election_year, scaled_pop_density_2013, ref = 2013) +
        scaled_hh_income_vgrdl +
        scaled_gdp_pc_ardeco +
        scaled_share_manufacturing_regstat +
        scaled_pop_density +
        scaled_population +
        scaled_schulabganger_mit_allgemeiner_hochschulreife_inkar +
        scaled_arbeitslosenquote_inkar | county + election_year^regbez,
    cluster = ~county,
    data = df
)

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

(etable(mod_twfe_for_table,
    style.tex = sdf,
    drop = c(
        "%election_year::2002:brown0_3_share_2013",
        "%election_year::2005:brown0_3_share_2013",
        "%election_year::2009:brown0_3_share_2013"
    ),
    tex = T,
    digits = 3,
    title = "Main effects",
    fitstat = ~ r2 + n,
    digits.stats = 3,
    file = "results/tab_F1.tex"
))

# =============================================================================
# 4. Figure F.1: Event study of brown employment on AfD vote share (2013-2021)
# =============================================================================

plot_df <- out_twfe %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

plot_df |>
    filter(term > 2002) %>%
    ggplot(aes(term, estimate)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 2013, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        width = 0
    ) +
    geom_point(
        shape = 21,
        size = 2,
        fill = "white",
    ) +
    ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
    xlab("Election") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    facet_wrap(~lab, ncol = 3) +
    # scale 1998, 2002, 2005, 2009, 2013, 2017, 2021
    scale_x_continuous(
        breaks = c(1998, 2002, 2005, 2009, 2013, 2017, 2021),
        labels = c("1998", "2002", "2005", "2009", "2013", "2017", "2021")
    )

ggsave(
    filename = "results/fig_F1.pdf",
    width = 8.5, height = 5
)

# =============================================================================
# 5. Table F.4: Different fixed effects specifications
# =============================================================================

# No cov, no FE
m1 <- feols(
    far_right ~ i(election_year, brown0_3_share_2013, ref = 2013) | county + election_year,
    cluster = ~county,
    data = df
)

# Covariates
m2 <- feols(
    far_right ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl + gdp_pc_ardeco + share_manufacturing_regstat +
        pop_density + population + schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar + arbeitslosenquote_inkar |
        county + election_year,
    cluster = ~county,
    data = df
)

# Covariates, state FE
m3 <- feols(
    far_right ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl + gdp_pc_ardeco + share_manufacturing_regstat +
        pop_density + population + schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar + arbeitslosenquote_inkar |
        county + election_year^state,
    cluster = ~county,
    data = df
)

# Covariates, Regierungsbezirk FE
m4 <- feols(
    far_right ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl + gdp_pc_ardeco + share_manufacturing_regstat +
        pop_density + population + schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar + arbeitslosenquote_inkar |
        county + election_year^regbez,
    cluster = ~county,
    data = df
)

# Table
mlist <- list(
    m1, m2, m3, m4
)

(etable(mlist,
    style.tex = sdf,
    keep = c(
        "%election_year::2017:brown0_3_share_2013",
        "%election_year::2021:brown0_3_share_2013"
    ),
    tex = T,
    digits = 3,
    title = "Main effects",
    fitstat = ~ r2 + n,
    digits.stats = 3,
    file = "results/tab_F4.tex"
))


# =============================================================================
# 6. Figure F.2: Sun Abraham event study
# =============================================================================

# Get median brown share in 2013
m_b <- df %>%
    filter(election_year == 2013) %>%
    pull(brown0_3_share_2013) %>%
    median(na.rm = T)

# Create event study dataset
df_sa <- df %>%
    mutate(
        brown_above_median = brown0_3_share_2013 > m_b,
        period = as.numeric(as.factor(election_year)),
        period_treated = if_else(brown_above_median, 6, 10000)
    ) %>%
    filter(!is.na(period_treated))

# Run event study model with controls
mod_sa_controls <- feols(
    .[pvars] ~ sunab(period_treated, period) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl + gdp_pc_ardeco + share_manufacturing_regstat +
        pop_density + population + schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar + arbeitslosenquote_inkar |
        county + election_year^regbez,
    cluster = ~county,
    data = df_sa
)

# Clean and format results
mod_tidy_sa_full <- mod_sa_controls %>%
    lapply(tidy_feols) %>%
    reduce(rbind) %>%
    filter(str_detect(term, "period")) %>%
    mutate(
        period = as.numeric(str_extract(term, "[+-]?\\d+")),
        covars = "Covariates included"
    ) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(
        lab = fct_reorder(as.factor(lab), party_order)
    ) %>%
    left_join(data.frame(
        period = c(-3, -2, -1, 0, 1),
        year = c(2005, 2009, 2013, 2017, 2021)
    )) %>%
    bind_rows(
        expand.grid(
            year = 2013,
            covars = "Covariates included",
            lab = unique(.$lab),
            estimate = 0,
            conf.low = 0,
            conf.high = 0
        )
    ) %>%
    mutate(across(c(estimate, conf.low, conf.high), ~ .x * 100))

# Plot results
ggplot(mod_tidy_sa_full, aes(year, estimate)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 2013, linetype = "dotted") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0
    ) +
    geom_point(shape = 21, fill = "white") +
    facet_wrap(~lab) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    xlab("Election") +
    ylab("Estimated effect of above-median\nbrown employment\n(effect measured in percentage points)") +
    scale_color_manual(values = c("grey50", "black"), name = "") +
    scale_fill_manual(values = c("grey50", "black"), name = "") +
    scale_x_continuous(breaks = c(2005, 2009, 2013, 2017, 2021))

ggsave(
    filename = "results/fig_F2.pdf",
    width = 8.5, height = 5
)

# =============================================================================
# 7. Tables F.2 & F.3: East / West
# =============================================================================

(etable(mod_twfe_adjusted_state$west,
    style.tex = sdf,
    tex = T,
    drop = c(
        "%election_year::2002:brown0_3_share_2013",
        "%election_year::2005:brown0_3_share_2013",
        "%election_year::2009:brown0_3_share_2013"
    ),
    digits = 3,
    title = "Main effects",
    digits.stats = 3,
    file = "results/tab_F2.tex"
))

(etable(mod_twfe_adjusted_state$east,
    style.tex = sdf,
    tex = T,
    drop = c(
        "%election_year::2002:brown0_3_share_2013",
        "%election_year::2005:brown0_3_share_2013",
        "%election_year::2009:brown0_3_share_2013"
    ),
    digits = 3,
    title = "Main effects",
    digits.stats = 3,
    file = "results/tab_F3.tex"
))

# =============================================================================
# 8. Figure F.3: Alternative treatments
# =============================================================================

# Function to estimate models
est_models <- function(treatment) {
    ## Declare treatment

    df[, "treatment"] <- df[, treatment]

    ## Basic
    mod_twfe_main <- feols(
        .[pvars] ~ i(election_year, treatment, ref = 2013) +
            i(election_year, share_manufacturing_2013, ref = 2013) +
            i(election_year, pop_density_2013, ref = 2013) +
            hh_income_vgrdl +
            gdp_pc_ardeco +
            share_manufacturing_regstat +
            pop_density +
            population +
            schulabganger_mit_allgemeiner_hochschulreife_inkar +
            auslanderanteil_inkar +
            arbeitslosenquote_inkar |
            county + election_year^regbez,
        cluster = ~county,
        data = df,
    )

    ## Clean and combine
    out_twfe <- list(
        mod_twfe_main = mod_twfe_main
    ) %>%
        map(clean_results, term_find = "treatment") %>%
        bind_rows(.id = "spec") %>%
        mutate(term = as.numeric(term)) %>%
        mutate(treatment = treatment)
}

# Run for all treatments
out_twfe_alltreatments <- list_treatments_2013 %>%
    map(est_models, .progress = T) %>%
    bind_rows()

# Create treatment dictionary
treatment_dict <- c(
    "brownness_share_2013" = "Treatment definition: continuous",
    "brown1_share_2013" = "Treatment definition: binary (equal to 1)",
    "brown0_3_share_2013" = "Treatment definition: binary (> 0.3)",
    "brown0_5_share_2013" = "Treatment definition: binary (> 0.5)"
) %>%
    data.frame(treatment = names(.), treatment_lab = .)

# Create plot data
plot_df <- out_twfe_alltreatments %>%
    filter(spec == "mod_twfe_main") %>%
    left_join(treatment_dict, by = "treatment") %>%
    filter(term > 2013) %>%
    left_join(ndf_parties, by = c("dv" = "v"))

# Plot
do_plot_13_17(plot_df) +
    facet_wrap(~treatment_lab, ncol = 2)

ggsave(
    filename = "results/fig_F3.pdf",
    width = 8.5, height = 7.5
)


# =============================================================================
# 9. Figure F.4: Exclude states one by one
# =============================================================================

# Function to exclude states one by one
state_list <- df$state %>% unique()

# Function to exclude states one by one
exclude_states <- function(s) {
    df_use <- df %>%
        filter(state != !!s)

    mod <- feols(
        .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
            i(election_year, share_manufacturing_2013, ref = 2013) +
            i(election_year, pop_density_2013, ref = 2013) +
            hh_income_vgrdl +
            gdp_pc_ardeco +
            share_manufacturing_regstat +
            pop_density +
            population +
            schulabganger_mit_allgemeiner_hochschulreife_inkar +
            auslanderanteil_inkar +
            arbeitslosenquote_inkar |
            county + election_year^regbez,
        cluster = ~county,
        data = df_use
    )

    ## Clean and combine

    mod <- mod %>%
        clean_results() %>%
        filter(term %in% c(2017, 2021)) %>%
        left_join(ndf_parties, by = c("dv" = "v")) %>%
        mutate(lab = as.factor(lab)) %>%
        mutate(lab = fct_reorder(lab, party_order))
}

# Do this for all states
out_twfe_states <- state_list %>%
    map(exclude_states, .progress = T) %>%
    bind_rows(.id = "state")

state_id_to_names <- function(state_id, how = "full_name") {
    if (!how %in% c("full_name", "short_name")) {
        stop("how must be either 'full_name' or 'short_name'")
    }
    if (how == "full_name") {
        names <- recode(state_id,
            `01` = "Schleswig-Holstein",
            `02` = "Hamburg", `03` = "Niedersachsen", `04` = "Bremen",
            `05` = "North Rhine-Westphalia", `06` = "Hesse",
            `07` = "Rhineland-Palatinate", `08` = "Baden-Württemberg",
            `09` = "Bavaria", `10` = "Saarland", `11` = "Berlin",
            `12` = "Brandenburg", `13` = "Mecklenburg-Vorpommern",
            `14` = "Saxony", `15` = "Saxony-Anhalt", `16` = "Thuringia"
        )
    }
    if (how == "short_name") {
        names <- recode(state_id,
            `01` = "SH", `02` = "HH", `03` = "NI",
            `04` = "HB", `05` = "NW", `06` = "HE", `07` = "RP",
            `08` = "BW", `09` = "BY", `10` = "SL", `11` = "BE",
            `12` = "BB", `13` = "MV", `14` = "SN", `15` = "ST",
            `16` = "TH"
        )
    }
    return(names)
}

# Get AfD
afd_plot <- out_twfe_states %>%
    filter(dv == "afd") %>%
    mutate(state = str_pad(state, 2, side = "left", pad = "0")) %>%
    mutate(state_name = state_id_to_names(state)) %>%
    mutate(state_name = factor(state_name)) %>%
    mutate(state_name = fct_reorder(state_name, estimate))

# Plot
pd <- position_dodge(0.4)

afd_plot |>
    mutate(term = as.factor(term)) %>%
    mutate(state_name = fct_reorder(state_name, estimate)) %>%
    ggplot(aes(state_name, estimate, group = term, color = term, fill = term)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        position = pd,
        width = 0
    ) +
    geom_point(
        aes(fill = term),
        size = 2,
        position = pd
    ) +
    xlab("Excluded state") +
    ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    coord_flip() +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in:"
    ) +
    scale_fill_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in:"
    ) +
    scale_shape_discrete(
        name = "Year effect estimated in:"
    )

ggsave(
    filename = "results/fig_F4.pdf",
    width = 8.5, height = 6.75
)

# =============================================================================
# 10. Figure F.5: Honest DiD
# =============================================================================

# Function to do this for a specific outcome
do_hdid <- function(outcome, lvec = c(1, 0)) {
    # Main spec (feols expression)

    fmla <- paste0(outcome, " ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez") %>%
        as.formula()

    # Estimate model

    mod_twfe <- feols(fmla,
        cluster = ~county,
        data = df,
    )

    betahat <- summary(mod_twfe)$coefficients # save the coefficients
    sigma <- summary(mod_twfe)$cov.scaled # save the covariance matrix

    # Subset the coefficients to exclude the control (2013)
    betahat <- betahat[2:5]
    sigma <- sigma[2:5, 2:5]

    hdid <- HonestDiD::createSensitivityResults_relativeMagnitudes(
        betahat = betahat,
        sigma = sigma,
        numPrePeriods = 2,
        numPostPeriods = 2,
        l_vec = lvec,
        Mbarvec = seq(1, 2, by = 0.5),
        parallel = T
    ) %>%
        mutate(outcome = outcome) %>%
        mutate(what = "magnitude")

    hdid_ols <- HonestDiD::constructOriginalCS(
        betahat = betahat,
        sigma = sigma,
        l_vec = lvec,
        numPrePeriods = 2,
        numPostPeriods = 2
    ) %>%
        dplyr::rename(lb = 1, ub = 2) %>%
        mutate(outcome = outcome) %>%
        mutate(what = "base")

    bind_rows(hdid, hdid_ols)
}

# Do for all variables
hdid_out <- pvars %>%
    .[!str_detect(., "afd")] %>%
    map_dfr(do_hdid, .progress = TRUE)

hdid_out_21 <- pvars %>%
    .[!str_detect(., "afd")] %>%
    map_dfr(do_hdid, lvec = c(0.0, 1), .progress = TRUE)

hdid_out_both <- pvars %>%
    .[!str_detect(., "afd")] %>%
    map_dfr(do_hdid, lvec = c(0.5, 0.5), .progress = TRUE)

## Merge with party names
hdid_full <- bind_rows(
    hdid_out %>% mutate(lvec = "2017"),
    hdid_out_21 %>% mutate(lvec = "2021"),
    hdid_out_both %>% mutate(lvec = "2017 and 2021"),
) %>%
    left_join(ndf_parties, by = c("outcome" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(
        lab,
        party_order
    )) %>%
    mutate(lvec = factor(lvec, levels = c("2017", "2021", "2017 and 2021")))

# More cleanup
hdid_full <- hdid_full %>%
    mutate(model = case_when(
        is.na(Mbar) ~ "OLS",
        T ~ paste0("Mbar = ", Mbar)
    )) %>%
    filter(!is.na(model)) %>%
    mutate(model = fct_relevel(
        model, "OLS", "Mbar = 1",
        "Mbar = 1.5", "Mbar = 2"
    ))


# Plot
pd <- position_dodge(0.5)

hdid_full %>%
    ggplot(aes(
        x = model, group = lvec, color = lvec, fill = lvec
    )) +
    geom_errorbar(aes(ymin = lb, ymax = ub),
        position = pd, width = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    facet_wrap(~lab) +
    labs(
        x = NULL,
        y = "Confidence set\n(percentage point change rel. to 2013)"
    ) +
    scale_color_brewer(palette = "Dark2", type = "qual", name = element_blank())

ggsave(
    filename = "results/fig_F5.pdf",
    width = 9.5, height = 6.5
)


# =============================================================================
# 11. Table F.5: Econ outcomes (Income, GDP, unemployed, manuf. revenue)
# =============================================================================

# Variables
vlist <- c(
    "hh_income_vgrdl",
    "gdp_pc_ardeco",
    "arbeitslosenquote_inkar"
)

# Check variable means
df %>%
    filter(election_year == 2013) %>%
    summarise(across(all_of(vlist), mean, na.rm = TRUE))

# Base spec:
fmla <- ".[vlist] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) + population | county +
    election_year^regbez" %>%
    as.formula()

mod_econ_base <- feols(fmla,
    cluster = ~county,
    data = df,
)

# Clean
out <- mod_econ_base %>%
    clean_results() %>%
    mutate(term = as.numeric(term))

# Same but 2wfe
fmla_2_unadjusted <- ".[vlist] ~ i(election_year, I(brown0_3_share_2013*100), ref = 2013)  | county +
    election_year^regbez" %>%
    as.formula()

fmla2 <- ".[vlist] ~ i(election_year, I(brown0_3_share_2013*100), ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) + population | county +
    election_year^regbez" %>%
    as.formula()

df$election_year %>% table()

mod_econ_2wfe_unadjusted <- feols(fmla_2_unadjusted,
    cluster = ~county,
    data = df %>% dplyr::filter(election_year %in% c(2013, 2017, 2021))
)

mod_econ_2wfe <- feols(fmla2,
    cluster = ~county,
    data = df %>% dplyr::filter(election_year %in% c(2013, 2017, 2021))
)

## Calculate average of all these outcomes in 2013

avg_2013 <- df %>%
    filter(election_year == 2013) %>%
    summarise(across(all_of(vlist), \(x) mean(x, na.rm = TRUE)))

vec_add <- c(rep(avg_2013, 2)) %>%
    unname()

## Table

(etable(list(mod_econ_2wfe_unadjusted, mod_econ_2wfe),
    style.tex = sdf,
    keep = "2013|2017",
    drop = "density|manuf|population",
    tex = T,
    digits = 3,
    title = "Main effects",
    fitstat = ~ r2 + n + war2,
    extralines = list("DV mean in 2013:" = vec_add),
    digits.stats = 3,
    file = "results/tab_F5.tex"
))

df %>%
    filter(election_year == 2013) %>%
    pull(brown0_3_share_2013) %>%
    summary()

# =============================================================================
# 12. Figure F.6: Suedekum treatment
# =============================================================================

# co2_differenz as the main treatment instead of brown0_3_share_2013
df <- df %>%
    mutate(
        anteil_beschaftige_in_sektoren_mit_co2_fte_anstieg = anteil_beschaftige_in_sektoren_mit_co2_fte_anstieg / 100
    )

mod_suedekum <- feols(
    .[pvars] ~ i(election_year, anteil_beschaftige_in_sektoren_mit_co2_fte_anstieg, ref = 2013) +
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
    data = df
)

# Plot
plot_df <- mod_suedekum %>%
    clean_results(term_find = "anteil_beschaftige_in_sektoren_mit_co2_fte_anstieg") %>%
    mutate(term = as.numeric(term)) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

pd <- position_dodge(0.4)

do_plot_13_17(plot_df) +
    ylab("Estimated effect of a 1 p.p. increase in employment in industries\nwith increasing CO2 emissions between 2000 and 2019\n(effect measured in percentage points)")

ggsave(
    filename = "results/fig_F6.pdf",
    width = 7.5, height = 4.5
)


# =============================================================================
# 13. Figure F.8: Population weights
# =============================================================================

# Population weights
mod_twfe_popweights <- feols(
    .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl +
        gdp_pc_ardeco +
        share_manufacturing_regstat +
        pop_density +
        population +
        schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar +
        arbeitslosenquote_inkar |
        county + election_year^regbez,
    cluster = ~county,
    weights = ~population_weights,
    data = df
)

# Standard
mod_twfe <- feols(
    .[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
        i(election_year, share_manufacturing_2013, ref = 2013) +
        i(election_year, pop_density_2013, ref = 2013) +
        hh_income_vgrdl +
        gdp_pc_ardeco +
        share_manufacturing_regstat +
        pop_density +
        population +
        schulabganger_mit_allgemeiner_hochschulreife_inkar +
        auslanderanteil_inkar +
        arbeitslosenquote_inkar |
        county + election_year^regbez,
    cluster = ~county,
    data = df
)

# Plot
plot_df <- mod_twfe_popweights %>%
    clean_results() %>%
    mutate(w = "Weighted by county population") %>%
    bind_rows(mod_twfe %>% clean_results() %>%
        mutate(w = "Unweighted")) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Plot
do_plot_13_17(plot_df) +
    facet_wrap(~w, ncol = 2)

ggsave(
    filename = "results/fig_F8.pdf",
    width = 7.5, height = 4.5
)

# =============================================================================
# 14. Figure F.9: Allowing for time-varying effects of manufacturing employment
# =============================================================================

fmla0 <- ".[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla1 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla2 <- ".[pvars]~ i(election_year,  brown0_3_share_2013, ref = 2013) +
    i(election_year, manuf_change_13_17, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

# Estimate model
manuf0 <- feols(fmla0,
    cluster = ~county,
    data = df,
)

manuf1 <- feols(fmla1,
    cluster = ~county,
    data = df,
)

manuf2 <- feols(fmla2,
    cluster = ~county,
    data = df,
)

plot_df <- list(
    no_controls = manuf0,
    levels = manuf1,
    changes = manuf2
) %>%
    map(clean_results, term_find = "brown0_3_share") %>%
    bind_rows(.id = "spec") %>%
    mutate(term = as.numeric(term)) %>%
    mutate(spec = case_when(
        spec == "no_controls" ~ "Base specification",
        spec == "levels" ~ "Manufacturing trends:\nlevels in 2013",
        spec == "changes" ~ "Manufacturing trends:\nchanges 2013-2017"
    )) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Plot
pd <- position_dodge(0.4)

do_plot_13_17(plot_df) +
    facet_wrap(~spec, ncol = 3)

ggsave(
    filename = "results/fig_F9.pdf",
    width = 9.5, height = 4.75
)

# =============================================================================
# 15. Figure F.10: Allowing for time-varying effects of pre-2016 levels of anti-refugee violence
# =============================================================================

# Code far -right vote share in 2013
df <- df %>%
    group_by(county) %>%
    mutate(
        observed_2013 = any(election_year == 2013),
        far_right_2013 = ifelse(observed_2013,
            far_right[election_year == 2013],
            NA
        )
    ) %>%
    ungroup()

# Estimate models
vlist <- c(
    "anti_refugee_violence_crimeratio",
    "anti_refugee_violence_popratio"
)

fmla0 <- ".[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla1 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, anti_refugee_violence_crimeratio, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla2 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, anti_refugee_violence_popratio, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla3 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, far_right_2013, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

# Estimate models
immig0 <- feols(fmla0,
    cluster = ~county,
    data = df,
)

immig1 <- feols(fmla1,
    cluster = ~county,
    data = df,
)

immig2 <- feols(fmla2,
    cluster = ~county,
    data = df,
)

immig3 <- feols(fmla3,
    cluster = ~county,
    data = df,
)

# Clean and combine
plot_df <- list(
    no_controls = immig0,
    crimeratio = immig1,
    popratio = immig2,
    far_right = immig3
) %>%
    map(clean_results, term_find = "brown0_3_share") %>%
    bind_rows(.id = "spec") %>%
    mutate(term = as.numeric(term)) %>%
    mutate(spec = case_when(
        spec == "no_controls" ~ "Base specification",
        spec == "crimeratio" ~ "Anti-refugee violence:\ncrime ratio",
        spec == "popratio" ~ "Anti-refugee violence:\npopulation ratio",
        spec == "far_right" ~ "Prior far-right\nvote share"
    )) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Spec to factor, order as above
plot_df <- plot_df %>%
    mutate(spec = as.factor(spec)) %>%
    mutate(spec = factor(spec, levels = c(
        "Base specification",
        "Anti-refugee violence:\ncrime ratio",
        "Anti-refugee violence:\npopulation ratio",
        "Prior far-right\nvote share"
    )))

# Plot
pd <- position_dodge(0.4)

plot_df |>
    mutate(term = as.factor(term)) %>%
    ggplot(aes(lab, estimate, group = term, color = term, fill = term)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        position = pd,
        width = 0
    ) +
    geom_point(
        aes(fill = term),
        size = 2,
        position = pd
    ) +
    xlab("Outcome") +
    ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    coord_flip() +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_fill_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_shape_discrete(
        name = "Year effect estimated in"
    ) +
    facet_wrap(~spec)

ggsave(
    filename = "results/fig_F10.pdf",
    width = 9.5, height = 6.75
)

# =============================================================================
# 16. Figure F.11: Peripherality
# =============================================================================

# Variables
vlist <- c(
    "zhb_dialectal_distance"
)

fmla0 <- ".[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla1 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, zhb_dialectal_distance, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

# Estimate models
zhb0 <- feols(fmla0,
    cluster = ~county,
    data = df,
)

zhb1 <- feols(fmla1,
    cluster = ~county,
    data = df,
)


# Clean and combine
plot_df <- list(
    base = zhb0,
    with_dialect = zhb1
) %>%
    map(clean_results, term_find = "brown0_3_share") %>%
    bind_rows(.id = "spec") %>%
    mutate(term = as.numeric(term)) %>%
    mutate(spec = case_when(
        spec == "base" ~ "Base specification",
        spec == "with_dialect" ~ "Dialectal distance"
    )) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Spec to factor, order as above
plot_df <- plot_df %>%
    mutate(spec = as.factor(spec)) %>%
    mutate(spec = factor(spec, levels = c(
        "Base specification",
        "Dialectal distance"
    )))

# Plot
pd <- position_dodge(0.4)

plot_df |>
    mutate(term = as.factor(term)) %>%
    ggplot(aes(lab, estimate, group = term, color = term, fill = term)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        position = pd,
        width = 0
    ) +
    geom_point(
        aes(fill = term),
        size = 2,
        position = pd
    ) +
    xlab("Outcome") +
    ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    coord_flip() +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_fill_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_shape_discrete(
        name = "Year effect estimated in"
    ) +
    facet_wrap(~spec)

ggsave(
    filename = "results/fig_F11.pdf",
    width = 7.5, height = 4.75
)

# =============================================================================
# 17. Figures F.12: Accounting for status
# =============================================================================

# Calculate 20th percentile of standardized population in 2013
pop_threshold <- df %>%
    filter(election_year == 2013) %>%
    summarise(pop_20th = quantile(population, 0.2, na.rm = TRUE)) %>%
    pull(pop_20th)

# Estimates
fmla0 <- ".[pvars] ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

fmla1 <- ".[pvars]~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, isei_combined_2015, ref = 2013) +
     hh_income_vgrdl +
    gdp_pc_ardeco +
    share_manufacturing_regstat +
    pop_density +
    population +
    schulabganger_mit_allgemeiner_hochschulreife_inkar +
    auslanderanteil_inkar +
    arbeitslosenquote_inkar |
    county + election_year^regbez" %>%
    as.formula()

# Estimate models
isei0 <- feols(fmla0,
    cluster = ~county,
    data = df,
)

isei1 <- feols(fmla1,
    cluster = ~county,
    data = df,
)

# Model for counties above 20th percentile of population
isei_large <- feols(fmla1,
    cluster = ~county,
    data = df %>% filter(population > pop_threshold),
)

# Clean and combine
plot_df <- list(
    base = isei0,
    with_isei = isei1,
    with_isei_large = isei_large
) %>%
    map(clean_results, term_find = "brown0_3_share") %>%
    bind_rows(.id = "spec") %>%
    mutate(term = as.numeric(term)) %>%
    mutate(spec = case_when(
        spec == "base" ~ "Base specification",
        spec == "with_isei" ~ "ISEI interaction",
        spec == "with_isei_large" ~ "ISEI interaction\n(excluding bottom 20% pop)"
    )) %>%
    filter(term %in% c(2017, 2021)) %>%
    left_join(ndf_parties, by = c("dv" = "v")) %>%
    mutate(lab = as.factor(lab)) %>%
    mutate(lab = fct_reorder(lab, party_order))

# Spec to factor, order as above
plot_df <- plot_df %>%
    mutate(spec = as.factor(spec)) %>%
    mutate(spec = factor(spec, levels = c(
        "Base specification",
        "ISEI interaction",
        "ISEI interaction\n(excluding bottom 20% pop)"
    )))

# Plot
pd <- position_dodge(0.4)

plot_df |>
    mutate(term = as.factor(term)) %>%
    ggplot(aes(lab, estimate, group = term, color = term, fill = term)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low,
            ymax = conf.high
        ),
        position = pd,
        width = 0
    ) +
    geom_point(
        aes(fill = term),
        size = 2,
        position = pd
    ) +
    xlab("Outcome") +
    ylab("Estimated effect of a one-percentage\npoint increase in brown employment\n(effect measured in percentage points)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    coord_flip() +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_fill_grey(
        start = 0, end = 0.6,
        name = "Year effect estimated in"
    ) +
    scale_shape_discrete(
        name = "Year effect estimated in"
    ) +
    facet_wrap(~spec, ncol = 3)

ggsave(
    filename = "results/fig_F12.pdf",
    width = 9.5, height = 4.75
)


# =============================================================================
# 18. Figures F.13 + F.14: Effect size
# =============================================================================

base_vars <- c(
    "brown0_3_share_2013",
    "share_manufacturing_2013",
    "pop_density_2013",
    "hh_income_vgrdl_2013",
    "gdp_pc_ardeco_2013",
    "schulabganger_mit_allgemeiner_hochschulreife_inkar_2013",
    "arbeitslosenquote_inkar_2013"
)

proper_names <- c(
    "Brown employment share",
    "Share of manufacturing employment",
    "Population density",
    "HH income",
    "GDP/pc",
    "Share graduates w/ Abitur",
    "Unemployment rate"
) %>%
    str_wrap(width = 25)

# Dictionary
df_dict <- data.frame(
    v = base_vars,
    name = proper_names
)

# First, create 2013 levels for the some of the variables
df_std <- df %>%
    group_by(county) %>%
    mutate(hh_income_vgrdl_2013 = hh_income_vgrdl[election_year == 2013]) %>%
    mutate(gdp_pc_ardeco_2013 = gdp_pc_ardeco[election_year == 2013]) %>%
    mutate(
        schulabganger_mit_allgemeiner_hochschulreife_inkar_2013 =
            schulabganger_mit_allgemeiner_hochschulreife_inkar[election_year == 2013]
    ) %>%
    mutate(arbeitslosenquote_inkar_2013 = arbeitslosenquote_inkar[election_year == 2013]) %>%
    ungroup()

df_std <- df_std %>%
    group_by(election_year) %>%
    mutate(across(all_of(base_vars), ~ as.numeric(scale(.)))) %>%
    ungroup()

v <- "brown0_3_share_2013"

do_specs <- function(v) {
    fmla <- paste0(
        "afd ~ i(election_year, ", v, ", ref = 2013) ",
        " | county + election_year^regbez"
    ) %>%
        as.formula()



    mod <- feols(fmla,
        cluster = ~county,
        data = df_std
    ) %>%
        tidy_feols() %>%
        filter(str_detect(term, v)) %>%
        mutate(period = str_extract(term, "\\d{4}")) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(coef = estimate) %>%
        mutate(v = v)

    return(mod)
}

# All in at the same time
fmla_all <- paste0(
    "afd ~ i(election_year, brown0_3_share_2013, ref = 2013) +
    i(election_year, share_manufacturing_2013, ref = 2013) +
    i(election_year, pop_density_2013, ref = 2013) +
    i(election_year, hh_income_vgrdl_2013, ref = 2013) +
    i(election_year, gdp_pc_ardeco_2013, ref = 2013) +
    i(election_year, schulabganger_mit_allgemeiner_hochschulreife_inkar_2013, ref = 2013) +
    i(election_year, arbeitslosenquote_inkar_2013, ref = 2013) |
    county + election_year^regbez"
) %>%
    as.formula()


mod_all <- feols(fmla_all,
    cluster = ~county,
    data = df_std
) %>%
    tidy_feols() %>%
    mutate(period = str_extract(term, "\\d{4}")) %>%
    mutate(period = as.numeric(period)) %>%
    mutate(coef = estimate) %>%
    mutate(v = str_extract(term, paste0(base_vars, collapse = "|"))) %>%
    left_join(df_dict, by = "v") %>%
    mutate(name = fct_reorder(name, coef))


# Across all variables
coefs <- map_dfr(base_vars, do_specs) %>%
    left_join(df_dict, by = "v") %>%
    mutate(name = fct_reorder(name, coef))

# Create a plot of coefficients with error bars
ggplot(coefs, aes(x = name, y = coef * 100, color = factor(period))) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(
        aes(ymin = conf.low * 100, ymax = conf.high * 100),
        position = position_dodge(width = 0.5),
        width = 0.0
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
        x = "Variable (all standardized)",
        y = "Estimate (percentage points, change rel. to 2013)",
        color = "Election year (rel. to 2013)"
    ) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Election year (rel. to 2013)"
    ) +
    coord_flip()

ggsave(
    filename = "results/fig_F13.pdf",
    width = 7, height = 4
)

# Same plot for the full model
mod_all %>%
    ggplot(aes(x = name, y = coef * 100, color = factor(period))) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(
        aes(ymin = conf.low * 100, ymax = conf.high * 100),
        position = position_dodge(width = 0.5),
        width = 0.0
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
        x = "Variable (all standardized)",
        y = "Estimate (percentage points, change rel. to 2013)",
        color = "Election year (rel. to 2013)"
    ) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_color_grey(
        start = 0, end = 0.6,
        name = "Election year (rel. to 2013)"
    ) +
    coord_flip()

ggsave(
    filename = "results/fig_F14.pdf",
    width = 7, height = 4
)


# END
