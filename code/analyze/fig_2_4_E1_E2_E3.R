# Outputs:
# - Figure 2: Map of brown employment share across German counties (2013)
# - Figure 4: Scatter plots of AfD vote share gains vs brown employment (2013-2021)
# - Figure E.1: Scatter plots of AfD vote share gains vs brown employment (2013-2017)
# - Figure E.2: Change in brown employment shares, 2013-2021
# - Figure E.3: Average party vote shares by brown employment share (median split), 2009-2021
# =============================================================================

# Clear workspace and load packages
rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, data.table, sf, ggrepel, kableExtra)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load county-level election data with covariates
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
# 2. CREATE FIGURE 2: MAP OF BROWN EMPLOYMENT SHARE (2013)
# =============================================================================

# Load shapefiles for German counties and states
krs <- st_read("data/raw/shapefiles/VG250_KRS.shp") %>%
    dplyr::select(AGS_0, GEN) %>%
    dplyr::mutate(county = substr(AGS_0, 1, 5)) %>%
    mutate(state_id = substr(county, 1, 2)) %>%
    dplyr::select(-AGS_0) %>%
    dplyr::rename(county_name = GEN)

land <- st_read("data/raw/shapefiles/VG250_LAN.shp") %>%
    filter(GF == 4) %>%
    dplyr::select(AGS_0) %>%
    dplyr::mutate(state_id = substr(AGS_0, 1, 2)) %>%
    dplyr::select(-AGS_0)

# Prepare brown employment data for 2013
brown_share <- df %>%
    filter(election_year == 2013) %>%
    dplyr::select(county, brown0_3_share_2013) %>%
    mutate(county = str_pad(county, 5, side = "left", pad = "0")) %>%
    distinct(county, .keep_all = TRUE)

# Merge brown employment data with county shapefile
krs <- krs %>%
    left_join(brown_share, by = "county") %>%
    filter(!state_id %in% c("04", "02", "11")) # Exclude city states (HH, HB, Berlin)

# Load major cities for map labels
cities_de <- read_csv("data/raw/shapefiles/cities_de.csv") %>%
    slice(1:5) %>%
    dplyr::select(city, lat, lng)

# Transform to consistent coordinate system
krs <- st_transform(krs, crs = 4326)
land <- st_transform(land, crs = 4326)

# Create map of brown employment share
ggplot() +
    geom_sf(data = krs, aes(fill = brown0_3_share_2013 * 100), color = NA) +
    geom_sf(data = land, fill = NA, color = "black") +
    scale_fill_distiller(
        palette = "BrBG", direction = -1,
        name = "Share of employees in brown occupations",
        trans = "log",
        breaks = c(1, 2, 3, 5, 10, 20)
    ) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
    ) +
    guides(fill = guide_colorbar(title.position = "top")) +
    geom_point(
        data = cities_de, aes(x = lng, y = lat),
        fill = "white", shape = 21, size = 2
    ) +
    geom_label_repel(
        data = cities_de, aes(x = lng, y = lat, label = city),
        nudge_y = -0.05, size = 4
    )

ggsave(
    filename = "results/fig_2.pdf",
    width = 5, height = 7.5
)

# =============================================================================
# 3. CREATE FIGURE 4: AFD GAIN VS BROWN EMPLOYMENT SHARE (2013-2021)
# =============================================================================

# Prepare data for 2013-2021 analysis
df_plot_2021 <- df %>%
    mutate(east = ifelse(as.numeric(state) > 10, "East Germany", "West Germany")) %>%
    filter(election_year %in% c(2013, 2021)) %>%
    group_by(county) %>%
    mutate(observed_both = n() == 2) %>%
    ungroup() %>%
    filter(observed_both) %>%
    group_by(county) %>%
    mutate(
        afd_diff = afd[election_year == 2021] - afd[election_year == 2013]
    ) %>%
    ungroup() %>%
    filter(election_year == 2013) %>%
    dplyr::select(county, afd_diff, brown0_3_share_2013, east) %>%
    left_join(krs %>% dplyr::select(-brown0_3_share_2013), by = "county") %>%
    distinct(county, .keep_all = TRUE)

# Select counties for labeling (outliers)
df_label_2021 <- df_plot_2021 %>%
    mutate(x = log(1 + brown0_3_share_2013)^4 * log(1 + afd_diff)) %>%
    dplyr::filter(!dplyr::between(x, quantile(x, 0.005), quantile(x, 0.98)))

# Create scatter plot for 2013-2021
ggplot(df_plot_2021, aes(brown0_3_share_2013 * 100, afd_diff * 100)) +
    geom_point(aes(shape = east, fill = east, color = east), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, aes(color = east)) +
    ggrepel::geom_text_repel(
        data = df_label_2021,
        aes(label = county_name),
        size = 3,
        box.padding = 0.2,
        point.padding = 0.2,
        segment.color = NA,
        segment.size = 0.2,
        nudge_y = -0.5
    ) +
    xlab("% of employees in\nBrown occupations in 2013\n(county-level)") +
    ylab("AfD vote share gain\nbetween 2013 and 2021 (p.p.)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)
    ) +
    scale_shape_manual(values = c(21, 22), name = "") +
    scale_color_brewer(name = "", type = "qual", palette = 1) +
    scale_fill_brewer(name = "", type = "qual", palette = 1) +
    theme(legend.position = "bottom")

ggsave(
    filename = "results/fig_4.pdf",
    width = 8.25, height = 5.5
)


# =============================================================================
# 4. CREATE FIGURE E.1: AFD GAIN VS BROWN EMPLOYMENT SHARE (2013-2017)
# =============================================================================

# Prepare data for 2013-2017 analysis
df_plot_2017 <- df %>%
    mutate(east = ifelse(as.numeric(state) > 10, "East Germany", "West Germany")) %>%
    filter(!county %in% c("03159", "16063")) %>% # Exclude specific counties
    filter(election_year %in% c(2013, 2017)) %>%
    group_by(county) %>%
    mutate(
        afd_diff = afd[election_year == 2017] - afd[election_year == 2013]
    ) %>%
    ungroup() %>%
    filter(election_year == 2013) %>%
    dplyr::select(county, afd_diff, brown0_3_share_2013, east) %>%
    left_join(krs %>% dplyr::select(-brown0_3_share_2013), by = "county") %>%
    distinct(county, .keep_all = TRUE)

# Select counties for labeling (outliers)
df_label_2017 <- df_plot_2017 %>%
    mutate(x = (100 * brown0_3_share_2013)^3 * log(1 + 0.1 * afd_diff)) %>%
    dplyr::filter(!dplyr::between(x, quantile(x, 0.005), quantile(x, 0.975)))

# Create scatter plot for 2013-2017
ggplot(df_plot_2017, aes(brown0_3_share_2013 * 100, afd_diff * 100)) +
    geom_point(aes(shape = east, fill = east, color = east), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, aes(color = east)) +
    ggrepel::geom_text_repel(
        data = df_label_2017,
        aes(label = county_name),
        size = 3,
        box.padding = 0.2,
        point.padding = 0.2,
        segment.color = NA,
        segment.size = 0.2,
        nudge_y = -0.5
    ) +
    xlab("% of employees in\nBrown occupations in 2013") +
    ylab("AfD vote share gain\nbetween 2013 and 2017 (p.p.)") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)
    ) +
    scale_shape_manual(values = c(21, 22), name = "") +
    scale_color_brewer(name = "", type = "qual", palette = 1) +
    scale_fill_brewer(name = "", type = "qual", palette = 1) +
    theme(legend.position = "bottom")

ggsave(
    filename = "results/fig_E1.pdf",
    width = 8.25, height = 5.5
)


# =============================================================================
# 5. FIGURE E.2: CHANGE IN BROWN EMPLOYMENT SHARES, 2013-2021
# =============================================================================

# Time series of brown employment share trends
brown_summary_time <- df %>%
    filter(election_year > 2012) %>%
    group_by(election_year) %>%
    summarise(
        mean_brown_share = mean(brown0_3_share, na.rm = TRUE) * 100,
        median_brown_share = median(brown0_3_share, na.rm = TRUE) * 100,
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = c(mean_brown_share, median_brown_share),
        names_to = "statistic",
        values_to = "value"
    ) %>%
    mutate(statistic = case_when(
        statistic == "mean_brown_share" ~ "Mean",
        statistic == "median_brown_share" ~ "Median",
        TRUE ~ statistic
    ))

ggplot(brown_summary_time, aes(x = election_year, y = value, color = statistic, linetype = statistic)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = unique(df$election_year)) +
    scale_color_manual(values = c("Mean" = "black", "Median" = "grey50")) +
    scale_linetype_manual(values = c("Mean" = "solid", "Median" = "solid")) +
    labs(
        x = "Election year",
        y = "Share of employees in Brown\noccupations across counties (%)",
        color = "Statistic",
        linetype = "Statistic"
    ) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    )

ggsave(
    filename = "results/fig_E2.pdf",
    width = 7, height = 4
)

# =============================================================================
# 6. FIGURE E.3: AVERAGE PARTY VOTE SHARES BY BROWN EMPLOYMENT SHARE (MEDIAN SPLIT), 2009-2021
# =============================================================================

# Reload data for longer time series analysis
df_long <- fread("data/final/data_main.csv",
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

# Set up data with 2013 brown employment variables
df_long <- df_long %>%
    group_by(county) %>%
    mutate(
        observed_2013 = any(election_year == 2013),
        brown0_3_share_2013 = ifelse(observed_2013, brown0_3_share[election_year == 2013], NA)
    ) %>%
    ungroup()

# Create median split for brown employment
df_long <- df_long %>%
    group_by(election_year) %>%
    mutate(brown_share_2013_cat = ifelse(
        brown0_3_share_2013 < median(brown0_3_share_2013, na.rm = TRUE),
        "Low brown employment share",
        "High brown employment share"
    )) %>%
    ungroup()

# Define party variables and convert to percentages
party_vars <- c("spd", "fdp", "gruene", "linke_pds", "afd", "cdu_csu", "far_right")

# Create labels for party variables
party_labels <- data.frame(
    v = party_vars,
    lab = c("SPD", "FDP", "Green Party", "Left Party", "AfD", "CDU/CSU", "Far-right parties")
) %>%
    mutate(order = c(5, 3, 6, 7, 1, 4, 2)) %>%
    arrange(order) %>%
    mutate(lab = fct_reorder(lab, order))

# Calculate conditional means by brown employment category
df_means <- df_long %>%
    group_by(election_year, brown_share_2013_cat) %>%
    summarize(across(all_of(party_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(party_vars), names_to = "party", values_to = "vote_share") %>%
    left_join(party_labels, by = c("party" = "v"))

# Plot conditional means
ggplot(
    df_means %>% filter(election_year > 2008),
    aes(x = election_year, y = vote_share * 100, color = brown_share_2013_cat, group = brown_share_2013_cat)
) +
    geom_vline(xintercept = 2015, linetype = "dotted", color = "black") +
    geom_line() +
    geom_point() +
    facet_wrap(~lab, scales = "free_y") +
    labs(
        x = "Election year",
        y = "Vote share (%)",
        color = NULL
    ) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_color_grey(start = 0, end = 0.6) +
    scale_x_continuous(breaks = unique(df_long$election_year))

ggsave(
    filename = "results/fig_E3.pdf",
    width = 9, height = 6.5
)

### END
