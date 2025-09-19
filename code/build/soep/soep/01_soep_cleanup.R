## SOEP Data Cleaning and Preparation Script
## This script processes raw SOEP (German Socio-Economic Panel) data by loading, merging,
## and cleaning various datasets including attitudes, employment, and demographic information.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, haven, data.table, readxl)


# Load and prepare root dataset (person-level information) -----------------

ppathl <- fread("data/raw/soep/ppathl.csv")
root <- ppathl[syear >= 1990, .(
  pid, piyear, syear, cid, hid, phrf, eintritt, austritt, sampreg,
  sex, sexor, gebjahr, partner, germborn, corigin, migback
)]
# fwrite(ppathl_small, "ppathl_small.dta")

# Load state/federal state information ------------------------------------

# hbrutto <- read_dta("hbrutto.dta") # my own file does not contain valuable data for bula
hbrutto <- data.table(read_dta("data/raw/soep/hbrutto.dta"))
hbrutto <- hbrutto |> select(hid, syear, bula)

# bula data for 2018-2021
bula <- fread("data/raw/soep/bula_hid.csv")
bula <- bula |> select(hid, syear, bula_h)

# Load attitudes and political variables -----------------------------------

# plh0004: Political tendency left/right
# plh0007: Interest in politics
# plh0011_h: Political orientation (Party Preference)
# plh0012_h: Political orientation (Party Affiliation)
# plh0013_h: Political orientation (Party Identification)
# plh0333: And how was it at the last general election (Bundestagswahl) on September 22, 2013? Which party did you vote for?
# pli0097_h: participaton pol. parties, local politics, political intiatives
# plh0111: important socially and politically involved

# plh0263_h: trade union member

# plh0173: Satisfaction with work
# plh0175: Satisfaction with household income
# plh0176: Satisfaction with personal income
# plh0206i11: Positive Attitude Toward Myself
# plh0032: Worried about economic development
# plh0033: Worried about finances
# plh0036: Worried about environment
# plh0037: Worried about consequences from climate change
# plh0042: Worried about job security
# plj0046: Worried about immigration to Germany

# plb0433_v2: How likely is it that you will lose your job?
# plb0438_v2: How likely is it that you will be demoted at your current place of employment?

# pl/plh0244: Attitude towards future

# plc0013_h: [de] Bruttoverdienst letzten Monat

pl <- fread("data/raw/soep/pl.csv")
attitudes <- pl[, .(
  pid, syear, iyear, pmonin, ptagin,
  plh0004, plh0007, plh0011_h, plh0012_h, plh0013_h, plh0333, pli0097_h, plh0263_h,
  plh0111, plh0173, plh0175, plh0176,
  plh0206i11, plh0032, plh0033, plh0036, plh0037, plh0042, plj0046,
  plb0433_v2, plb0438_v2, plc0013_h
)]
# fwrite(pl_small, "pl_small.dta")

# Load employment and education variables ----------------------------------

# pglfs: labor force status
# pglabgro: current gross labor income in euro
# pglabnet: current net labor income in euro
# pgstib: occupational position
# pgjobch: occpuational change
# pgjobend: Reasons for occupational change
# pgisco88: current occupational classification (ISCO-88 Com, 4 digits)
# pgisco08: current occupational classification (ISCO-08, 4 digits)
# pgisced97: ISCED-1997-Classification: "International Standard Classification of Education (ISCED)"
# pgisced11: ISCED-2011-Classification: "International Standard Classification of Education (ISCED)"
# pgisei08: Last Reached Isei Value
# pgisei88: Last Reached Isei Value
# pgkldb92: Current Occupational Classification (KldB92)
# pgnace: Industry Occupation (NACE Rev. 1.1, Sector)
# pgnace2: Industry Occupation [pbra] (NACE Rev. 2, Sector)
# pgerwzeit: Length Of Time With Firm
# pgpsbil: school-leaving degree

pgen <- fread("data/raw/soep/pgen.csv")
employment <- pgen[, .(
  pid, syear, pglfs, pglabgro, pglabnet, pgstib, pgjobch, pgjobend,
  pgisco88, pgisco08, pgisced97, pgisced11, pgisei08, pgisei88,
  pgkldb92, pgnace, pgnace2, pgerwzeit, pgpsbil
)]
# fwrite(pgen_small, "pgen_small.dta")

# Load mechanism variables (work-related stress, mental health) -----------

# plb0113: Thinking About Work-Related Problems First Thing in The Morning
# plb0114: Easy To Stop Thinking About Work
# plb0115: Make Sacrifices For Career
# plb0116: Always thinking about work
# plb0117: Sleeping Problems Due to Work
# plb0124: job burden: bad chance of promotion
# plb0125: job burden: bad chance of promotion (scaled)
# plb0126: job burden: worsening work
# plb0127: job burden: worsening work (scaled)
# plb0128: job burden: job jeopardy
# plb0129: job burden: job jeopardy (scaled)
# plb0176_h: working hours per week
# ple0027: frequency of being sad in the last 4 weeks
# ple0028: frequency of being calm and relaxed in the last 4 weeks
# ple0029: frequency of being energetic in the last 4 weeks
# ple0033: frequency of achieving less due to emotional problems in the last 4 weeks
# plh0184: frequency of being angry in the last 4 weeks
# plh0185: frequency of being worried in the last 4 weeks
# plh0186: frequency of being happy in the last 4 weeks
# plh0187: frequency of being sad in the last 4 weeks
# plh0334: flourishing: Do you feel that what you are doing in your life is valuable and useful?
# plh0182: How satisfied are you currently with your life in general?

mechanisms <- pl[, .(
  pid, syear, plb0113, plb0114, plb0115, plb0116, plb0117,
  plb0124, plb0125, plb0126, plb0127, plb0128, plb0129,
  plb0176_h,
  ple0027, ple0028, ple0029, ple0033,
  plh0184, plh0185, plh0186, plh0187, plh0334, plh0182
)]

# Merge all datasets into main dataframe ----------------------------------

df <- root |>
  # left_join(interview, by = c("pid", "syear"), suffix=c("",".y")) |>
  left_join(hbrutto, by = c("hid", "syear"), suffix = c("", ".y")) |>
  left_join(bula, by = c("hid", "syear"), suffix = c("", ".y")) |>
  left_join(attitudes, by = c("pid", "syear"), suffix = c("", ".y")) |>
  left_join(employment, by = c("pid", "syear"), suffix = c("", ".y")) |>
  left_join(mechanisms, by = c("pid", "syear"), suffix = c("", ".y")) |>
  select(-ends_with(".y"))

# Transform state variables for consistency -------------------------------

df <- df |>
  mutate(
    bula_h = gsub("[^0-9]", "", bula_h),
    # combine bula & bula_h (keep bula if not NA, else bula_h)
    state = ifelse(is.na(bula), bula_h, bula)
  ) |>
  select(-c(bula, bula_h))

# Rename variables for clarity and consistency ----------------------------

df <- df |>
  rename(
    weighting_factor = phrf,
    birth_year = gebjahr,
    pol_tendency = plh0004,
    pol_interest = plh0007,
    partisan = plh0011_h,
    party_aff = plh0012_h,
    party_aff_intensity = plh0013_h,
    party_last_election = plh0333,
    particip_politics = pli0097_h,
    union_member = plh0263_h,
    important_particip_politics = plh0111,
    satisf_work = plh0173,
    satisf_hh_income = plh0175,
    satisf_ind_income = plh0176,
    satisf_life = plh0182,
    self_esteem = plh0206i11,
    worried_econ_dev = plh0032,
    worried_finances = plh0033,
    worried_environment = plh0036,
    worried_climate_change = plh0037,
    worried_job_security = plh0042,
    worried_migration = plj0046,
    likely_job_loss = plb0433_v2,
    likely_job_demotion = plb0438_v2,
    monthly_income = plc0013_h,
    labor_force_status = pglfs,
    labor_income_gross = pglabgro,
    labor_income_net = pglabnet,
    occupational_position = pgstib,
    occupational_change = pgjobch,
    occupational_change_reason = pgjobend,
    time_w_firm = pgerwzeit,
    edu = pgpsbil,
    think_work_problems_morning = plb0113,
    easy_stop_thinking_work = plb0114,
    sacrifices_for_career = plb0115,
    always_thinking_work = plb0116,
    sleeping_problems_work = plb0117,
    job_burden_chance_promotion = plb0124,
    job_burden_chance_promotion_scale = plb0125,
    job_burden_worsening_work = plb0126,
    job_burden_worsening_work_scale = plb0127,
    job_burden_job_jeopardy = plb0128,
    job_burden_job_jeopardy_scale = plb0129,
    working_hours = plb0176_h,
    freq_angry = plh0184,
    freq_worried = plh0185,
    freq_happy = plh0186,
    freq_sad = plh0187,
    last4_melancholy = ple0027,
    last4_calm = ple0028,
    last4_energetic = ple0029,
    last4_accompl_less_emo = ple0033,
    activities_useful = plh0334
  ) |>
  rename_all(~ str_replace_all(., "pg", ""))

# Recode and clean variables ----------------------------------------------

df <- df |>
  mutate(
    piyear = na_if(piyear, -2),
    state = ifelse(state < 0, NA, state),
    # Map state codes to state names for analysis
    state_name = recode(state,
      `01` = "Schleswig-Holstein",
      `02` = "Hamburg",
      `03` = "Niedersachsen",
      `04` = "Bremen",
      `05` = "North Rhine-Westphalia",
      `06` = "Hesse",
      `07` = "Rhineland-Palatinate",
      `08` = "Baden-Württemberg",
      `09` = "Bavaria",
      `10` = "Saarland",
      `11` = "Berlin",
      `12` = "Brandenburg",
      `13` = "Mecklenburg-Vorpommern",
      `14` = "Saxony",
      `15` = "Saxony-Anhalt",
      `16` = "Thuringia"
    ),
    female = ifelse(sex == 2, 1, 0),
    male = ifelse(sex == 1, 1, 0),
    heterosex = ifelse(sexor == 1, 1, 0),
    birth_year = na_if(birth_year, -1),
    partner = ifelse(partner > 0 & partner < 5, 1, 0),
    pol_tendency = ifelse(pol_tendency < 0, NA, pol_tendency),
    pol_interest = ifelse(pol_interest < 0, NA, abs(pol_interest - 4)),
    partisan = ifelse(partisan == 1, 1, ifelse(partisan == 2, 0, NA)),
    party_aff_intensity = ifelse(party_aff_intensity < 0, NA, abs(party_aff_intensity - 5)),
    # party affiliation
    party_aff = ifelse(party_aff < 0, NA, party_aff),
    SPD = ifelse(party_aff == 1, 1, ifelse(!is.na(party_aff), 0, NA)),
    CDU = ifelse(party_aff == 2, 1, ifelse(!is.na(party_aff), 0, NA)),
    CSU = ifelse(party_aff == 3, 1, ifelse(!is.na(party_aff), 0, NA)),
    FDP = ifelse(party_aff == 4, 1, ifelse(!is.na(party_aff), 0, NA)),
    Gruene = ifelse(party_aff == 5, 1, ifelse(!is.na(party_aff), 0, NA)),
    Linke = ifelse(party_aff == 6, 1, ifelse(!is.na(party_aff), 0, NA)),
    NPD_Republikaner_Rechte = ifelse(party_aff == 7, 1, ifelse(!is.na(party_aff), 0, NA)),
    AfD = ifelse(party_aff == 27, 1, ifelse(!is.na(party_aff), 0, NA)),
    CDUCSU = ifelse(CDU == 1 | CSU == 1 | party_aff == 13, 1, ifelse(!is.na(party_aff), 0, NA)),
    center_right = ifelse(CDUCSU == 1 | FDP == 1, 1, ifelse(!is.na(party_aff), 0, NA)),
    center_left = ifelse(SPD == 1 | Gruene == 1, 1, ifelse(!is.na(party_aff), 0, NA)),
    far_right = ifelse(NPD_Republikaner_Rechte == 1 | AfD == 1, 1, ifelse(!is.na(party_aff), 0, NA)),
    # party last election
    party_last_election = ifelse(party_last_election < 0, NA, party_last_election),
    SPD_last_election = ifelse(party_last_election == 1, 1, ifelse(!is.na(party_last_election), 0, NA)),
    CDU_last_election = ifelse(party_last_election == 2, 1, ifelse(!is.na(party_last_election), 0, NA)),
    CSU_last_election = ifelse(party_last_election == 3, 1, ifelse(!is.na(party_last_election), 0, NA)),
    FDP_last_election = ifelse(party_last_election == 4, 1, ifelse(!is.na(party_last_election), 0, NA)),
    Gruene_last_election = ifelse(party_last_election == 5, 1, ifelse(!is.na(party_last_election), 0, NA)),
    Linke_last_election = ifelse(party_last_election == 6, 1, ifelse(!is.na(party_last_election), 0, NA)),
    NPD_Republikaner_Rechte_last_election = ifelse(party_last_election == 7, 1, ifelse(!is.na(party_last_election), 0, NA)),
    AfD_last_election = ifelse(party_last_election == 27, 1, ifelse(!is.na(party_last_election), 0, NA)),
    CDUCSU_last_election = ifelse(CDU == 1 | CSU == 1 | party_last_election == 13, 1, ifelse(!is.na(party_last_election), 0, NA)),
    center_right_last_election = ifelse(CDUCSU == 1 | FDP == 1, 1, ifelse(!is.na(party_last_election), 0, NA)),
    center_left_last_election = ifelse(SPD == 1 | Gruene == 1, 1, ifelse(!is.na(party_last_election), 0, NA)),
    far_right_last_election = ifelse(NPD_Republikaner_Rechte == 1 | AfD == 1, 1, ifelse(!is.na(party_last_election), 0, NA)),
    # Others
    particip_politics = ifelse(particip_politics < 0, NA, abs(particip_politics - 5)),
    union_member = ifelse(union_member < 0, NA, abs(union_member - 2)),
    important_particip_politics = ifelse(important_particip_politics < 0, NA, abs(important_particip_politics - 4)),
    satisf_work = ifelse(satisf_work < 0, NA, satisf_work),
    satisf_hh_income = ifelse(satisf_hh_income < 0, NA, satisf_hh_income),
    satisf_ind_income = ifelse(satisf_ind_income < 0, NA, satisf_ind_income),
    satisf_life = ifelse(satisf_life < 0, NA, satisf_life),
    self_esteem = ifelse(self_esteem < 0, NA, self_esteem),
    worried_econ_dev = ifelse(worried_econ_dev < 0, NA, abs(worried_econ_dev - 3)),
    worried_finances = ifelse(worried_finances < 0, NA, abs(worried_finances - 3)),
    worried_environment = ifelse(worried_environment < 0, NA, abs(worried_environment - 3)),
    worried_climate_change = ifelse(worried_climate_change < 0, NA, abs(worried_climate_change - 3)),
    worried_job_security = ifelse(worried_job_security < 0, NA, abs(worried_job_security - 3)),
    worried_migration = ifelse(worried_migration < 0, NA, abs(worried_migration - 3)),
    likely_job_loss = ifelse(likely_job_loss < 0, NA, likely_job_loss),
    likely_job_demotion = ifelse(likely_job_demotion < 0, NA, likely_job_demotion),
    monthly_income = ifelse(monthly_income < 0, NA, monthly_income),
    labor_force_status = ifelse(labor_force_status < 0, NA, labor_force_status),
    labor_income_gross = ifelse(labor_income_gross < 0, NA, labor_income_gross),
    labor_income_net = ifelse(labor_income_net < 0, NA, labor_income_net),
    occupational_change = ifelse(occupational_change > 2, 1,
      ifelse(occupational_change == 2 | occupational_change == 1, 0, NA)
    ),
    occupational_change_reason = ifelse(occupational_change_reason < 0, NA, occupational_change_reason),
    occ_chng_term_by_emp = ifelse(occupational_change_reason == 1, 1, 0),
    occ_chng_own_resig = ifelse(occupational_change_reason == 4, 1, 0),
    occ_chng_mutual_resig = ifelse(occupational_change_reason == 5, 1, 0),
    occ_chng_comp_closed = ifelse(occupational_change_reason == 11, 1, 0),
    isco88 = ifelse(isco88 < 0, NA, isco88),
    isco08 = ifelse(isco08 < 0, NA, isco08),
    isced97 = ifelse(isced97 < 0, NA, isced97),
    isced11 = ifelse(isced11 < 0, NA, isced11),
    isei08 = ifelse(isei08 < 0, NA, isei08),
    isei88 = ifelse(isei88 < 0, NA, isei88),
    kldb92 = ifelse(kldb92 < 0, NA, kldb92),
    nace = ifelse(nace < 0, NA, nace),
    nace2 = ifelse(nace2 < 0, NA, nace2),
    time_w_firm = ifelse(time_w_firm < 0, NA, time_w_firm),
    edu = ifelse(edu > 4 | edu < 1, NA, edu),
    think_work_problems_morning = ifelse(think_work_problems_morning < 0, NA, think_work_problems_morning - 1),
    easy_stop_thinking_work = ifelse(easy_stop_thinking_work < 0, NA, easy_stop_thinking_work - 1),
    sacrifices_for_career = ifelse(sacrifices_for_career < 0, NA, sacrifices_for_career - 1),
    always_thinking_work = ifelse(always_thinking_work < 0, NA, always_thinking_work - 1),
    sleeping_problems_work = ifelse(sleeping_problems_work < 0, NA, sleeping_problems_work - 1),
    job_burden_chance_promotion = ifelse(job_burden_chance_promotion < 0, NA, abs(job_burden_chance_promotion - 2)),
    job_burden_chance_promotion_scale = ifelse(job_burden_chance_promotion_scale < 0, NA, job_burden_chance_promotion_scale - 1),
    job_burden_worsening_work = ifelse(job_burden_worsening_work < 0, NA, abs(job_burden_worsening_work - 2)),
    job_burden_worsening_work_scale = ifelse(job_burden_worsening_work_scale < 0, NA, job_burden_worsening_work_scale - 1),
    job_burden_job_jeopardy = ifelse(job_burden_job_jeopardy < 0, NA, abs(job_burden_job_jeopardy - 2)),
    job_burden_job_jeopardy_scale = ifelse(job_burden_job_jeopardy_scale < 0, NA, job_burden_job_jeopardy_scale - 1),
    working_hours = ifelse(working_hours < 0, NA, working_hours),
    freq_angry = ifelse(freq_angry < 0, NA, freq_angry),
    freq_worried = ifelse(freq_worried < 0, NA, freq_worried),
    freq_happy = ifelse(freq_happy < 0, NA, freq_happy),
    freq_sad = ifelse(freq_sad < 0, NA, freq_sad),
    last4_melancholy = ifelse(last4_melancholy < 0, NA, abs(last4_melancholy - 5)),
    last4_calm = ifelse(last4_calm < 0, NA, abs(last4_calm - 5)),
    last4_energetic = ifelse(last4_energetic < 0, NA, abs(last4_energetic - 5)),
    last4_accompl_less_emo = ifelse(last4_accompl_less_emo < 0, NA, abs(last4_accompl_less_emo - 5)),
    activities_useful = ifelse(activities_useful < 0, NA, activities_useful),
    age = piyear - birth_year,
    sex = ifelse(sex < 0, NA, sex - 1),
    state = as.character(state),
    emp = case_when(
      labor_force_status %in% c(1:5, 7:9) ~ "nilf",
      labor_force_status %in% c(10:12) ~ "employed",
      labor_force_status %in% c(6) ~ "unemployed",
      TRUE ~ NA_character_
    ),
    migback = if_else(migback > 1, 1, 0),
    monthly_income = if_else(monthly_income > quantile(monthly_income, probs = c(0.9999), na.rm = T), NA, monthly_income)
  ) |>
  group_by(syear) %>%
  mutate(
    isei08 = (isei08 - mean(isei08, na.rm = T)) / sd(isei08, na.rm = T),
    isei88 = (isei88 - mean(isei88, na.rm = T)) / sd(isei88, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(isei_combined = coalesce(isei08, isei88)) %>%
  # create change variables for party preferences over time
  group_by(pid) |>
  arrange(pid, syear) |>
  mutate(
    delta_CDUCSU = CDUCSU - lag(CDUCSU),
    delta_FDP = FDP - lag(FDP),
    delta_SPD = SPD - lag(SPD),
    delta_Gruene = Gruene - lag(Gruene),
    delta_Linke = Linke - lag(Linke),
    delta_NPD_Republikaner_Rechte = NPD_Republikaner_Rechte - lag(NPD_Republikaner_Rechte),
    delta_center_right = center_right - lag(center_right),
    delta_center_left = center_left - lag(center_left)
  ) |>
  ungroup()

# Create 2015 baseline variables for heterogeneous treatment effects ------

hte_vars <- df %>%
  filter(syear == 2015) %>%
  mutate(
    has_abitur_2015 = case_when(
      is.na(edu) ~ NA,
      edu == 4 ~ 1,
      TRUE ~ 0
    ),
    has_higher_ed_2015 = case_when(
      is.na(isced97) ~ NA,
      isced97 == 6 ~ 1,
      TRUE ~ 0
    ),
    east_2015 = case_when(
      is.na(state) ~ NA,
      as.numeric(state) > 11 ~ 1,
      TRUE ~ 0
    ),
    female_2015 = case_when(
      is.na(female) ~ NA,
      female == 1 ~ 1,
      TRUE ~ 0
    ),
    income_above_med_2015 = case_when(
      is.na(monthly_income) ~ NA,
      monthly_income > median(monthly_income, na.rm = T) ~ 1,
      TRUE ~ 0
    ),
    age_above_50_2015 = case_when(
      is.na(age) ~ NA,
      age > 50 ~ 1,
      TRUE ~ 0
    ),
    worried_migration_high_2015 = case_when(
      is.na(worried_migration) ~ NA,
      worried_migration > 1 ~ 1,
      T ~ 0
    ),
    manufacturing_job_2015 = case_when(
      is.na(nace2) ~ NA,
      nace2 %in% c(10:33) ~ 1,
      T ~ 0
    )
  ) %>%
  dplyr::select(
    pid, has_abitur_2015,
    has_higher_ed_2015, east_2015,
    female_2015, income_above_med_2015,
    age_above_50_2015, manufacturing_job_2015, worried_migration_high_2015
  )

## Merge 2015 baseline variables back to main dataframe

df <- df %>%
  left_join(hte_vars, by = "pid")

## Create interview date variable for temporal analysis
df <- df %>%
  mutate(
    date = ymd(paste(syear, pmonin, ptagin, sep = " "))
  )

# Change working directory to project folder
setwd("~/Princeton Dropbox/Vincent Heddesheimer/hhv_replication_green_transition")

# Load occupation-level greenness/brownness data --------------------------

# Greeness / brownness classification by ISCO-08 occupation codes
vona <- fread("data/raw/occupations/Volna_ISCOCodes.csv") |>
  select(
    isco08 = ISCO08,
    Greenness, Brownness
  )

# Vona crosswalked to ISCO-88 for historical compatibility
vona88 <- fread("data/raw/occupations/brownness_isco88.csv") |>
  rename(Greenness_88 = Greenness, Brownness_88 = Brownness)

# ISCO correspondence table for code conversion
isco <- read_xls("data/raw/occupations/corrtab08-88.xls")

# Merge with greenness/brownness scores for both ISCO-08 and ISCO-88 ------

df_merged <- df |>
  left_join(vona, by = c("isco08" = "isco08")) |>
  left_join(vona88, by = c("isco88" = "isco88")) |>
  # combine greenness/brownness measures from both ISCO versions
  mutate(
    Greenness = ifelse(!is.na(isco08) & is.na(Greenness), 0, Greenness),
    Brownness = ifelse(!is.na(isco08) & is.na(Brownness), 0, Brownness),
    Greenness_88 = ifelse(!is.na(isco88) & is.na(Greenness_88), 0, Greenness_88),
    Brownness_88 = ifelse(!is.na(isco88) & is.na(Brownness_88), 0, Brownness_88),
    greenness = ifelse(!is.na(Greenness), Greenness, Greenness_88),
    brownness = ifelse(!is.na(Brownness), Brownness, Brownness_88),
    green_dummy = case_when(
      greenness > 0.2 ~ 1,
      TRUE ~ 0
    ),
    brown_dummy = case_when(
      brownness > 0.3 ~ 1,
      TRUE ~ 0
    ),
    # for both green & brown dummy: if is.na(labor_force_status), code as NA
    green_dummy = ifelse(is.na(labor_force_status), NA, green_dummy),
    brown_dummy = ifelse(is.na(labor_force_status), NA, brown_dummy)
  )

# Remove intermediate greenness/brownness variables
df_merged <- df_merged |>
  select(-Greenness, -Greenness_88, -Brownness, -Brownness_88)

## Code brown employment status in 2015 (constant within person)
df_merged <- df_merged %>%
  group_by(pid) %>%
  mutate(observed_2015 = 1 * (any(syear == 2015, na.rm = T))) %>%
  mutate(brown_dummy_2015 = ifelse(
    observed_2015,
    brown_dummy[syear == 2015],
    NA
  )) %>%
  ungroup()

# Add post-treatment dummies
df_merged <- df_merged %>%
  mutate(post_2015 = if_else(syear > 2014, 1, 0)) %>%
  mutate(post_2019 = if_else(syear > 2019, 1, 0))


# Remove variables that are not needed
df_merged <- df_merged %>%
  select(-c(
    sexor, party_last_election, particip_politics, union_member, important_particip_politics,
    occupational_change:occupational_change_reason,
    think_work_problems_morning:activities_useful,
    SPD_last_election:far_right_last_election,
    occ_chng_term_by_emp:occ_chng_comp_closed,
    delta_CDUCSU:delta_center_left
  ))

# Write final cleaned dataset to file
fwrite(df_merged, "data/final/not_to_be_shared/data_soep.csv")

### END