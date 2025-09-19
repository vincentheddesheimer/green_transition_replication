## German Longitudinal Election Study (GLES) Candidate Data Processing Script
## This script processes candidate data from GLES surveys (2009, 2013, 2017) to create
## a harmonized dataset of political candidates with demographic, background, and
## political experience variables for analysis of candidate characteristics.

# The raw data files are not publicly available.
# For replicating this script, access the data from GESIS.
# 2017: https://search.gesis.org/research_data/ZA6814
# 2013: https://search.gesis.org/research_data/ZA5716
# 2009: https://search.gesis.org/research_data/ZA5318

pacman::p_load(tidyverse, haven, foreign, stringdist)

rm(list = ls())

# Set data path
path <- "data/raw/candidates/"


# Function for fuzzy string matching
fuzzy_string_match <- function(string_vec1, string_vec2, method = "jw") {
    out <- lapply(string_vec1, function(s) {
        d <- stringdist(s, string_vec2, method = method)
        m <- string_vec2 %>% .[which.min(d)]
        data.frame(orig_string = s, match_string = m, dist_string = min(d,
            na.rm = T
        ), stringsAsFactors = F)
    }) %>% reduce(rbind)
    out
}


# Load GLES 2017 candidate data -----------------------------------------

gles17 <- read.dta(paste0(path, "ZA6814_GLES17_Kandidaten_v3-0-0/ZA6814_v3-0-0.dta"))

# Define variable groups for GLES 2017 ----------------------------------

## 2017 ##

basevars <- c(
    "kandidaturtyp", "wkname", "wknr", "listenplatz",
    "geschlecht", "geburtsjahr", "a1"
)
backgroundvars <- c(
    "c3", "e9", "e10", "e11", "e15",
    "e18a", "e18b", "e18c", "a2", "b5"
)
previouselections <- c("a3a", "a3b", "a3c", "a3d", "a3e")
previous_political_act <- paste0("a4", letters[1:10])
nominationvars <- c("a7a", "a7b", "a8a", "a8b")

keepvars <- c(
    basevars, backgroundvars,
    previouselections,
    previous_political_act,
    nominationvars
)

# Select relevant variables for GLES 2017
gles17 <- gles17 %>%
    dplyr::select(keepvars)

# Recode and clean GLES 2017 variables ----------------------------------

gles17 <- gles17 %>%
    mutate_all(as.character) %>%
    mutate(
        kandidaturtyp = recode(kandidaturtyp,
            `Liste` = "list",
            `WK` = "direct",
            `WK & Liste` = "both"
        ),
        listenplatz = ifelse(as.numeric(listenplatz < 0), NA, listenplatz),
        female = ifelse(geschlecht == "weiblich", 1, 0),
        age = 2017 - as.numeric(geburtsjahr),
        party = a1,
        party_tenure = ifelse(as.numeric(a2) > 1900, 2017 - as.numeric(a2), NA),
        campaign_budget = ifelse(as.numeric(b5) > -1, as.numeric(b5), NA)
    ) %>%
    dplyr::select(-geschlecht, -age, -a1, -a2, -b5) %>%
    mutate(party = recode(party,
        `AfD` = "afd",
        `CDU` = "cdu_csu", `CSU` = "cdu_csu",
        `DIE LINKE` = "left",
        `FDP` = "fdp", `SPD` = "spd",
        `Buendnis 90/Die Gruenen` = "greens"
    ))

# Extract left-right position from text variable
gles17 <- gles17 %>%
    mutate(
        candidate_lr = str_remove_all(c3, "[a-zA-Z]"),
        candidate_lr = as.numeric(candidate_lr)
    )

# Recode previous election participation variables
gles17 <- gles17 %>%
    mutate_at(
        vars(a3a, a3b, a3c, a3d, a3e),
        ~ recode(.,
            `angetreten und gewaehlt` = "Elected",
            `angetreten, aber nicht gewaehlt` = "Ran",
            `nicht angetreten` = "DidNotRun", .default = "Missing"
        )
    ) %>%
    mutate_at(vars(a3a, a3b, a3c, a3d, a3e), ~ ifelse(. == "Missing", NA, .)) %>%
    rename(
        ran_13 = a3a, ran_09 = a3b, ran_05 = a3c, ran_02 = a3d,
        ran_98 = a3e
    )

# Recode previous political activity variables
gles17 <- gles17 %>%
    mutate_at(
        vars(a4a:a4j),
        ~ recode(.,
            `nicht zutreffend` = 0,
            `zutreffend` = 1
        )
    )

# Rename political activity variables
cnames <- c(
    "PA_Campaign_Unpaid",
    "PA_Campaign_Paid",
    "PA_EmployedParty",
    "PA_OfficeLocal",
    "PA_OfficeNational",
    "PA_Mayor",
    "PA_RepLocal",
    "PA_RepState",
    "PA_StateGovt",
    "PA_EuroMP"
)

colnames(gles17)[str_detect(colnames(gles17), "a4")] <- cnames

# Calculate mean political activity score
gles17$PA_Mean <- apply(gles17[, cnames], 1, mean, na.rm = T)

# Recode nomination influence variables
gles17$a7a <- factor(gles17$a7a)
levels(gles17$a7a) <- c(
    "Other", "DelegateAssembly", NA, NA,
    "MemberAssembly", "LeadershipLocal", "LeadershipNarional", NA
)
gles17$a7b <- factor(gles17$a7b)
levels(gles17$a7b) <- c(
    "Other", "DelegateAssembly", NA, NA,
    "MemberAssembly", "LeadershipLocal", "LeadershipNarional", NA
)

# Recode nomination competition variables
gles17 <- gles17 %>%
    mutate(a8a = recode(a8a,
        `etwas umkaempft` = "3Somewhat",
        `nicht umkaempft` = "1NotAtAll",
        `sehr umkaempft` = "4Very",
        `wenig umkaempft` = "2Slightly",
        .default = "Missing"
    )) %>%
    mutate_at(vars(a8b), ~ ifelse(. == "Missing", NA, .))

gles17 <- gles17 %>%
    mutate(a8b = recode(a8b,
        `etwas umkaempft` = "3Somewhat",
        `nicht umkaempft` = "1NotAtAll",
        `sehr umkaempft` = "4Very",
        `wenig umkaempft` = "2Slightly",
        .default = "Missing"
    )) %>%
    mutate_at(vars(a8b), ~ ifelse(. == "Missing", NA, .))

# Convert nomination variables to character
gles17 <- gles17 %>%
    mutate_at(vars(a7a, a7b, a8a, a8b), as.character)

# Calculate age variable
gles17 <- gles17 %>%
    mutate(age = 2017 - as.numeric(geburtsjahr))

# Recode education variables using factors
gles17$e9 <- factor(gles17$e9)
levels(gles17$e9) <- c(
    "Abitur", NA, "Student", "Abitur",
    NA, "Hauptschule", NA, "Realschule", "None"
)
gles17$e10 <- factor(gles17$e10)
levels(gles17$e10) <- c(
    rep("Vocational", 6), NA, "BA", "MA", "None", NA,
    "Vocational", "Student", "Student", "PhD"
)

# Convert education variables back to character
gles17 <- gles17 %>%
    mutate_at(vars(e9, e10), as.character)

# Recode employment status
gles17 <- gles17 %>%
    mutate(e11 = factor(e11))
levels(gles17$e11) <- c(
    "Unemployed", "NotOnLaborMarket", "Politician",
    NA, "PartTime", "NotOnLaborMarket", NA,
    "Retired", "FullTime", "PartTime", "FullTime"
)

gles17 <- gles17 %>%
    mutate(e11 = as.character(e11))

# Recode marital status
gles17 <- gles17 %>%
    mutate(e15 = recode(e15,
        `alleinstehend` = "Single",
        `geschieden oder in Trennung lebend` = "Divorced",
        `verwitwet` = "Widowed",
        `zusammenlebend: verheiratet/Partnerschaft` = "Married",
        .default = "Missing"
    )) %>%
    mutate_at(vars(e15), ~ ifelse(. == "Missing", NA, .))

# Recode household work variables
gles17 <- gles17 %>%
    mutate_at(
        vars(e18a, e18b, e18c),
        ~ recode(.,
            `gelegentlich` = "3Sometimes",
            `haeufig` = "4Often",
            `nie` = "1Never",
            `sehr haeufig` = "5VeryOften",
            `selten` = "2Rarely",
            .default = "Missing"
        )
    ) %>%
    mutate_at(vars(e18a, e18b, e18c), ~ ifelse(. == "Missing", NA, .))

# Rename variables for consistency
gles17 <- gles17 %>%
    rename(
        influence_nom_direct = a7a, influence_nom_list = a7b,
        competitve_nom_direct = a8a, competitve_nom_list = a8b,
        educ_sec = e9, educ_highest = e10, occ_status = e11, marital = e15,
        freq_housework = e18a, freq_childcare = e18b, freq_care_adult = e18c
    )

# Create indicator for first-time candidates
ranvars <- c("ran_13", "ran_09", "ran_05", "ran_02", "ran_98")

gles17$cand_first_time <- apply(gles17[, ranvars], 1, function(x) {
    ifelse(all(x == "DidNotRun", na.rm = T), 1, 0)
})

# Add election year
gles17$elec_year <- 2017

# Filter to direct candidates only
gles17 <- gles17 %>%
    dplyr::filter(kandidaturtyp != "list")

# Load and process GLES 2013 candidate data ------------------------------

#### GLES 2013 ####
## E2 is gender
## E3 is YOB
## A1 is party
## 2017->2013 :
## [e9-e11], [e10-e12], [e11-e13], [e15-e17], [e18*-missing]
## [a3*-a3*]
## [a4*-a4*]
## [a7*-a8*], [a8*-a9*]

gles13 <- read.dta(paste0(path, "ZA5716_GLES_Kandidaten13_v3-0-0/ZA5716_v3-0-0.dta"))

# Define variable groups for GLES 2013
basevars <- c(
    "kandidaturtyp", "wkname", "listenplatz",
    "c3", "e2", "e3", "a1"
)
backgroundvars <- c(
    "e11", "e12", "e13", "e17",
    "e18a", "e18b", "e18c", "a2", "b5"
)
previouselections <- c("a3a", "a3b", "a3c", "a3d", "a3e")
previous_political_act <- paste0("a4", letters[1:10])
nominationvars <- c("a8a", "a8b", "a9a", "a9b")

keepvars <- c(
    basevars, backgroundvars,
    previouselections,
    previous_political_act,
    nominationvars
)

# Select relevant variables for GLES 2013
gles13 <- gles13 %>%
    dplyr::select(keepvars)

# Recode and clean GLES 2013 variables
gles13 <- gles13 %>%
    mutate_all(as.character) %>%
    mutate(
        kandidaturtyp = recode(kandidaturtyp,
            `Liste` = "list",
            `WK` = "direct",
            `WK & Liste` = "both"
        ),
        listenplatz = ifelse(as.numeric(listenplatz < 0), NA, listenplatz),
        female = ifelse(e2 == "weiblich", 1, 0),
        age = 2013 - as.numeric(e3),
        party = a1,
        elec_year = 2013,
        party_tenure = ifelse(as.numeric(a2) > 1900, 2013 - as.numeric(a2), NA),
        campaign_budget = ifelse(as.numeric(b5) > -1, as.numeric(b5), NA)
    ) %>%
    dplyr::select(-e2, -age, -a1, -a2, -b5) %>%
    mutate(party = recode(party,
        `AfD` = "afd",
        `CDU` = "cdu_csu", `CSU` = "cdu_csu",
        `DIE LINKE` = "left",
        `FDP` = "fdp", `SPD` = "spd",
        `GRUENE` = "greens"
    )) %>%
    dplyr::filter(!party == "PIRATEN")

# Extract left-right position
gles13 <- gles13 %>%
    mutate(
        candidate_lr = str_remove_all(c3, "[a-zA-Z]"),
        candidate_lr = as.numeric(candidate_lr)
    )

# Recode previous election participation variables
gles13 <- gles13 %>%
    mutate_at(
        vars(a3a, a3b, a3c, a3d, a3e),
        ~ recode(.,
            `angetreten und gewaehlt` = "Elected",
            `angetreten, aber nicht gewaehlt` = "Ran",
            `nicht angetreten` = "DidNotRun", .default = "Missing"
        )
    ) %>%
    mutate_at(vars(a3a, a3b, a3c, a3d, a3e), ~ ifelse(. == "Missing", NA, .)) %>%
    rename(
        ran_09 = a3a, ran_05 = a3b, ran_02 = a3c, ran_98 = a3d,
        ran_94 = a3e
    )

# Recode previous political activity variables
gles13 <- gles13 %>%
    mutate_at(
        vars(a4a:a4j),
        ~ recode(.,
            `nicht zutreffend` = 0,
            `zutreffend` = 1
        )
    )

# Rename political activity variables
colnames(gles13)[str_detect(colnames(gles13), "a4")] <- cnames

# Calculate mean political activity score
gles13$PA_Mean <- apply(gles13[, cnames], 1, mean, na.rm = T)

# Recode nomination influence variables
gles13$a8a <- factor(gles13$a8a)
levels(gles13$a8a) <- c(
    "Other", "DelegateAssembly", NA,
    "MemberAssembly", "LeadershipNational", "LeadershipLocal", NA
)
gles13$a8b <- factor(gles13$a8b)
levels(gles13$a8b) <- c(
    "Other", "DelegateAssembly", NA,
    "MemberAssembly", "LeadershipNational", "LeadershipLocal", NA
)

# Recode nomination competition variables
gles13 <- gles13 %>%
    mutate(a9a = recode(a9a,
        `etwas umkaempft` = "3Somewhat",
        `nicht umkaempft` = "1NotAtAll",
        `sehr umkaempft` = "4Very",
        `wenig umkaempft` = "2Slightly",
        .default = "Missing"
    )) %>%
    mutate_at(vars(a9a), ~ ifelse(. == "Missing", NA, .))

gles13 <- gles13 %>%
    mutate(a9b = recode(a8b,
        `etwas umkaempft` = "3Somewhat",
        `nicht umkaempft` = "1NotAtAll",
        `sehr umkaempft` = "4Very",
        `wenig umkaempft` = "2Slightly",
        .default = "Missing"
    )) %>%
    mutate_at(vars(a9b), ~ ifelse(. == "Missing", NA, .))

# Convert nomination variables to character
gles13 <- gles13 %>%
    mutate_at(vars(a8a, a8b, a9a, a9b), as.character)

# Calculate age variable
gles13 <- gles13 %>%
    mutate(age = 2013 - as.numeric(e3))

# Recode education variables
gles13$e11 <- factor(gles13$e11)
levels(gles13$e11) <- c(
    "Abitur", "Student", "Abitur",
    "Hauptschule", NA, NA, "Realschule", "None"
)
gles13$e12 <- factor(gles13$e12)
levels(gles13$e12) <- c(
    rep("Vocational", 7), "BA", "MA", NA, "None", NA,
    "Vocational", "Student", "Student", "PhD"
)

# Convert education variables back to character
gles13 <- gles13 %>%
    mutate_at(vars(e11, e12), as.character)

# Recode employment status
gles13 <- gles13 %>%
    mutate(e13 = factor(e13))
levels(gles13$e13) <- c(
    "Unemployed", "NotOnLaborMarket", "Politician",
    "PartTime", "NotOnLaborMarket", NA, NA,
    "Retired", "FullTime", "PartTime", "FullTime"
)

gles13 <- gles13 %>%
    mutate(e13 = as.character(e13))

# Recode marital status
gles13 <- gles13 %>%
    mutate(e17 = recode(e17,
        `alleinstehend` = "Single",
        `geschieden oder in Trennung lebend` = "Divorced",
        `verwitwet` = "Widowed",
        `zusammenlebend: verheiratet/Partnerschaft` = "Married",
        .default = "Missing"
    )) %>%
    mutate_at(vars(e17), ~ ifelse(. == "Missing", NA, .))

# Set household work variables to missing (not available in 2013)
gles13 <- gles13 %>%
    mutate_at(vars(matches("e18")), ~ (. <- NA))

# Rename variables for consistency
gles13 <- gles13 %>%
    rename(
        influence_nom_direct = a8a, influence_nom_list = a8b,
        competitve_nom_direct = a9a, competitve_nom_list = a9b,
        educ_sec = e11, educ_highest = e12, occ_status = e13, marital = e17,
        freq_housework = e18a, freq_childcare = e18b, freq_care_adult = e18c,
        geburtsjahr = e3
    )

# Create indicator for first-time candidates
ranvars <- c("ran_09", "ran_05", "ran_02", "ran_98", "ran_94")

gles13$cand_first_time <- apply(gles13[, ranvars], 1, function(x) {
    ifelse(all(x == "DidNotRun", na.rm = T), 1, 0)
})

# Add election year
gles13$elec_year <- 2013

# Filter to direct candidates only
gles13 <- gles13 %>%
    dplyr::filter(kandidaturtyp != "list")

# Handle missing electoral district numbers for 2013
name_to_nr <- read.dta(paste0(path, "ZA6814_GLES17_Kandidaten_v3-0-0/ZA6814_v3-0-0.dta")) %>%
    dplyr::select(wkname, wknr) %>%
    distinct(wkname, .keep_all = T) %>%
    dplyr::filter(!wknr < 0)

# Clean strings for fuzzy matching
gles13$wkname <- iconv(gles13$wkname, "UTF-8", "UTF-8", sub = "")
name_to_nr$wkname <- iconv(name_to_nr$wkname, "UTF-8", "UTF-8", sub = "")

# Remove problematic characters
gles13$wkname <- gsub("[^\x01-\x7F]", "", gles13$wkname)
name_to_nr$wkname <- gsub("[^\x01-\x7F]", "", name_to_nr$wkname)

fuzzy_string_join <- function(df_left, df_right, by = "", quantile_dist_not_match = 0.5) {
    if (!((by %in% colnames(df_left)) & (by %in% colnames(df_right)))) {
        stop("By variable needs to be in both data frames")
    }
    sv1 <- df_left %>%
        pull(!!by) %>%
        as.character() %>%
        unique()
    sv2 <- df_right %>%
        pull(!!by) %>%
        as.character() %>%
        unique()
    fsm <- fuzzy_string_match(sv1, sv2)
    q <- quantile(fsm$dist_string, quantile_dist_not_match, na.rm = T)
    cat("Strings with distance greater than", q, "will not be matched")
    fsm <- fsm %>%
        mutate(match_string = ifelse(dist_string >
            q, NA_character_, match_string)) %>%
        dplyr::rename(`:=`(
            !!by,
            orig_string
        )) %>%
        dplyr::select(-dist_string)
    df_final <- df_left %>%
        left_join(fsm) %>%
        left_join(df_right %>%
            dplyr::rename(`:=`(match_string, !!by))) %>%
        dplyr::select(-match_string)
    df_final
}

# Perform fuzzy join to match electoral districts
gles13 <- fuzzy_string_join(gles13,
    name_to_nr,
    by = c("wkname"),
    quantile_dist_not_match = 0.975
)

# Load and process GLES 2009 candidate data ------------------------------

#### GLES 2009 ####
## e1 is gender
## e2 is YOB
## a1 is party
## 2017->2009 :
## [e9-e9], [e10-e10], [e11-e12], [e15-e16], [e18*-missing]
## [a3*-missing]
## [a4*- a18*,a19*]

gles09 <- read.dta(paste0(path, "ZA5318_GLES09_Kandidaten/ZA5318_v2-0-0.dta"))

# Define variable groups for GLES 2009
basevars <- c(
    "kand_typ", "wkname", "wknr",
    "e1", "e2", "b11", "a2", "a1", "c3"
)
backgroundvars <- c("e9", "e10", "e12", "e16")
previous_political_act <- colnames(gles09) %>% str_subset("a18|a19|a8")

keepvars <- c(
    basevars, backgroundvars,
    previous_political_act
)

gles09 <- gles09 %>%
    dplyr::select(one_of(keepvars))

# Filter out non-participants
gles09 <- gles09 %>%
    dplyr::filter(!e1 == "96. nicht teilgenommen") %>%
    dplyr::filter(!e1 == "99. keine Antwort")

# Recode missing values
gles09 <- gles09 %>%
    mutate_all(as.character) %>%
    mutate(across(a1:a19_7, ~ ifelse(str_detect(., paste0(as.character(96:99),
        collapse = "|"
    )),
    NA,
    .
    )))

# Recode and clean GLES 2009 variables
gles09 <- gles09 %>%
    mutate_all(as.character) %>%
    mutate(
        kand_typ = recode(kand_typ,
            `2. Liste` = "list",
            `1. Wahlkreis` = "direct",
            `3. Wahlkreis & Liste` = "both"
        ),
        female = ifelse(e1 == "2. weiblich", 1, 0),
        age = 2009 - ifelse(as.numeric(e2) > 1900, as.numeric(e2), NA),
        party = a1,
        elec_year = 2009,
        party_tenure = ifelse(as.numeric(a2) > 1900, 2009 - as.numeric(a2), NA),
        campaign_budget = ifelse(as.numeric(b11) > -1, as.numeric(b11), NA)
    ) %>%
    dplyr::select(-e2, -e2, -a1, -a2, -b11) %>%
    mutate(party = recode(party,
        `AfD` = "afd",
        `2. cdu` = "cdu_csu", `3. csu` = "cdu_csu",
        `6. Die Linke` = "left",
        `4. fdp` = "fdp", `1. spd` = "spd",
        `5. Bündnis 90/Die Grünen` = "greens", .default = NA_character_
    ))

# Extract left-right position
gles09 <- gles09 %>%
    mutate(
        candidate_lr = str_remove_all(c3, "[a-zA-Z]"),
        candidate_lr = as.numeric(candidate_lr)
    )

# Rename previous political activity variables
n_old <- c(
    "a8", "a8a",
    "a18_1", "a18_2", "a18_3",
    "a19_1", "a19_2", "a19_3",
    "a19_4",
    "a19_5", "a19_6", "a19_7"
)
n_new <- c(
    "PA_EmployedParty", "drop0",
    "PA_OfficeLocal", "drop1", "PA_OfficeNational",
    "PA_Mayor", "PA_StateGovt", "drop2",
    "PA_RepLocal",
    "PA_RepState", "drop3", "PA_EuroMP"
)

# Rename variables
gles09 <- gles09 %>%
    rename_at(vars(all_of(n_old)), ~ as.character(n_new)) %>%
    dplyr::select(-matches("drop"))

# Recode political activity variables
vars_PA <- n_new %>%
    str_subset("drop", negate = T)
attr(gles09, "label.table") <- NULL

gles09 <- gles09 %>%
    mutate(
        PA_EmployedParty = ifelse(str_detect(PA_EmployedParty, "nein"),
            0, 1
        ),
        PA_EuroMP = ifelse(str_detect(PA_EuroMP, "nie"),
            0, 1
        )
    ) %>%
    mutate(across(all_of(vars_PA), ~ ifelse(as.numeric(.) > 0, 1, 0)))

# Calculate mean political activity score
gles09$PA_Mean <- apply(gles09[, vars_PA], 1, mean, na.rm = T)

# Recode education variables
gles09$e9 <- factor(gles09$e9)
levels(gles09$e9) <- c(
    "Hauptschule", "Realschule", "Abitur", "Abitur",
    "MA", "PhD", "PhD", "None"
)

gles09 <- gles09 %>%
    mutate_at(vars(e9), as.character)

# Recode employment status
gles09 <- gles09 %>%
    mutate(e12 = factor(e12))
levels(gles09$e12) <- c(
    "FullTime", "NotOnLaborMarket", "FullTime",
    "PartTime", "PartTime", "NotOnLaborMarket",
    "Unemployed", "NotOnLaborMarket", "Retired", "NotOnLaborMarket"
)

gles09 <- gles09 %>%
    mutate(e12 = as.character(e12))

# Recode marital status
gles09 <- gles09 %>%
    mutate(e16 = recode(e16,
        `7. ledig` = "Single",
        `3. geschieden` = "Divorced",
        `4. verwitwet` = "Widowed",
        `1. verheiratet` = "Married",
        `2. verheiratet, aber getrennt lebend` = "Married",
        `5. in eheähnlicher Lebensgemeinschaft` = "Married",
        `6. verpartnert` = "Single",
        .default = "Missing"
    )) %>%
    mutate_at(vars(e16), ~ ifelse(. == "Missing", NA, .))

# Create vocational education indicator
gles09 <- gles09 %>%
    mutate(educ_voc = ifelse(e10 == "2. ja", 1, 0))

# Rename variables for consistency
gles09 <- gles09 %>%
    rename(educ_highest = e9, occ_status = e12, marital = e16) %>%
    dplyr::select(-e1, -e10)

# Harmonize education variable
gles09 <- gles09 %>%
    mutate(educ_highest = ifelse(educ_highest %in% c(
        "Abitur", "Hauptschule",
        "None", "Realschule"
    ) &
        educ_voc == 1,
    "Vocational",
    educ_highest
    ))

# Combine all GLES datasets into single harmonized dataset --------------

gles_full <- bind_rows(
    gles17, gles13 %>%
        mutate(wknr = as.character(wknr)),
    gles09
) %>%
    dplyr::select(
        -kandidaturtyp, -geburtsjahr, -wkname, -listenplatz,
        -female, -age, -kand_typ, -cand_first_time
    ) %>%
    rename(elec_district = wknr) %>%
    mutate(elec_district = as.numeric(elec_district)) %>%
    mutate(gles_included = 1) %>%
    dplyr::select(-c3, -educ_voc) %>%
    dplyr::select(
        -matches("ran"), -educ_sec, -matches("freq"),
        -matches("competitive|influence|competitve")
    ) %>%
    dplyr::select(-PA_Campaign_Unpaid, -PA_Campaign_Paid) %>%
    mutate(campaign_budget = ifelse(campaign_budget > 500000, NA, campaign_budget))

# Save harmonized candidate dataset
write_rds(gles_full, "data/intermediate/candidates/gles_candidates_clean.rds")