# Codebook for data_soep.csv

This codebook documents all variables in the SOEP (German Socio-Economic Panel) datasets used for the analysis of "The Green Transition and Political Polarization Along Occupational Lines." We use three different versions of the SOEP.

1.  Socio-Economic Panel, Update data from 1984-2021 (SOEP-Core, v38.1, International Edition - Update), 2023, doi:10.5684/soep.core.v38.1i
2.  Socio-Economic Panel, data from 1984-2021 (SOEP-Core, v38.1, Remote Edition - Update) 2023, doi:10.5684/soep.core.v38.1r
3.  SOEP-Innovationssample (SOEP-IS), Daten der Jahre 1998-2021. 2023. DOI: 10.5684/soep.is.2021.

This data is proprietary. Please consult `README.md` for information on how to access this data.

## data_soep.csv

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **pid** | Person identifier (unique individual ID) | SOEP-Core v38.1 |
| **piyear** | Interview year | SOEP-Core v38.1 |
| **syear** | Survey year | SOEP-Core v38.1 |
| **cid** | Case identifier | SOEP-Core v38.1 |
| **hid** | Household identifier | SOEP-Core v38.1 |
| **weighting_factor** | Survey weighting factor (phrf) | SOEP-Core v38.1 |
| **eintritt** | Entry year into panel | SOEP-Core v38.1 |
| **austritt** | Exit year from panel | SOEP-Core v38.1 |
| **sampreg** | Sample region | SOEP-Core v38.1 |
| **sex** | Sex (0=male, 1=female) | SOEP-Core v38.1 |
| **birth_year** | Birth year | SOEP-Core v38.1 |
| **partner** | Partnership status (0=no partner, 1=has partner) | SOEP-Core v38.1 |
| **germborn** | Born in Germany (0=no, 1=yes) | SOEP-Core v38.1 |
| **corigin** | Country of origin | SOEP-Core v38.1 |
| **migback** | Migration background (0=no, 1=yes) | SOEP-Core v38.1 |
| **iyear** | Interview year | SOEP-Core v38.1 |
| **pmonin** | Interview month | SOEP-Core v38.1 |
| **ptagin** | Interview day | SOEP-Core v38.1 |
| **pol_tendency** | Political tendency left-right scale | SOEP-Core v38.1 |
| **pol_interest** | Interest in politics (inverted scale) | SOEP-Core v38.1 |
| **partisan** | Party identification (0=no, 1=yes) | SOEP-Core v38.1 |
| **party_aff** | Party affiliation | SOEP-Core v38.1 |
| **party_aff_intensity** | Party affiliation intensity (inverted scale) | SOEP-Core v38.1 |
| **satisf_work** | Satisfaction with work (0-10 scale) | SOEP-Core v38.1 |
| **satisf_hh_income** | Satisfaction with household income (0-10 scale) | SOEP-Core v38.1 |
| **satisf_ind_income** | Satisfaction with individual income (0-10 scale) | SOEP-Core v38.1 |
| **self_esteem** | Self-esteem measure | SOEP-Core v38.1 |
| **worried_econ_dev** | Worried about economic development (inverted scale) | SOEP-Core v38.1 |
| **worried_finances** | Worried about finances (inverted scale) | SOEP-Core v38.1 |
| **worried_environment** | Worried about environment (inverted scale) | SOEP-Core v38.1 |
| **worried_climate_change** | Worried about climate change (inverted scale) | SOEP-Core v38.1 |
| **worried_job_security** | Worried about job security (inverted scale) | SOEP-Core v38.1 |
| **worried_migration** | Worried about migration (inverted scale) | SOEP-Core v38.1 |
| **likely_job_loss** | Likelihood of job loss | SOEP-Core v38.1 |
| **likely_job_demotion** | Likelihood of job demotion | SOEP-Core v38.1 |
| **monthly_income** | Monthly income in euros | SOEP-Core v38.1 |
| **labor_force_status** | Labor force status | SOEP-Core v38.1 |
| **labor_income_gross** | Gross labor income | SOEP-Core v38.1 |
| **labor_income_net** | Net labor income | SOEP-Core v38.1 |
| **occupational_position** | Occupational position | SOEP-Core v38.1 |
| **isco88** | ISCO-88 occupational classification (4-digit) | SOEP-Core v38.1 |
| **isco08** | ISCO-08 occupational classification (4-digit) | SOEP-Core v38.1 |
| **isced97** | ISCED-1997 education classification | SOEP-Core v38.1 |
| **isced11** | ISCED-2011 education classification | SOEP-Core v38.1 |
| **isei08** | International Socio-Economic Index (ISEI-08, standardized) | SOEP-Core v38.1 |
| **isei88** | International Socio-Economic Index (ISEI-88, standardized) | SOEP-Core v38.1 |
| **kldb92** | German occupational classification (KldB92) | SOEP-Core v38.1 |
| **nace** | Industry classification (NACE Rev. 1.1) | SOEP-Core v38.1 |
| **nace2** | Industry classification (NACE Rev. 2) | SOEP-Core v38.1 |
| **time_w_firm** | Time with current firm | SOEP-Core v38.1 |
| **edu** | Education level | SOEP-Core v38.1 |
| **satisf_life** | Life satisfaction (0-10 scale) | SOEP-Core v38.1 |
| **state** | State identifier (character) | SOEP-Core v38.1 |
| **state_name** | State name | Derived from state code |
| **female** | Female indicator (0=male, 1=female) | Derived from sex |
| **male** | Male indicator (0=female, 1=male) | Derived from sex |
| **heterosex** | Heterosexual indicator | SOEP-Core v38.1 |
| **SPD** | Social Democratic Party affiliation (0=no, 1=yes) | Derived from party_aff |
| **CDU** | Christian Democratic Union affiliation (0=no, 1=yes) | Derived from party_aff |
| **CSU** | Christian Social Union affiliation (0=no, 1=yes) | Derived from party_aff |
| **FDP** | Free Democratic Party affiliation (0=no, 1=yes) | Derived from party_aff |
| **Gruene** | Green Party affiliation (0=no, 1=yes) | Derived from party_aff |
| **Linke** | Left Party affiliation (0=no, 1=yes) | Derived from party_aff |
| **NPD_Republikaner_Rechte** | Far-right party affiliation (0=no, 1=yes) | Derived from party_aff |
| **AfD** | Alternative for Germany affiliation (0=no, 1=yes) | Derived from party_aff |
| **CDUCSU** | CDU/CSU combined affiliation (0=no, 1=yes) | Derived from party_aff |
| **center_right** | Center-right party affiliation (0=no, 1=yes) | Derived from party_aff |
| **center_left** | Center-left party affiliation (0=no, 1=yes) | Derived from party_aff |
| **far_right** | Far-right party affiliation (0=no, 1=yes) | Derived from party_aff |
| **age** | Age in years | Derived from birth_year and piyear |
| **emp** | Employment status (employed, unemployed, nilf) | Derived from labor_force_status |
| **isei_combined** | Combined ISEI score (standardized) | Derived from isei08 and isei88 |
| **has_abitur_2015** | Has Abitur (high school diploma) in 2015 (0=no, 1=yes) | Derived from edu in 2015 |
| **has_higher_ed_2015** | Has higher education in 2015 (0=no, 1=yes) | Derived from isced97 in 2015 |
| **east_2015** | East Germany indicator in 2015 (0=no, 1=yes) | Derived from state in 2015 |
| **female_2015** | Female indicator in 2015 (0=no, 1=yes) | Derived from sex in 2015 |
| **income_above_med_2015** | Income above median in 2015 (0=no, 1=yes) | Derived from monthly_income in 2015 |
| **age_above_50_2015** | Age above 50 in 2015 (0=no, 1=yes) | Derived from age in 2015 |
| **manufacturing_job_2015** | Manufacturing job in 2015 (0=no, 1=yes) | Derived from nace2 in 2015 |
| **worried_migration_high_2015** | High migration worry in 2015 (0=no, 1=yes) | Derived from worried_migration in 2015 |
| **date** | Interview date | Derived from syear, pmonin, ptagin |
| **greenness** | Green occupation score | Vona et al. (2018), Cavalotti et al. (2025) |
| **brownness** | Brown occupation score | Vona et al. (2018), Cavalotti et al. (2025) |
| **green_dummy** | Green occupation indicator (0=no, 1=yes) | Derived from greenness |
| **brown_dummy** | Brown occupation indicator (0=no, 1=yes) | Derived from brownness |
| **observed_2015** | Observed in 2015 (0=no, 1=yes) | Derived from syear |
| **brown_dummy_2015** | Brown occupation indicator in 2015 (0=no, 1=yes) | Derived from brown_dummy in 2015 |
| **post_2015** | Post-2015 indicator (0=no, 1=yes) | Derived from syear |
| **post_2019** | Post-2019 indicator (0=no, 1=yes) | Derived from syear |


## Additional variables used via SOEP remote access

For the county-level treatment analysis we also use respondents' county identifiers.

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **county** | County identifier (5-digit AGS code) | SOEP-Core, v38.1, Remote Edition |
| **brown0_3_share_2015** | County brown employment share (0.3 threshold) in 2015 | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |


## data_soep_is.csv: additional variables used in SOEP Innovation Sample

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **iss1** | Status in social environment (standardized) | SOEP-IS 2021 |
| **iss2** | Status in Germany (standardized) | SOEP-IS 2021 |