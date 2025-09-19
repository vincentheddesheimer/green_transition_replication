# Codebook for data_main.csv and cty_annual.csv

This codebook documents all variables in the main county-level datasets used for the analysis of "The Green Transition and Political Polarization Along Occupational Lines." The data_main.csv contains election-year observations, while cty_annual.csv contains annual observations from 1996-2021 (excluding election-specific variables).

## data_main.csv

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **county** | County identifier (5-digit AGS code) | Federal Statistical Office of Germany (Destatis) |
| **election_year** | Federal election year (1990, 1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021) | Federal Statistical Office of Germany (Destatis) |
| **state** | State identifier (2-digit code) | Derived from county code |
| **state_name** | State name | Derived from state code |
| **regbez** | Administrative district (3-digit code) | Derived from county code |
| **turnout** | Voter turnout (proportion of eligible voters who voted) | GERDA: The German Election Database |
| **spd** | Vote share for Social Democratic Party (SPD) | GERDA: The German Election Database |
| **gruene** | Vote share for Green Party (Bündnis 90/Die Grünen) | GERDA: The German Election Database |
| **fdp** | Vote share for Free Democratic Party (FDP) | GERDA: The German Election Database |
| **linke_pds** | Vote share for Left Party (Die Linke) and predecessor PDS | GERDA: The German Election Database |
| **afd** | Vote share for Alternative for Germany (AfD) | GERDA: The German Election Database |
| **cdu_csu** | Vote share for Christian Democratic Union/Christian Social Union (CDU/CSU) | GERDA: The German Election Database |
| **far_right** | Vote share for far-right parties (including AfD, NPD, DVU, REP) | GERDA: The German Election Database |
| **total_employees** | Total number of employees in county | Federal Statistical Office of Germany (Destatis) |
| **brownweights** | Number of employees in brown occupations (weighted classification) | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown1** | Number of employees in brown occupations (strict classification) | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown0_3** | Number of employees in brown occupations (0.3 threshold classification) | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown0_5** | Number of employees in brown occupations (0.5 threshold classification) | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **crosswalked** | Indicator for counties that underwent boundary harmonization | Derived from crosswalk process |
| **county_name** | County name | Federal Statistical Office of Germany (Destatis) |
| **area** | County area in square kilometers | Federal Institute for Research on Building, Urban Affairs, and Spatial Development (BBSR) |
| **population** | County population | Federal Institute for Research on Building, Urban Affairs, and Spatial Development (BBSR) |
| **pop_density** | Population density (inhabitants per square kilometer) | Federal Institute for Research on Building, Urban Affairs, and Spatial Development (BBSR) |
| **share_manufacturing_regstat** | Share of manufacturing employment in total employment (%) | Federal Statistical Office of Germany (Destatis) |
| **working_population_regstat** | Total working population | Federal Statistical Office of Germany (Destatis) |
| **manufacturing_regstat** | Number of employees in manufacturing sector | Federal Statistical Office of Germany (Destatis) |
| **unemployed_regstat** | Number of unemployed persons | Federal Statistical Office of Germany (Destatis) |
| **hh_income_vgrdl** | Household income per capita (in euros) | National economic accounts of the federal states (Volkswirtschaftliche Gesamtrechnung der Länder) |
| **schulabganger_mit_allgemeiner_hochschulreife_inkar** | Share of high school graduates with Abitur (%) | INKAR database |
| **auslanderanteil_inkar** | Share of foreign residents (%) | INKAR database |
| **arbeitslosenquote_inkar** | Unemployment rate (%) | INKAR database |
| **gdp_pc_ardeco** | GDP per capita (in euros) | ARDECO database |
| **n_crimes_2015** | Number of crimes in 2015 | Federal Criminal Police Office (BKA) - Polizeiliche Kriminalstatistik |
| **anti_refugee_violence_2015** | Number of anti-refugee violence incidents in 2015 | Bencek and
Strasheim (2016) |
| **anti_refugee_violence_crimeratio** | Anti-refugee violence incidents per 1000 crimes | Derived from the two sources above |
| **anti_refugee_violence_popratio** | Anti-refugee violence incidents per 100,000 inhabitants | Derived from Bencek and Strasheim (2016) and BBSR |
| **anteil_beschaftige_in_sektoren_mit_co2_fte_anstieg** | Share of employees in sectors with CO2 emissions increase |  Südekum and Rademacher (2024) |
| **isei_combined_2015** | International Socio-Economic Index of Occupational Status (2015) | SOEP data processed remotely |
| **zhb_dialectal_distance** | Dialectal distance from Hanover (inverted for interpretation) | Ziblatt, Hilbig, and Bischof (2024) |
| **afd_local_vote_share** | AfD vote share in local elections | GERDA: The German Election Database |
| **afd_local_ran** | Number of AfD candidates in local elections | GERDA: The German Election Database |
| **total_local_munis_cty** | Total number of municipalities in county | GERDA: The German Election Database |
| **afd_local_ran_prop** | Voter-weighted proportion of municipalities where AfD ran | GERDA: The German Election Database |
| **total_local_eligible_voters_cty** | Total eligible voters in local elections | GERDA: The German Election Database |
| **unemp_rate_regstat** | Unemployment rate calculated from employment data (%) | Federal Statistical Office of Germany (Destatis) |
| **post_2015** | Post-treatment indicator (1 if election year > 2015) | Derived from election year |
| **post_2019** | Post-treatment indicator (1 if election year > 2019) | Derived from election year |
| **brownness_share** | Share of employees in brown occupations (brownweights/total_employees) | Derived from employment data |
| **brown1_share** | Share of employees in brown occupations (brown1/total_employees) | Derived from employment data |
| **brown0_3_share** | Share of employees in brown occupations (brown0_3/total_employees) | Derived from employment data |
| **brown0_5_share** | Share of employees in brown occupations (brown0_5/total_employees) | Derived from employment data |

## cty_annual.csv

The annual county-level dataset includes the following additional variables:

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **year** | Year of observation (1996-2021) | Various sources |
| **employees** | Total number of employees in county | Federal Statistical Office of Germany (Destatis) |