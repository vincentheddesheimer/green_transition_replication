# Codebook for Candidate Data

This codebook documents all variables in the candidates dataset used for the analysis of "The Green Transition and Political Polarization Along Occupational Lines." The data includes electoral district-level information about AfD candidates, their occupational backgrounds, political experience, and district-level brown employment measures.

This data is proprietary. Please consult `README.md` for information on how to access this data.

## data_candidates.rds

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **wk_id** | Electoral district identifier | Federal Agency for Cartography and Geodesy |
| **brownweights** | Weighted count of brown employment in electoral district | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown1** | Count of highly brown employment (brownness score ≥ 1.0) in electoral district | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown0_3** | Count of moderately brown employment (brownness score ≥ 0.3) in electoral district | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **brown0_5** | Count of brown employment (brownness score ≥ 0.5) in electoral district | Vona et al. (2018), Cavalotti et al. (2025), German Federal Employment Agency |
| **total_employees** | Total number of employees in electoral district | Federal Statistical Office of Germany (Destatis) |
| **state** | German state (Bundesland) identifier | Derived from wk_id |
| **election_year** | Year of federal election (2013, 2017, 2021) | GLES Kandidatenstudie |
| **brownness_share** | Share of brown employment in total employment (brownweights/total_employees) | Derived from employment data |
| **brown1_share** | Share of highly brown employment in total employment | Derived from employment data |
| **brown0_3_share** | Share of moderately brown employment in total employment | Derived from employment data |
| **brown0_5_share** | Share of brown employment in total employment | Derived from employment data |
| **occupation** | Candidate's occupation title | Own classification based on GLES Candidate Study |
| **original_matched_kldb_title** | Original occupation title from KLdB classification | Own classification based on GLES Candidate Study |
| **matched_kldb_code** | KLdB (German classification of occupations) code | Own classification based on GLES Candidate Study |
| **isco08_code_1** | Primary ISCO-08 occupation code | Own classification based on GLES Candidate Study |
| **isco08_code_2** | Secondary ISCO-08 occupation code | Own classification based on GLES Candidate Study |
| **isco08_code_3** | Tertiary ISCO-08 occupation code | Own classification based on GLES Candidate Study |
| **isco08_code_4** | Quaternary ISCO-08 occupation code | Own classification based on GLES Candidate Study |
| **isco08_code_5** | Quinary ISCO-08 occupation code | Own classification based on GLES Candidate Study |
| **full_name** | Candidate's full name | Own classification based on GLES Candidate Study |
| **gles_educ_highest** | Candidate's highest level of education | GLES Candidate Study |
| **gles_occ_status** | Candidate's employment status | GLES Candidate Study |
| **gles_marital** | Candidate's marital status | GLES Candidate Study |
| **gles_pol_act_EmployedParty** | Binary indicator if candidate was employed by party | GLES Candidate Study |
| **gles_pol_act_OfficeLocal** | Binary indicator if candidate held local office | GLES Candidate Study |
| **gles_pol_act_OfficeNational** | Binary indicator if candidate held national office | GLES Candidate Study |
| **gles_pol_act_Mayor** | Binary indicator if candidate was mayor | GLES Candidate Study |
| **gles_pol_act_RepLocal** | Binary indicator if candidate was local representative | GLES Candidate Study |
| **gles_pol_act_RepState** | Binary indicator if candidate was state representative | GLES Candidate Study |
| **gles_pol_act_StateGovt** | Binary indicator if candidate was in state government | GLES Candidate Study |
| **gles_pol_act_EuroMP** | Binary indicator if candidate was European MP | GLES Candidate Study |
| **gles_party_tenure** | Years of party membership | GLES Candidate Study |
| **gles_campaign_budget** | Campaign budget in euros | GLES Candidate Study |
| **gles_candidate_lr** | Candidate's self-reported left-right position (0-10 scale) | GLES Candidate Study |
| **gles_pol_act_Mean** | Mean score across all political activity indicators | Derived from GLES political activity variables |
| **gles_included** | Binary indicator if candidate participated in GLES survey | Derived from GLES data availability |
| **afd_local_ran_prop** | Proportion of electoral districts where AfD fielded local candidates | County-level election data |
| **afd_fields_candidate** | Binary indicator if AfD fielded a candidate in this district | Derived from candidate data |
| **afd_fields_candidate_and_gles** | Binary indicator if AfD fielded a candidate who participated in GLES | Derived from candidate and GLES data |