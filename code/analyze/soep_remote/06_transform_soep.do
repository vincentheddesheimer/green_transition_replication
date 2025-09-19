** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP


/* 
This script prepares the SOEP remote dataframe with the external county-year level data based on county identifiers.

We save the resulting dataframe in the intermediate files folder.
*/


*quietly {

***************************
*** Transform SOEP data ***
***************************

use $mydata/user/soepmerged, clear


* rename variables
* Assuming your Stata dataset is named "df"

* rename variables
rename phrf weighting_factor
rename gebjahr birth_year
rename plh0004 pol_tendency
rename plh0007 pol_interest
rename plh0011_h partisan
rename plh0012_h party_aff
rename plh0013_h party_aff_intensity
rename plh0333 party_last_election
rename pli0097_h particip_politics
rename plh0263_h union_member
rename plh0111 important_particip_politics
rename plh0173 satisf_work
rename plh0175 satisf_hh_income
rename plh0176 satisf_ind_income
rename plh0182 satisf_life
rename plh0206i11 self_esteem
rename plh0032 worried_econ_dev
rename plh0033 worried_finances
rename plh0036 worried_environment
rename plh0037 worried_climate_change
rename plh0042 worried_job_security
rename plj0046 worried_migration
rename plb0433_v2 likely_job_loss
rename plb0438_v2 likely_job_demotion
rename plc0013_h monthly_income
rename pglfs labor_force_status
rename pglabgro labor_income_gross
rename pglabnet labor_incomenet
rename pgstib occupational_position
rename pgjobch occupational_change
rename pgjobend occupational_change_reason
rename pgerwzeit time_w_firm
rename pgpsbil edu
rename pgisco88 isco88
rename pgisco08	isco08
rename pgisei08 isei08
rename pgisei88 isei88
rename pgkldb92 kldb92
rename pgnace nace
rename pgnace2 nace2
rename pgisced97 isced97
rename pgisced11 isced11
rename plb0113 think_work_problems_morning
rename plb0114 easy_stop_thinking_work
rename plb0115 sacrifices_for_career
rename plb0116 always_thinking_work
rename plb0117 sleeping_problems_work
rename plb0124 job_burden_chance_promotion
rename plb0125 job_burden_chance_promotion_scl
rename plb0126 job_burden_worsening_work
rename plb0127 job_burden_worsening_work_scl
rename plb0128 job_burden_job_jeopardy
rename plb0129 job_burden_job_jeopardy_scl
rename plb0176_h working_hours
rename plh0184 freq_angry
rename plh0185 freq_worried
rename plh0186 freq_happy
rename plh0187 freq_sad
rename ple0027 last4_melancholy
rename ple0028 last4_calm
rename ple0029 last4_energetic
rename ple0033 last4_accompl_less_emo
rename plh0334 activities_useful

* Recode variables
replace piyear = cond(piyear == -2, ., piyear)
gen county = cond(kkz_rek < 0, ., kkz_rek)
gen female = 0
replace female = cond(sex == 2, 1, 0)
replace female = . if missing(sex)
gen male = 0
replace male = cond(sex == 1, 1, 0)
replace male = . if missing(sex)
replace birth_year = cond(birth_year == -1, ., birth_year)
replace partner = cond(partner > 0 & partner < 5, 1, 0)
replace pol_tendency = cond(pol_tendency < 0, ., pol_tendency)
replace pol_interest = cond(pol_interest < 0, ., abs(pol_interest - 4))
replace partisan = cond(partisan == 1, 1, cond(partisan == 2, 0, .))
replace party_aff_intensity = cond(party_aff_intensity < 0, ., ///
	abs(party_aff_intensity - 5))
// party affiliation
replace party_aff = . if party_aff < 0
gen SPD = 0
gen CDU = 0
gen CSU = 0
gen FDP = 0
gen Gruene = 0
gen Linke = 0
gen NPD_Republikaner_Rechte = 0
gen AfD = 0
gen CDUCSU = 0
gen center_right = 0
gen center_left = 0
gen far_right = 0
replace SPD = cond(party_aff == 1, 1, 0)
replace CDU = cond(party_aff == 2, 1, 0)
replace CSU = cond(party_aff == 3, 1, 0)
replace FDP = cond(party_aff == 4, 1, 0)
replace Gruene = cond(party_aff == 5, 1, 0)
replace Linke = cond(party_aff == 6, 1, 0)
replace NPD_Republikaner_Rechte = cond(party_aff == 7, 1, 0)
replace AfD = cond(party_aff == 27, 1, 0)
replace AfD = . if syear < 2013
replace CDUCSU = cond(CDU == 1 | CSU == 1 | party_aff == 13, 1, 0)
replace center_right = cond(CDUCSU == 1 | FDP == 1, 1, 0)
replace center_left = cond(SPD == 1 | Gruene == 1, 1, 0)
replace far_right = cond(NPD_Republikaner_Rechte == 1 | AfD == 1, 1, 0)
replace SPD = . if missing(party_aff)
replace CDU = . if missing(party_aff)
replace CSU = . if missing(party_aff)
replace FDP = . if missing(party_aff)
replace Gruene = . if missing(party_aff)
replace Linke = . if missing(party_aff)
replace NPD_Republikaner_Rechte = . if missing(party_aff)
replace AfD = . if missing(party_aff)
replace CDUCSU = . if missing(party_aff)
replace center_right = . if missing(party_aff)
replace center_left = . if missing(party_aff)
replace far_right = . if missing(party_aff)
// party last election
replace party_last_election = . if party_last_election < 0
gen SPD_last_election = 0
gen CDU_last_election = 0
gen CSU_last_election = 0
gen FDP_last_election = 0
gen Gruene_last_election = 0
gen Linke_last_election = 0
gen NPD_Rep_Rechte_last_election = 0
gen AfD_last_election = 0
gen CDUCSU_last_election = 0
gen center_right_last_election = 0
gen center_left_last_election = 0
gen far_right_last_election = 0
replace SPD_last_election = cond(party_last_election == 1, 1, 0)
replace CDU_last_election = cond(party_last_election == 2, 1, 0)
replace CSU_last_election = cond(party_last_election == 3, 1, 0)
replace FDP_last_election = cond(party_last_election == 4, 1, 0)
replace Gruene_last_election = cond(party_last_election == 5, 1, 0)
replace Linke_last_election = cond(party_last_election == 6, 1, 0)
replace NPD_Rep_Rechte_last_election = cond(party_last_election == 7, 1, 0)
replace AfD_last_election = cond(party_last_election == 27, 1, 0)
replace AfD_last_election = . is syear < 2013
replace CDUCSU_last_election = cond(CDU == 1 | CSU == 1 ///
	| party_last_election == 13, 1, 0)
replace center_right_last_election = cond(CDUCSU == 1 | FDP == 1, 1, 0)
replace center_left_last_election = cond(SPD == 1 | Gruene == 1, 1, 0)
replace far_right_last_election = cond(NPD_Republikaner_Rechte == 1 ///
	| AfD == 1, 1, 0)
replace SPD_last_election = . if missing(party_last_election)
replace CDU_last_election = . if missing(party_last_election)
replace CSU_last_election = . if missing(party_last_election)
replace FDP_last_election = . if missing(party_last_election)
replace Gruene_last_election = . if missing(party_last_election)
replace Linke_last_election = . if missing(party_last_election)
replace NPD_Rep_Rechte_last_election = . if missing(party_last_election)
replace AfD_last_election = . if missing(party_last_election)
replace CDUCSU_last_election = . if missing(party_last_election)
replace center_right_last_election = . if missing(party_last_election)
replace center_left_last_election = . if missing(party_last_election)
replace far_right_last_election = . if missing(party_last_election)
// Others
* Assuming your Stata dataset is named "df"

* Handle missing values and transformations
replace particip_politics = . if particip_politics < 0
replace union_member = . if union_member < 0
replace important_particip_politics = . if important_particip_politics < 0
replace satisf_work = . if satisf_work < 0
replace satisf_hh_income = . if satisf_hh_income < 0
replace satisf_ind_income = . if satisf_ind_income < 0
replace satisf_life = . if satisf_life < 0
replace self_esteem = . if self_esteem < 0
replace worried_econ_dev = . if worried_econ_dev < 0
replace worried_finances = . if worried_finances < 0
replace worried_environment = . if worried_environment < 0
replace worried_climate_change = . if worried_climate_change < 0
replace worried_job_security = . if worried_job_security < 0
replace worried_migration = . if worried_migration < 0
replace likely_job_loss = . if likely_job_loss < 0
replace likely_job_demotion = . if likely_job_demotion < 0
replace monthly_income = . if monthly_income < 0
replace labor_force_status = . if labor_force_status < 0
replace labor_income_gross = . if labor_income_gross < 0
replace labor_incomenet = . if labor_incomenet < 0
replace occupational_change = . if occupational_change < 0
replace occupational_change = 1 if occupational_change > 2
replace occupational_change = 0 if occupational_change == 1 ///
	| occupational_change == 2
replace occupational_change_reason = . if occupational_change_reason < 0
gen occ_chng_term_by_emp = cond(occupational_change_reason == 1, 1, 0)
gen occ_chng_own_resig = cond(occupational_change_reason == 4, 1, 0)
gen occ_chng_mutual_resig = cond(occupational_change_reason == 5, 1, 0)
gen occ_chng_comp_closed = cond(occupational_change_reason == 11, 1, 0)
replace isco88 = . if isco88 < 0
replace isco08 = . if isco08 < 0
replace isced97 = . if isced97 < 0
replace isced11 = . if isced11 < 0
replace isei08 = . if isei08 < 0
replace isei88 = . if isei88 < 0
replace kldb92 = . if kldb92 < 0
replace nace = . if nace < 0
replace nace2 = . if nace2 < 0
replace time_w_firm = . if time_w_firm < 0
replace edu = . if edu > 4 | edu < 1
replace think_work_problems_morning = . if think_work_problems_morning < 0
replace think_work_problems_morning = think_work_problems_morning - 1
replace easy_stop_thinking_work = . if easy_stop_thinking_work < 0
replace easy_stop_thinking_work = easy_stop_thinking_work - 1
replace sacrifices_for_career = . if sacrifices_for_career < 0
replace sacrifices_for_career = sacrifices_for_career - 1
replace always_thinking_work = . if always_thinking_work < 0
replace always_thinking_work = always_thinking_work - 1
replace sleeping_problems_work = . if sleeping_problems_work < 0
replace sleeping_problems_work = sleeping_problems_work - 1
replace job_burden_chance_promotion = . if job_burden_chance_promotion < 0
replace job_burden_chance_promotion = abs(job_burden_chance_promotion - 2)
replace job_burden_chance_promotion_scl = . if ///
	job_burden_chance_promotion_scl < 0
replace job_burden_chance_promotion_scl = job_burden_chance_promotion_scl - 1
replace job_burden_worsening_work = . if job_burden_worsening_work < 0
replace job_burden_worsening_work = abs(job_burden_worsening_work - 2)
replace job_burden_worsening_work_scl = . if job_burden_worsening_work_scl < 0
replace job_burden_worsening_work_scl = job_burden_worsening_work_scl - 1
replace job_burden_job_jeopardy = . if job_burden_job_jeopardy < 0
replace job_burden_job_jeopardy = abs(job_burden_job_jeopardy - 2)
replace job_burden_job_jeopardy_scl = . if job_burden_job_jeopardy_scl < 0
replace job_burden_job_jeopardy_scl = job_burden_job_jeopardy_scl - 1
replace working_hours = . if working_hours < 0
replace freq_angry = . if freq_angry < 0
replace freq_worried = . if freq_worried < 0
replace freq_happy = . if freq_happy < 0
replace freq_sad = . if freq_sad < 0
replace last4_melancholy = . if last4_melancholy < 0
replace last4_melancholy = abs(last4_melancholy - 5)
replace last4_calm = . if last4_calm < 0
replace last4_calm = abs(last4_calm - 5)
replace last4_energetic = . if last4_energetic < 0
replace last4_energetic = abs(last4_energetic - 5)
replace last4_accompl_less_emo = . if last4_accompl_less_emo < 0
replace last4_accompl_less_emo = abs(last4_accompl_less_emo - 5)
replace activities_useful = . if activities_useful < 0
gen age = piyear - birth_year
replace sex = . if sex < 0
replace sex = sex - 1
gen state = string(bula)
replace migback = cond(migback > 1, 1, 0)

* Calculate the 99.99th percentile value using egen
egen q9999 = pctile(monthly_income), p(99.99)

* Set monthly_income to missing if it is greater than q9999
replace monthly_income = . if monthly_income > q9999


// Sort the data by syear
sort syear

// Calculate means and standard deviations for isei08 and 
// isei88 within each syear group
by syear: egen isei08_mean = mean(isei08)
by syear: egen isei08_sd = sd(isei08)
by syear: egen isei88_mean = mean(isei88)
by syear: egen isei88_sd = sd(isei88)

// Standardize isei08 and isei88 within each syear group
replace isei08 = (isei08 - isei08_mean) / isei08_sd
replace isei88 = (isei88 - isei88_mean) / isei88_sd

// Ungroup the data
soepdrop isei08_mean isei08_sd isei88_mean isei88_sd

// Calculate isei_combined by coalescing isei08 and isei88
gen isei_combined = cond(!missing(isei08), isei08, isei88)


// Sort the data by pid and syear
sort pid syear

// Set the data as panel data
xtset pid syear

// Create change variables
by pid: gen delta_CDUCSU = CDUCSU - L.CDUCSU
by pid: gen delta_FDP = FDP - L.FDP
by pid: gen delta_SPD = SPD - L.SPD
by pid: gen delta_Gruene = Gruene - L.Gruene
by pid: gen delta_Linke = Linke - L.Linke
by pid: gen delta_NPD_Republikaner_Rechte = ///
	NPD_Republikaner_Rechte - L.NPD_Republikaner_Rechte
by pid: gen delta_center_right = center_right - L.center_right
by pid: gen delta_center_left = center_left - L.center_left


* Create the post-2013 & post 2015 variables
gen post_2013 = (syear > 2013)
gen post_2015 = (syear > 2015)

* Create the manufacturing employment variable
// We have Nace1 codes until 2017 and Nace2 codes beginning in 2013. 
// Manufacturing Nace1 codes: all between 15-37
// Manufactruing Nace2 codes: all between 10-33
* For Nace1 codes until 2017
gen manufacturing_nace1 = cond(syear <= 2017 & inrange(nace, 15, 37), 1, 0)
replace manufacturing_nace1 = . if syear > 2017

* For Nace2 codes beginning in 2013
gen manufacturing_nace2 = cond(syear >= 2013 & inrange(nace2, 10, 33), 1, 0)
replace manufacturing_nace2 = . if syear < 2013

* For years with Nace2 codes
gen manufacturing = manufacturing_nace2 if !missing(manufacturing_nace2)

* For years with only Nace1 codes
replace manufacturing = manufacturing_nace1 if ///
	missing(manufacturing_nace2) & !missing(manufacturing_nace1)


* Create the emp variable
gen emp = ""
replace emp = "nilf" if (labor_force_status >= 1 & labor_force_status <= 5) ///
	| (labor_force_status >= 7 & labor_force_status <= 9)
replace emp = "employed" if labor_force_status == 10 ///
	| labor_force_status == 11 | labor_force_status == 12
replace emp = "unemployed" if labor_force_status == 6


*** Create manufacturing 2013
sort pid syear

* Backup the main dataset
preserve

* Soepkeep only the year 2013
soepkeep if syear == 2013

* Soepkeep only the necessary variables
soepkeep pid syear manufacturing
rename manufacturing manufacturing_2013

* Save this as a temporary dataset
save $mydata/user/manufacturing_2013, replace

* Restore the main dataset
restore

* Merge the temporary dataset with the main dataset
merge m:1 pid using $mydata/user/manufacturing_2013
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge


**********************
*** Save dataframe ***
**********************

save $mydata/user/soepprep, replace 

*}


* End of Code
