** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/soep_is_merged, clear


* rename variables
* Assuming your Stata dataset is named "df"

* rename variables
rename gebjahr birth_year
rename plj0046 worried_migration
rename plc0013 monthly_income
rename pglfs labor_force_status
rename pglabgro labor_income_gross
rename pglabnet labor_incomenet
rename pgstib occupational_position
rename pgjobch occupational_change
rename pgsbil edu
rename pgis88 isco88
rename pgis08 isco08
rename pgisei08 isei08
rename pgisei88 isei88
rename pgnace nace
rename pgnace2 nace2
rename pgisced isced

* Recode variables
replace iyear = cond(iyear == -2, ., iyear)
gen county = cond(kkz_rek < 0, ., kkz_rek)
gen female = 0
replace female = cond(sex == 2, 1, 0)
replace female = . if missing(sex)
gen male = 0
replace male = cond(sex == 1, 1, 0)
replace male = . if missing(sex)
replace birth_year = cond(birth_year == -1, ., birth_year)
replace worried_migration = . if worried_migration < 0
replace monthly_income = . if monthly_income < 0
replace labor_force_status = . if labor_force_status < 0
replace labor_income_gross = . if labor_income_gross < 0
replace labor_incomenet = . if labor_incomenet < 0
replace occupational_change = . if occupational_change < 0
replace occupational_change = 1 if occupational_change > 2
replace occupational_change = 0 if occupational_change == 1 ///
	| occupational_change == 2
replace isco88 = . if isco88 < 0
replace isco08 = . if isco08 < 0
replace isced = . if isced < 0
replace isei08 = . if isei08 < 0
replace isei88 = . if isei88 < 0
replace nace = . if nace < 0
replace nace2 = . if nace2 < 0
replace iss1 = . if iss1 < 0
replace iss2 = . if iss2 < 0
replace edu = . if edu > 4 | edu < 1
gen age = syear - birth_year
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
save $mydata/user/is_manufacturing_2013, replace

* Restore the main dataset
restore

* Merge the temporary dataset with the main dataset
merge m:1 pid using $mydata/user/is_manufacturing_2013
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge


**********************
*** Save dataframe ***
**********************

save $mydata/user/is_soepprep, replace 

*}


* End of Code
