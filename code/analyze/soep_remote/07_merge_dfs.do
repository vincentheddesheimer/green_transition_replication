** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP


/* 
This script merges the SOEP remote dataframe with four external datasets.

1) isco correspondence tables.

2) individual-level brownness data based on isco08 scores.

3) county-year level brownness data based on county identifiers.

4) individualized weights that account for compositional differences. 


We save the resulting dataframe in the intermediate files folder.
*/


********************
*** 1. Load SOEP ***
********************
clear

use $mydata/user/soepprep, clear


********************************************
*** 2. Merge with brownness isco08 table ***
********************************************

* Merge with the dataset preserved in memory using the specified variables
merge m:1 isco08 using $mydata/user/brownness_isco08
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

********************************************
*** 3. Merge with brownness isco88 table ***
********************************************


* Left join with the "vona" dataset
merge m:1 isco88 using $mydata/user/brownness_isco88
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

* Fill in values
replace greenness_08 = 0 if isco08 != . & missing(greenness_08)
replace brownness_08 = 0 if isco08 != . & missing(brownness_08)
replace greenness_88 = 0 if isco88 != . & missing(greenness_88)
replace brownness_88 = 0 if isco88 != . & missing(brownness_88)

* Create combined variables
gen greenness = greenness_08
gen brownness = brownness_08
replace greenness = greenness_88 if missing(greenness_08)
replace brownness = brownness_88 if missing(brownness_08)

* Create dummy variables
gen green_dummy = 0
gen brown_dummy = 0

replace green_dummy = 1 if greenness > 0.2
replace brown_dummy = 1 if brownness > 0.3
replace green_dummy = . if missing(labor_force_status) 
replace brown_dummy = . if missing(labor_force_status)

*** Create 2015 brownness variable
* Sort the data by pid and syear
sort pid syear

* Backup the main dataset
preserve

soepkeep if syear == 2015

soepkeep pid syear brownness labor_force_status
rename brownness brownness_2015
rename labor_force_status labor_force_status_2015

* Save this as a temporary dataset
save $mydata/user/brownness_2015, replace

* Restore the main dataset
restore

* Merge the temporary dataset with the main dataset
merge m:1 pid using $mydata/user/brownness_2015
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

* Create 2015 dummy
gen brown_dummy_2015 = 0
replace brown_dummy_2015 = 1 if brownness_2015 > 0.3
replace brown_dummy_2015 = . if missing(labor_force_status_2015)
soepdrop labor_force_status_2015


************************************************
*** 4. Merge with county brownness dataframe ***
************************************************

* Merge with the dataset preserved in memory using the specified variables
merge m:1 county using $mydata/user/cty_brownness_covars
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge


*************************
*** 3. Save dataframe ***
*************************

save $mydata/user/merged_brown_soep, replace 

* End Code
