** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

quietly{

use $mydata/user/merged_brown_soep, replace 

*** Create 1999 brownness variable
* Sort the data by pid and syear
sort pid syear

* Backup the main dataset
preserve

* only the year 1999
soepkeep if syear == 1999

* only the necessary variables
soepkeep pid syear brownness
rename brownness brownness_1999

* Save this as a temporary dataset
save $mydata/user/brownness_1999, replace

* Restore the main dataset
restore

* Merge the temporary dataset with the main dataset
merge m:1 pid using $mydata/user/brownness_1999
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

* Create 1999 dummy
gen brown_dummy_1999 = 0
replace brown_dummy_1999 = 1 if brownness_1999 > 0.3
replace brown_dummy_1999 = . if missing(brownness_1999)

*** Create brown share 1999 variables
* Merge with the dataset preserved in memory using the specified variables
merge m:1 county using $mydata/user/cty_brownness_1999
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

save $mydata/user/merged_brown_soep_1999, replace
}
