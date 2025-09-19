** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP


*quietly {

* Rooting 
use pid cid eintritt austritt sex gebjahr germborn corigin migback ///
	using $soep_is_2020/ppfad, clear
// Save
save $mydata/user/is_ppathl, replace 

* Survey info
use pid syear iyear imonth iday using $soep_is_2020/bio, clear
// Save
save $mydata/user/is_bio, replace 

* Status
use pid syear iss1 iss2 using $soep_is_2020/inno, clear
save $mydata/user/inno, replace 

* Prepare dataset pgen
use pid hid syear pglfs pglabgro pglabnet pgstib pgjobch pgis88 pgis08 ///
	pgisced pgisei08 pgisei88 pgnace pgnace2 pgsbil ///
	using $soep_is_2020/pgen, clear 
save $mydata/user/is_pgen, replace 

* Other
use pid syear plc0013 plj0046 using $soep_is_2020/p, clear
save $mydata/user/is_p, replace 

* Prepare dataset regionl
use hid syear bula using $soep_is_2020/regionl, clear 
save $mydata/user/is_regionl, replace

* Prepare dataset kkz
use hid syear kkz_rek using ${soep_is_2020}/is_kkz_rek.dta, clear 
save $mydata/user/is_kkz_rek, replace 


*** Merge all datasets ***
use $mydata/user/is_pgen
merge m:1 pid using $mydata/user/is_ppathl
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge
merge m:1 pid syear using $mydata/user/is_bio
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge
merge m:1 pid syear using $mydata/user/inno
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge
merge m:1 pid syear using $mydata/user/is_p
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge
merge m:1 hid syear using $mydata/user/is_regionl
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge
merge m:1 hid syear using $mydata/user/is_kkz_rek
soepkeep if _merge == 1 | _merge == 3 
soepdrop _merge

save $mydata/user/soep_is_merged, replace 

*}

* End of Code
