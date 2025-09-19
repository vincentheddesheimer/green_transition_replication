** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP


/* 
This script prepares the SOEP remote dataframe with the external county-year level data based on county identifiers.

We save the resulting dataframe in the intermediate files folder.
*/



********************
*** 1. Load SOEP ***
********************

* Run the code quietly: no output needed
quietly {

* Rooting file
use pid piyear syear cid hid phrf sampreg sex gebjahr partner germborn corigin migback using $soep38/ppathl, clear
// Keep people who completed a questionnaire after 1989
soepkeep if syear>=1990
// Save
save $mydata/user/ppathl, replace 

* Prepare dataset pl
use hid syear pid syear iyear pmonin ptagin plh0004 plh0007 plh0011_h plh0012_h plh0013_h plh0333 pli0097_h plh0263_h plh0111 plh0173 plh0175 plh0176 plh0206i11 plh0032 plh0033 plh0036 plh0037 plh0042 plj0046 plb0433_v2 plb0438_v2 plc0013_h plb0113 plb0114 plb0115 plb0116 plb0117 plb0124 plb0125 plb0126 plb0127 plb0128 plb0129 plb0176_h ple0027 ple0028 ple0029 ple0033 plh0184 plh0185 plh0186 plh0187 plh0334 plh0182 using $soep38/pl, clear
save $mydata/user/pl, replace 

* Prepare dataset pgen
use pid syear pglfs pglabgro pglabnet pgstib pgjobch pgjobend pgisco88 pgisco08 pgisced97 pgisced11 pgisei08 pgisei88 pgkldb92 pgnace pgnace2 pgerwzeit pgpsbil using $soep38/pgen, clear 
save $mydata/user/pgen, replace 

* Prepare dataset regionl
use hid syear kkz_rek bula using $soep38/regionl, clear 
save $mydata/user/regionl, replace 


*** Merge all datasets ***
use $mydata/user/ppathl
merge 1:1 pid syear using $mydata/user/biol, keep(match master) nogenerate
merge m:1 pid syear using $mydata/user/pl, keep(match master) nogenerate
merge m:1 pid syear using $mydata/user/pgen, keep(match master) nogenerate
merge m:1 hid syear using $mydata/user/regionl, keep(match master) nogenerate

save $mydata/user/soepmerged, replace 

}

* End of Code
