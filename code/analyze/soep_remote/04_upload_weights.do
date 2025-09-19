** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP


/* 
This script uploads the isco correspondence table for isco88 and isco08 codes into SOEP remote.

We save these dataframes in the intermediate files folder.
*/

quietly{
	
clear

********************************************
*** 1. Upload isco08 brownness dataframe ***
********************************************

// use weights calculated by
// code/soep/03_soep_generate_weights.R

input double pid syear weights
... // insert weights 
end

}

* List duplicates based on pid and syear
duplicates list pid syear

*************************
*** 2. Save dataframe ***
*************************

save $mydata/user/brownjob_weights, replace 


* End of Code
