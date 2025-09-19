** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/merged_brown_soep, clear

soepkeep if syear == 2015

// Get means by county using collapse
collapse (mean) isei_combined, by(county)

// View summary statistics of the collapsed data
tabstat isei_combined, by(county) statistics(n mean sd min max)
