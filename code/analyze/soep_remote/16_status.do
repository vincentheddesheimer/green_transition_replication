** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/merged_brown_soep_is, clear

local outcomes iss1 iss2

rename brown0_3_share_2015 treat

* Standardize status variables
// Sort the data by syear
sort syear

// Calculate means and standard deviations for isei08 and 
// isei88 within each syear group
by syear: egen iss1_mean = mean(iss1)
by syear: egen iss1_sd = sd(iss1)
by syear: egen iss2_mean = mean(iss2)
by syear: egen iss2_sd = sd(iss2)

// Standardize isei08 and isei88 within each syear group
replace iss1 = (iss1 - iss1_mean) / iss1_sd
replace iss2 = (iss2 - iss2_mean) / iss2_sd

soepkeep if syear == 2016

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat
	}
	
	di "Outcome variable: `var'_2016_bivariate"
    estout, cells("b se") style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex i.bula
	}
	
	di "Outcome variable: `var'_2016_basecov"
    estout, cells("b se") style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex ///
		isei_combined monthly_income i.bula
	}
	
	di "Outcome variable: `var'_2016_addcov"
    estout, cells("b se") style(tex)
}


* Same for 2018

use $mydata/user/merged_brown_soep_is, clear

local outcomes iss1 iss2

rename brown0_3_share_2015 treat

* Standardize status variables
// Sort the data by syear
sort syear

// Calculate means and standard deviations for isei08 and 
// isei88 within each syear group
by syear: egen iss1_mean = mean(iss1)
by syear: egen iss1_sd = sd(iss1)
by syear: egen iss2_mean = mean(iss2)
by syear: egen iss2_sd = sd(iss2)

// Standardize isei08 and isei88 within each syear group
replace iss1 = (iss1 - iss1_mean) / iss1_sd
replace iss2 = (iss2 - iss2_mean) / iss2_sd

soepkeep if syear == 2018

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat
	}
	
	di "Outcome variable: `var'_2018_bivariate"
    estout, cells("b se") style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex i.bula
	}
	
	di "Outcome variable: `var'_2018_basecov"
    estout, cells("b se") style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex ///
		isei_combined monthly_income i.bula
	}
	
	di "Outcome variable: `var'_2018_addcov"
    estout, cells("b se") style(tex)
}


