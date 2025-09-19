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

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat
	}
	
	di "Outcome variable: `var'_bivariate"
    estout, cells("b se") stats(N r2, fmt(%9.0g %9.3f)) style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex i.bula i.syear
	}
	
	di "Outcome variable: `var'_basecov"
    estout, cells("b se") stats(N r2, fmt(%9.0g %9.3f)) style(tex)
}

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
	reg `var' c.treat age i.isced migback sex ///
		isei_combined monthly_income i.bula i.syear
	}
	
	di "Outcome variable: `var'_addcov"
    estout, cells("b se") stats(N r2, fmt(%9.0g %9.3f)) style(tex)
}

