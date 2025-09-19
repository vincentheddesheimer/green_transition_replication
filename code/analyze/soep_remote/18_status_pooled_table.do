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

* Initialize a matrix to store results
eststo clear

// Loop over dependent variables for bivariate regression
foreach var in `outcomes' {
    quietly {
        reg `var' c.treat
        eststo bivariate_`var'
    }
}

// Loop over dependent variables for basecov regression
foreach var in `outcomes' {
    quietly {
        reg `var' c.treat age i.isced migback sex i.bula i.syear
        eststo basecov_`var'
    }
}

// Loop over dependent variables for addcov regression
foreach var in `outcomes' {
    quietly {
        reg `var' c.treat age i.isced migback sex i.bula i.syear ///
            isei_combined monthly_income
        eststo addcov_`var'
    }
}

estout bivariate_iss1 bivariate_iss2 basecov_iss1 basecov_iss2 ///
	addcov_iss1 addcov_iss2, ///
	cells(b(fmt(a3) star) t(fmt(2) par("{ralign @modelwidth:{txt:(}" "{txt:)}}")))
	stats(r2 N, fmt(%9.3f) labels(`"N"'))
	starlevels(* 0.05 ** 0.01 *** 0.001)
 varwidth(12)
 modelwidth(12)
 abbrev
 delimiter(" ")
 smcltags
 prehead(`"{hline @width}"')
 posthead("{hline @width}")
 prefoot("{hline @width}")
 postfoot(`"{hline @width}"' `"t statistics in parentheses"' `"@starlegend"')
 varlabels(, end("" "") nolast)
 mlabels(, depvar)
 numbers
 collabels(none)
 eqlabels(, begin("{hline @width}" "") nofirst)
 interaction(" # ")
 notype
 level(95)
 style(tex)
