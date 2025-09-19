** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/merged_brown_soep_1999, clear

gen unemployed = 0
replace unemployed = 1 if labor_force_status == 6
replace unemployed = . if missing(labor_force_status)

gen employed = 0
replace employed = 1 if labor_force_status == 10 ///
	| labor_force_status == 11 | labor_force_status == 12
replace employed = . if missing(labor_force_status)

rename brown0_3_share_1999 treat

local outcomes particip_politics important_particip_politics self_esteem ///
	freq_angry freq_worried freq_happy satisf_life partisan pol_interest ///
	party_aff_intensity satisf_work satisf_hh_income satisf_ind_income ///
	worried_econ_dev worried_finances worried_environment ///
	worried_climate_change worried_job_security employed

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
		reghdfe `var' c.treat##ib1999.syear age monthly_income i.isced97 ///
            isei_combined, ///
            a(pid bula#syear) vce(cluster county)
			}
			
	di "Outcome variable: `var'"
    estout, cells("b se") style(tex)
}

local outcomes unemployed monthly_income

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
		reghdfe `var' c.treat##ib1999.syear age i.isced97 isei_combined, ///
            a(pid bula#syear) vce(cluster county)
			}
			
	di "Outcome variable: `var'"
    estout, cells("b se") style(tex)
}
