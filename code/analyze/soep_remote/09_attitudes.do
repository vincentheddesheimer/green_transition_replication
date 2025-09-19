** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/merged_brown_soep, clear

local outcomes particip_politics important_particip_politics self_esteem ///
	freq_angry freq_worried freq_happy satisf_life ///
	partisan pol_interest party_aff_intensity ///
	satisf_work satisf_hh_income ///
	satisf_ind_income worried_econ_dev worried_finances ///
	worried_environment worried_climate_change worried_job_security

rename brown0_3_share_2015 treat

// Loop over dependent variables
foreach var in `outcomes' {
    quietly {
		reghdfe `var' c.treat##ib2015.syear age monthly_income i.isced97 ///
            isei_combined worried_migration, ///
            a(pid bula#syear) vce(cluster county)
			}
			
	di "Outcome variable: `var'"
    estout, cells("b se") style(tex)
}
