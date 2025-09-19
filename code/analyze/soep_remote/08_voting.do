** user = // insert user name
** password = // insert password
** package = STATA
** project = GSOEP

// Load user specific data
use $mydata/user/merged_brown_soep, clear

local outcomes CDUCSU SPD FDP Gruene Linke AfD far_right

rename brown0_3_share_2015 treat

// Loop over dependent variables
foreach var in `outcomes' {
    quietly{
		reghdfe `var' c.treat##ib2015.syear age monthly_income i.isced97 ///
            isei_combined worried_migration, ///
            a(pid bula#syear) vce(cluster county)
		}
	di "Outcome variable: `var'"
    estout, cells("b se") style(tex)
}
