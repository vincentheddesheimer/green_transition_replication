# Outputs:
# - Table E.2: Descriptive statistics, comparing respondents in brown jobs with respondents in non-brown jobs in 2015
#
# Note: This replication uses the cleaned SOEP data with brownness scores.
# Users need to obtain access to the original SOEP data themselves.
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, kableExtra)

# Load data
df <- fread("data/final/not_to_be_shared/data_soep_descriptive.csv")

# Create LaTeX table: Table E.2
(table <- kbl(df,
  format = "latex",
  booktabs = T,
  linesep = "",
  digts = 2, col.names = c(
    "Brown job",
    "Variable", "Mean",
    "Observations"
  ),
  title = "Individual-level descriptives",
) %>%
  kable_styling(latex_options = c(
    "hold_position"
  ), full_width = F))

# Save table
write_lines(table, "results/tab_E2.tex")

### END
