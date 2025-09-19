# =============================================================================
# Outputs:
# - Table E.1 showing most frequent high-emissions jobs in the 2015 SOEP
#
# Note: This replication uses the cleaned SOEP data with brownness scores.
# Users need to obtain access to the original SOEP data themselves.
# =============================================================================

# The raw SOEP data files are not publicly available.
# Refer to the README.md file for details on how to access the data.

rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, data.table, kableExtra, readxl)

# Load data
# isco
isco <- read_xls("data/raw/occupations/corrtab08-88.xls")
df <- fread("data/final/not_to_be_shared/data_soep.csv")

# what are most common brown occupations?
df <- df |>
  filter(brown_dummy == 1 & syear == 2015) |>
  group_by(isco08) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  head(10) |>
  rename(`ISCO08 Code` = isco08) |>
  left_join(
    isco |>
      select(
        `ISCO08 Title` = `ISCO-08 Title EN`,
        `ISCO08 Code` = `ISCO 08 Code`
      ) |>
      mutate(`ISCO08 Code` = as.numeric(`ISCO08 Code`)),
    by = "ISCO08 Code"
  ) |>
  distinct() |>
  rename() |>
  select(`ISCO08 Code`, `ISCO08 Title`, N = n)

# Create LaTeX table: Table E.1
(table <- kbl(
  df,
  linesep = "",
  booktabs = TRUE,
  format = "latex",
  longtable = TRUE,
  caption = "Most frequent high-emissions jobs in the 2015 SOEP",
  label = "brown_jobs_soep_2015",
  position = "h",
  escape = FALSE
) |>
  kable_paper(full_width = FALSE) |>
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 9
  ) |>
  footnote(
    general = "The table shows the most frequent high-emissions jobs in the 2015 SOEP.",
    general_title = "Notes: ",
    alphabet_title = "Type II: ",
    footnote_as_chunk = TRUE,
    title_format = c("italic")
  )
)

# Save
write_lines(table, "results/tab_E1.tex")

### END
