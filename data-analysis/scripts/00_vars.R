# ==============================================================================
# 00_vars.R - 
# Purpose  = Create variable catalogs for PH DHS rounds
#            (1993, 1998, 2003, 2008, 2013) – separately for
#            Kids-Recode (KR), Individual-Recode (IR), and
#            Household-Recode (HR) files.
# Author   = Erika Salvador ’28  <esalvador28@amherst.edu>
# Project  = Manila Contraceptive Ban & the Q–Q Trade-off
# ==============================================================================

# Load required libraries 
library(haven)      # read Stata .dta
library(dplyr)      # filter / mutate / summarise
library(purrr)      # map functions
library(stringr)    # string helpers
library(janitor)    # clean_names
library(tidyr)      # unnest / pivot_longer
library(glue)       # formatted paths
library(readr)      # write_csv

# 1. Set up survey rounds & file types =========================================
rounds   = c(1993, 1998, 2003, 2008, 2013)
file_dir = "../raw-data/dhs_raw"         

suffixes = c(
  kr = "kr_children",     # Kids Recode (child level)
  ir = "ir_individual",   # Individual Recode (woman level)
  hr = "hr_household"     # Household Recode (household level)
)

# 2. Define helper function to read data and pull variable meta ================

grab_vars = function(year, suf) {
  
  # Build full file path (`glue` fills in {year} & {suf})
  f_path = glue("{file_dir}/dhs_{year}_{suf}.DTA")
  
  # Haven returns a data-frame-like object with labelled columns
  dat    = read_dta(f_path)
  
  tibble(
    survey_year = year,
    file_type   = names(suffixes)[suffixes == suf],     # kr / ir / hr
    variable    = names(dat),
    type        = map_chr(dat, ~ class(.x)[1])          # extract first class
  )
}

# 3. Build combined meta table =================================================
catalog_all =
  expand_grid(year = rounds, suf = suffixes) |>         # all combos
  mutate(meta = map2(year, suf, grab_vars)) |>          # read each file
  unnest(meta) |>                                       # drop the list column
  select(survey_year, file_type, variable, type) |>     # tidy order
  arrange(file_type, survey_year, variable)

# 4. Split and save to CSV =====================================================
output_dir = "../outputs"      
dir.create(output_dir, showWarnings = FALSE)

catalog_all |> 
  group_by(file_type) |> 
  group_walk(~ write_csv(.x,
                         file = glue("{output_dir}/{.y$file_type}_variable_catalog.csv")))

message("Variable catalogs created in: ", normalizePath(output_dir))
