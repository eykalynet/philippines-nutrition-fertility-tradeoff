# ==============================================================================
# 01_dhs_merge.R -
# Purpose = Build an under-five child-level analytic panel from Philippine DHS
#           rounds 1993-2013.  Steps: read KR / IR / HR, keep core variables,
#           merge, derive nutrition flags, save cleaned datasets.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity-Quality Trade-off 
#           (nutrition arm)
#
# In plain English this script:
#   1) Reads the raw child (KR), mother (IR), and household (HR)
#      files for each survey year.
#   2) Keeps only the variables we actually need.
#   3) Adds prefixes (???m_???, ???h_???) so mother- and household-level
#      columns cannot collide with child-level names when merged.
#   4) Merges the three files so that every row is a child.
#   5) Stacks (appends) all years together.
#   6) Saves the result as both .rds (R-native) and .csv (universal).
# ==============================================================================

# Load libraries
# install.packages(c("haven","dplyr","purrr","readr"), dependencies = TRUE)
library(haven)   # read_dta()
library(dplyr)   # mutate(), select(), left_join(), etc.
library(purrr)   # map_dfr() for looping
library(readr)   # write_rds(), write_csv()

# 1. Folders ===================================================================
raw_path  <- "../raw-data/dhs_raw"  # where PHKR1993FL.DTA etc. live
save_path <- "../clean-data"        # where clean files will be written

# 2. Survey waves to process ===================================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)

# 3. Short-lists of columns to retain ==========================================
# Why short-list? Shrinking the file up-front speeds everything up,
# lowers memory use, and avoids accidentally analyzing the wrong variable.

kr_keep <- c(                                   # CHILD file
  "survey_year", "v001", "v002", "v003", "bidx",          # IDs
  "b4", "b5", "bord", "b9",                               # sex, survival, birth order, age (mos.)
  "hw70", "hw71", "hw72",                                 # HAZ, WAZ, WHZ ??100
  "m14", "m15", "m3a", "m3b", "m3c", "m3h",               # ANC & delivery
  "h2","h3","h4","h5","h6","h7","h8","h9",                # vaccinations
  "h11", "h22", "h31",                                    # illness
  "hw53", "hw56", "hw57"                                  # anaemia (if present)
)

ir_keep <- c(                                   # MOTHER file
  "survey_year", "v001", "v002", "v003",                 # IDs
  "v012", "v106", "v133", "v201", "v218", "v212",        # age, education, fertility
  "v130", "v190", "v025", "v024",                        # ethnicity, wealth, urban, region
  "v445", "v457"                                         # BMI, anaemia (if present)
)

hr_keep <- c(                                   # HOUSEHOLD file
  "survey_year", "hv001", "hv002",                       # IDs
  "hv009", "hv025", "hv024", "hv270",                    # hh size, urban, region, wealth
  "hv201", "hv205", "hv206", "hv225"                     # water, toilet, electricity, shared toilet
)

# 4. Helper function to read + clean one survey wave ===========================
load_and_clean_year <- function(year) {
  
  message("Processing ", year)
  
  # CHILD (KR) 
  kr_file <- file.path(raw_dir, paste0("dhs_", year, "_kr_children.DTA"))
  kr <- read_dta(kr_file) |>
    mutate(survey_year = year) |>
    select(any_of(kr_keep))
  
  # MOTHER (IR) 
  ir_file <- file.path(raw_dir, paste0("dhs_", year, "_ir_individual.DTA"))
  ir <- read_dta(ir_file) |>
    mutate(survey_year = year) |>
    select(any_of(ir_keep)) |>
    rename_with(~ paste0("m_", .x), -c(survey_year, v001, v002, v003))
  # Prefix ???m_??? ensures we don't overwrite shared variable names like v024 (region)
  
  # HOUSEHOLD (HR)
  hr_file <- file.path(raw_dir, paste0("dhs_", year, "_hr_household.DTA"))
  hr <- read_dta(hr_file) |>
    mutate(survey_year = year) |>
    select(any_of(hr_keep)) |>
    rename_with(~ paste0("h_", .x), -c(survey_year, hv001, hv002))
  # Prefix ???h_??? keeps household variables distinct
  
  # Merge: CHILD ??? MOTHER ??? HOUSEHOLD 
  kr_ir <- left_join(kr, ir, by = c("survey_year", "v001", "v002", "v003"))
  full  <- left_join(kr_ir, hr, by = c("survey_year", "v001" = "hv001", "v002" = "hv002"))
  
  return(full)
}

# 5. Loop over all waves =======================================================
dhs_all <- survey_years |>
  map_dfr(load_and_clean_year)

# 6. Save results ==============================================================
write_rds(dhs_all, file.path(save_path, "01_dhs_merged.rds"))
write_csv(dhs_all, file.path(save_path, "01_dhs_merged.csv"))

message("Clean files written to: ", normalizePath(save_path))