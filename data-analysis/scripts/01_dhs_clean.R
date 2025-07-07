# ==============================================================================
# 01_dhs_clean.R -
# Purpose : Load every KR, IR, HR DHS file for PH (1993–2013), harmonise, merge,
#           flag treatment/control, and save to clean-data/.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity-Quality Trade-off (nutrition arm)
# ==============================================================================

# Load required libraries
library(haven)        # = for reading .dta files (Stata format)
library(dplyr)        # = for data manipulation (filter, mutate, join, etc.)
library(labelled)     # = for handling variable labels from Stata
library(janitor)      # = for cleaning column names (snake_case)
library(stringr)      # = for string operations
library(purrr)        # = for functional programming (map functions)
library(glue)         # = for formatted strings
library(readr)        # = for writing output files
library(crayon)       # = for colorful messages
library(cli)          # = for pretty status symbols


# Step 1: Set up paths and years =============================================

rounds <- c(1993, 1998, 2003, 2008, 2013)                # = list of survey years
file_dir <- "../raw-data/dhs_raw"                        # = folder with raw files

suffixes <- c(
  kr = "kr_children",                                    # = Kids Recode files
  ir = "ir_individual",                                  # = Individual Recode files
  hr = "hr_household"                                    # = Household Recode files
)

# Function to read and standardize DHS files
read_dhs <- function(year, suf) {
  file.path(file_dir, glue("dhs_{year}_{suf}.DTA")) |>  # = build file path
    read_stata() |>                                     # = read Stata file
    clean_names() |>                                    # = clean column names
    mutate(survey_year = year)                          # = tag with year
}

# Step 2: Load and select variables from each file type ======================

# Children (KR)
kr_vars <- c("survey_year", "v001", "v002", "bidx", "hw70", "hw71",
             "b4", "b5", "b9", "bord", "caseid")

kr_all <- map_dfr(rounds, read_dhs, suf = suffixes["kr"]) |>
  select(any_of(kr_vars))

# Women (IR)
ir_vars <- c("survey_year", "caseid", "v001", "v002", "v003",    # = IDs
             "v012", "v106", "v133",                             # = mother's age, education
             "v190", "v191", "v130", "v131")                     # = wealth, ethnicity

ir_all <- map_dfr(rounds, read_dhs, suf = suffixes["ir"]) |>
  select(any_of(ir_vars))

# Household (HR)
hr_vars <- c("survey_year", "hv001", "hv002",                   # = household IDs
             "hv205", "hv206", "hv213",                         # = water, toilet, electricity
             "hv271", "hv270")                                  # = wealth index

hr_all <- map_dfr(rounds, read_dhs, suf = suffixes["hr"]) |>
  select(any_of(hr_vars)) |>
  rename(v001 = hv001, v002 = hv002)                            # = align IDs for joining

# Step 3: Merge KR + IR + HR ================================================

dat <- kr_all |>
  left_join(ir_all, by = c("survey_year", "caseid", "v001", "v002")) |>
  left_join(hr_all, by = c("survey_year", "v001", "v002"))

# Step 4: Create new variables ==============================================

clean_dat <- dat |>
  mutate(
    haz = ifelse(!is.na(hw70), hw70 / 100, NA_real_),           # = height-for-age z-score
    whz = ifelse(!is.na(hw71), hw71 / 100, NA_real_),           # = weight-for-height z-score
    
    post_policy = ifelse(survey_year >= 2003, 1, 0),            # = post-treatment indicator
    manila_treat = 0                                            # = placeholder (to update later)
  ) |>
  select(
    survey_year, v001, v002, bidx, caseid,
    haz, whz,                                                   # = child nutrition
    b4, bord, b9,                                               # = child demographics
    v012, v106, v133,                                           # = mother: age, education
    v190, v191,                                                 # = household wealth
    hv205, hv206, hv213, hv270, hv271,                          # = housing conditions
    post_policy, manila_treat
  )

# Step 5: Save clean file ===================================================

out_path <- "../clean-data/dhs_merged_clean.rds"
write_rds(clean_dat, out_path, compress = "gz")

message(crayon::green(cli::symbol$tick), " DHS cleaned & saved to: ", out_path)
