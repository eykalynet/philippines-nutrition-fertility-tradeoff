# ==============================================================================
# Script Name : 04b_build_dhs_ir_panel.R
# Purpose     : Harmonize woman-level IR DHS datasets across survey years
#               into a unified panel with key fertility and household variables
# Years       : 1993, 1998, 2003, 2008, 2013
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads raw DHS IR (individual recode) files for each survey year
#     - Applies a crosswalk to standardize variable names across years
#     - Selects and renames key fertility, household, and maternal variables
#     - Appends all harmonized data into one unified woman-level panel
#     - Saves the cleaned dataset in both .rds and .dta formats
#     - Output will be used for merging sibship size to child-level KR data
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/04b_build_dhs_ir_panel.R")

# 1. Load Libraries ============================================================
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(here)

# 2. Define File Paths ========================================================
raw_path <- here("data-analysis", "raw-data", "dhs_raw")
output_path <- here("data-analysis", "clean-data")

# 3. Define Years =============================================================
years <- c(1993, 1998, 2003, 2008, 2013)

# 4. Loop Over Files, Count Children < 10 =====================================
ir_list <- list()

for (yr in years) {
  
  file <- file.path(raw_path, paste0("dhs_", yr, "_ir_individual.dta"))
  ir <- read_dta(file)
  
  # Get child age and alive status variables
  age_vars <- paste0("b8_", sprintf("%02d", 1:20))
  alive_vars <- paste0("b5_", sprintf("%02d", 1:20))
  
  # Keep only variables that exist
  age_vars_exist <- age_vars[age_vars %in% names(ir)]
  alive_vars_exist <- alive_vars[alive_vars %in% names(ir)]
  
  # Count children under 10 who are alive
  ir <- ir %>%
    rowwise() %>%
    mutate(children_u10_alive = sum(
      map2_lgl(
        .x = c_across(all_of(alive_vars_exist)),
        .y = c_across(all_of(age_vars_exist)),
        ~ !is.na(.x) && .x == 1 && !is.na(.y) && .y < 10
      )
    )) %>%
    ungroup() %>%
    mutate(survey_year = yr)
  
  ir_list[[as.character(yr)]] <- ir
}

# 5. Merge and Save ===========================================================
merged_ir <- bind_rows(ir_list)

saveRDS(merged_ir, file = file.path(output_path, "merged_ir_1993_2007_children_u10.rds"))