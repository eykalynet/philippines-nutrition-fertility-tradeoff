# ==============================================================================
# Script Name : 04_build_dhs_under_5_no_geo.R
# Purpose     : Harmonize child-level KR DHS datasets across survey years
#               into a unified under-5 panel without geographic metadata
# Years       : 1993, 1998, 2003, 2008, 2013
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads raw DHS KR (child recode) files for each survey year
#     - Applies a crosswalk to standardize variable names across years
#     - Selects and renames variables consistently across years
#     - Appends all harmonized data into one under-5 child panel
#     - Saves the harmonized dataset in both .rds and .dta formats
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/04_build_dhs_under_5_no_geo.R")

# 1. Load Libraries ============================================================
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(here)

# 2. Define File Paths ========================================================
raw_path <- here("data-analysis", "raw-data", "dhs_raw")
crosswalk_path <- here(raw_path, "var_crosswalk.csv")

# 3. Load and Clean Crosswalk =================================================
crosswalk <- read_csv(crosswalk_path)
colnames(crosswalk) <- tolower(colnames(crosswalk))
crosswalk$variable <- tolower(crosswalk$variable)

crosswalk <- crosswalk %>%
  mutate(across(-variable, ~ tolower(trimws(.))))

# 4. Define Harmonization Function ============================================
harmonize_kr <- function(year) {
  # Load KR data
  kr_file <- file.path(raw_path, paste0("dhs_", year, "_kr_children.dta"))
  kr <- read_dta(kr_file)
  kr_vars <- tolower(names(kr))
  
  # Get relevant variables for this year
  raw_vars <- tolower(crosswalk[[as.character(year)]])
  std_names <- tolower(crosswalk$variable)
  
  # Filter out NAs and unavailable vars
  valid_idx <- !is.na(raw_vars) & raw_vars %in% kr_vars
  raw_vars <- raw_vars[valid_idx]
  std_names <- std_names[valid_idx]
  
  # Harmonize
  names(raw_vars) <- std_names
  kr_selected <- kr %>%
    select(all_of(raw_vars)) %>%
    rename_with(~ names(raw_vars), .cols = everything()) %>%
    mutate(survey_year = year)
  
  return(kr_selected)
}

# 5. Run Harmonization for All Years ==========================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
harmonized_list <- map(survey_years, harmonize_kr)
kr_all <- bind_rows(harmonized_list)

# 6. Save Harmonized Dataset ==================================================
write_rds(kr_all, here("data-analysis", "clean-data", "dhs_kr_children_harmonized.rds"))
write_dta(kr_all, here("data-analysis", "clean-data", "dhs_kr_children_harmonized.dta"))
