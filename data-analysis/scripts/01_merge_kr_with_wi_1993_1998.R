# ==============================================================================
# Script Name : merge_wealth_hr_1993_1998.R
# Purpose     : Merge Wealth Index (WI) data into Household Recode (HR) and 
#               Kids Recode (KR) files for the 1993 and 1998 Philippine DHS
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads HR and WI household-level datasets for 1993 and 1998
#     - Harmonizes household identifiers (hhid/whhid) across files
#     - Merges wealth quintile and wealth factor score into HR
#     - Renames WI variables as v190 (wealth quintile), v191 (wealth score)
#     - Merges HR-WI data into KR files using cluster and household IDs
#     - Backs up original KR files before overwriting
#     - Outputs merged HR and KR datasets in .dta format
#     - Reports merge success and match rates for both HR and KR by year
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/01_merge_kr_with_wi_1993_1998.R")

# 1. Load Required Libraries ===================================================
library(haven)
library(dplyr)
library(readr)
library(stringr)

# 2. Set Survey Years and File Paths ==========================================
survey_years <- c(1993, 1998)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Merge WI into HR ==========================================================
merge_hr_wealth <- function(year) {
  message("\n-----------------------------")
  message("Merging HR with WI for ", year)
  message("-----------------------------")
  
  hr_file <- file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))
  wi_file <- file.path(data_path, paste0("dhs_", year, "_wi_wealth_index.DTA"))
  
  hr <- read_dta(hr_file)
  wi <- read_dta(wi_file)
  
  # Harmonize identifiers and rename WI columns
  if (!("hhid" %in% names(hr))) stop(paste(year, "HR file missing 'hhid'"))
  if (!("whhid" %in% names(wi))) stop(paste(year, "WI file missing 'whhid'"))
  
  hr <- hr |> mutate(hhid = str_trim(hhid))
  wi <- wi |> mutate(whhid = str_trim(whhid)) |> rename(
    hhid  = whhid,
    hv270 = wlthind5,
    hv271 = wlthindf
  )
  
  # Merge and rename wealth columns to v190/v191
  merged <- left_join(hr, wi |> select(hhid, hv270, hv271), by = "hhid") |>
    rename(
      v190 = hv270,
      v191 = hv271
    )
  
  # Diagnostic
  unmatched_n <- sum(is.na(merged$v190))
  total_n <- nrow(merged)
  matched_n <- total_n - unmatched_n
  message("Total households: ", total_n)
  message("Matched households: ", matched_n)
  message("Unmatched households: ", unmatched_n,
          " (", round(100 * unmatched_n / total_n, 1), "%)")
  
  # Save
  out_file <- file.path(data_path, paste0("dhs_", year, "_hr_household_with_WI.dta"))
  write_dta(merged, out_file)
  message("Saved HR-WI file: ", out_file)
}

# 4. Merge HR-WI into KR =======================================================
merge_kr_with_wealth <- function(year) {
  message("\n-----------------------------")
  message("Merging KR with WI for ", year)
  message("-----------------------------")
  
  kr_file <- file.path(data_path, paste0("dhs_", year, "_kr_children.dta"))
  hr_wi_file <- file.path(data_path, paste0("dhs_", year, "_hr_household_with_WI.dta"))
  
  # Step 1: Backup original KR file if not already backed up
  kr_backup_file <- file.path(data_path, paste0("dhs_", year, "_kr_children_old.dta"))
  if (!file.exists(kr_backup_file)) {
    file.rename(kr_file, kr_backup_file)
    message("Backed up original KR file to: ", kr_backup_file)
  } else {
    message("Backup already exists: ", kr_backup_file)
  }
  
  # Step 2: Load KR and HR-WI data
  kr <- read_dta(kr_backup_file)
  hr_wi <- read_dta(hr_wi_file)
  
  # Step 3: Drop existing v190/v191 if present
  kr <- kr %>% select(-any_of(c("v190", "v191")))
  
  # Step 4: Merge on v001/v002 ≡ hv001/hv002
  kr_wi <- left_join(
    kr,
    hr_wi %>% select(hv001, hv002, v190, v191),
    by = c("v001" = "hv001", "v002" = "hv002")
  )
  
  # Step 5: Diagnostic and save
  message("Matched children: ", sum(!is.na(kr_wi$v190)), "/", nrow(kr_wi))
  
  write_dta(kr_wi, kr_file)
  message("Saved KR file with WI: ", kr_file)
}

# 5. Run All Merges ============================================================
for (yr in survey_years) {
  merge_hr_wealth(yr)
  merge_kr_with_wealth(yr)
}
