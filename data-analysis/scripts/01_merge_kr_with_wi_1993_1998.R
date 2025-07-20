# ==============================================================================
# merge_wealth_hr_1993_1998.R
# Purpose: Merge Wealth Index (WI) data into HR (Household Recode) files
#          for the 1993 and 1998 Philippine DHS.
# Author : Erika Salvador '28 (esalvador28@amherst.edu)
# Project: Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
# In plain English, this script:
# - Loads HR and WI household-level files for 1993 and 1998
# - Harmonizes household identifiers (hhid/whhid)
# - Merges wealth quintile (hv270) and factor score (hv271) into HR
# - Saves merged datasets and reports match rates
# ==============================================================================

# 1. Load Required Libraries ===================================================
library(haven)     # For loading .DTA files
library(dplyr)     # For data wrangling
library(readr)     # For writing output
library(stringr)   # For trimming whitespace from IDs

# 2. Set Survey Years and File Paths ===========================================
survey_years <- c(1993, 1998)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Define Merge Function =====================================================
merge_hr_wealth <- function(year) {
  message("\n-----------------------------")
  message("Merging HR with WI for ", year)
  message("-----------------------------")
  
  # Load raw HR and WI files
  hr_file <- file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))
  wi_file <- file.path(data_path, paste0("dhs_", year, "_wi_wealth_index.DTA"))
  hr <- read_dta(hr_file)
  wi <- read_dta(wi_file)
  
  # Harmonize identifiers and rename WI variables
  if (year == 1993) {
    if (!("hhid" %in% names(hr))) stop("1993 HR file missing 'hhid'")
    if (!("whhid" %in% names(wi))) stop("1993 WI file missing 'whhid'")
    
    hr <- hr |> mutate(hhid = str_trim(hhid))
    wi <- wi |> mutate(whhid = str_trim(whhid)) |> rename(
      hhid  = whhid,
      hv270 = wlthind5,
      hv271 = wlthindf
    )
    
  } else if (year == 1998) {
    if (!("hhid" %in% names(hr))) stop("1998 HR file missing 'hhid'")
    if (!("whhid" %in% names(wi))) stop("1998 WI file missing 'whhid'")
    
    hr <- hr |> mutate(hhid = str_trim(hhid))
    wi <- wi |> mutate(whhid = str_trim(whhid)) |> rename(
      hhid  = whhid,
      hv270 = wlthind5,
      hv271 = wlthindf
    )
    
  } else {
    stop("This script only supports 1993 and 1998.")
  }
  
  # Merge HR with WI
  merged <- left_join(hr, wi |> select(hhid, hv270, hv271), by = "hhid")
  
  # Diagnostic summary
  unmatched_n <- sum(is.na(merged$hv270))
  total_n <- nrow(merged)
  matched_n <- total_n - unmatched_n
  message("Total households: ", total_n)
  message("Matched households: ", matched_n)
  message("Unmatched households: ", unmatched_n, 
          " (", round(100 * unmatched_n / total_n, 1), "%)")
  
  # Save merged file
  out_file <- file.path(data_path, paste0("dhs_", year, "_hr_household_with_WI.dta"))
  write_dta(merged, out_file)
  message("Saved: ", out_file)
}

# 4. Run Merge for All Years ===================================================
for (yr in survey_years) {
  merge_hr_wealth(yr)
}
