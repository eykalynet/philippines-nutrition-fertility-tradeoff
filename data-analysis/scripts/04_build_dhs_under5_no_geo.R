# ==============================================================================
# Script Name : 04_build_dhs_under5_with_geo.R
# Purpose     : Construct a unified child-level panel of under-5 children 
#               from the Philippine DHS with geographic metadata
# Years       : 1993, 1998, 2003, 2008, 2013
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads and harmonizes KR, IR, HR, BR, and WI files across all survey years
#     - Merges household and maternal covariates into child-level records
#     - Integrates GPS-based or municipality-proxy geographic metadata
#     - Filters children under age five and aligns variable formats
#     - Saves final cleaned and merged panel dataset for analysis
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/04_build_dhs_under5_with_geo.R")

# 1. Load Libraries ============================================================
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(naniar)
library(skimr)
library(mice)
library(survey)
library(tidyr)

# 2. Set Paths and Survey Years ===============================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
raw_path     <- here("data-analysis", "raw-data", "dhs_raw")
clean_path   <- here("data-analysis", "clean-data")

# 3. Variable Shortlists =======================================================
kr_keep <- c("v001", "v002", "v003", "v005", "v008",
             "bidx", "b0", "b1", "b2", "b4", "b5", "bord", "b9", "b11",
             "m14", "m15", "m3a", "m3b", "m3c", "m3h",
             "m18", "m19",
             "h2", "h3", "h4", "h5", "h6", "h7", "h11", "h22", "h31",
             "m45a", "m45b", "m45c", "m45d", "m45e", "m45f", "m45g", "m45h",
             "m45i", "m45j", "m45k", "m45l", "m45m", "m45n", "m45o", "m45p",
             "m45q", "m45r", "m45s", "m45t", "m45u", "m45v", "m45w", "m45x")

ir_keep <- c("v001", "v002", "v003", "v012", "v106", "v133",
             "v201", "v218", "v212", "v130", "v025", "v024",
             "v701", "v705", "v714", "caseid")

hr_keep <- c("hv001", "hv002", "hv009", "hv025", "hv024",
             "hv201", "hv204", "hv205", "hv206", "hv225",
             "hv270", "hv271")

br_keep <- c("v001", "v002", "v003", "bidx", "bord", "b0", "b4", "b5", "b11", "b19")

# 4. Load and Process by Year ==================================================
dhs_all <- purrr::map_dfr(survey_years, function(year) {
  message("Processing ", year, "...")
  
  kr <- read_dta(file.path(raw_path, paste0("dhs_", year, "_kr_children.DTA"))) |> select(any_of(kr_keep))
  ir <- read_dta(file.path(raw_path, paste0("dhs_", year, "_ir_individual.DTA"))) |> select(any_of(ir_keep))
  hr_file <- if (year %in% c(1993, 1998)) paste0("dhs_", year, "_hr_household_with_WI.DTA") else paste0("dhs_", year, "_hr_household.DTA")
  hr <- read_dta(file.path(raw_path, hr_file)) |> select(any_of(hr_keep))
  br <- read_dta(file.path(raw_path, paste0("dhs_", year, "_br_birth.DTA"))) |> select(any_of(br_keep))
  
  kr_ir <- left_join(kr, ir, by = c("v001", "v002", "v003"))
  kr_ir_br <- left_join(kr_ir, br, by = c("v001", "v002", "v003", "bidx"))
  hr <- hr |> rename(v001 = hv001, v002 = hv002)
  full_data <- left_join(kr_ir_br, hr, by = c("v001", "v002"))
  
  full_data |> mutate(survey_year = year)
})

# 5. Rename Variables and Remove Redundancy ====================================
dhs_all <- dhs_all |> rename(
  cluster_id         = v001,
  household_id       = v002,
  mother_line        = v003,
  sample_weight      = v005,
  interview_date     = v008,
  child_index        = bidx,
  child_is_twin      = b0.x,
  month_birth        = b1,
  year_birth         = b2,
  child_sex          = b4.x,
  child_alive        = b5.x,
  birth_order        = bord.x,
  child_age_months   = b9,
  birth_interval     = b11.x,
  bcg_vaccine        = m14,
  polio0_vaccine     = m15,
  dpt1               = m3a,
  dpt2               = m3b,
  dpt3               = m3c,
  measles_vaccine    = m3h,
  perceived_size     = m18,
  birth_weight_grams = m19,
  fever              = h2,
  cough              = h3,
  diarrhea           = h4,
  seek_fever         = h5,
  seek_cough         = h6,
  seek_diarrhea      = h7,
  syringe_use        = h11,
  full_immunized     = h22,
  vitA_supplement    = h31,
  mother_age         = v012,
  mother_educ        = v106,
  mother_educyrs     = v133,
  total_children     = v201,
  living_children    = v218,
  age_first_birth    = v212,
  ethnicity          = v130,
  urban_rural        = v025,
  region             = v024,
  health_decision    = v701,
  purchase_decision  = v705,
  mother_working     = v714,
  household_size     = hv009,
  water_source       = hv201,
  water_time_min     = hv204,
  toilet_type        = hv205,
  toilet_shared      = hv206,
  toilet_shared_bin  = hv225,
  wealth_index_cat   = hv270,
  wealth_index_score = hv271
)

dhs_all <- dhs_all |> 
  select(-matches("\\.y$"), -caseid, -hv024, -hv025)

# 6. Save Final Panel without Geographic Data ==================================
write_dta(dhs_all, file.path(clean_path, "dhs_under5_panel_no_geo.dta"))
write_rds(dhs_all, file.path(clean_path, "dhs_under5_panel_no_geo.rds"))