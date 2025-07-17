# ==============================================================================
# dhs_merge_under5_panel.R -
# Purpose = Build a unified child-level panel (under age 5) from Philippine DHS:
#           combines 1993–2013 KR, IR, and HR files; computes nutrition indicators;
#           adds flags for birth risk, maternal profile, weights, and immunization.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads KR (child), IR (maternal), and HR (household) files for each DHS round.
#   2) Keeps only relevant variables for anthropometrics, fertility, and demographics.
#   3) Computes z-scores (HAZ, WAZ, WHZ) and undernutrition flags.
#   4) Merges maternal and household data.
#   5) Adds custom flags (e.g., parity, teen birth, maternal BMI, immunization).
#   6) Adds cluster ID, household ID, and normalized survey weights.
#   7) Saves final merged panel as RDS.
# ==============================================================================

# 1. Load libraries ============================================================
library(haven)     # read .dta files
library(dplyr)     # data wrangling
library(purrr)     # for map_dfr
library(readr)     # for write_rds
library(anthro)    # for computing z-scores

# 2. Define survey years and file paths ========================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Variable shortlists =======================================================
kr_keep <- c(
  "survey_year", "v001", "v002", "v003", "v005", "bidx", "b4", "b5", "bord", "b9", "b11",
  "hw70", "hw71", "hw72", "hw1", "hw2",
  "m14", "m15", "m3a", "m3b", "m3c", "m3h",
  "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h11", "h22", "h31",
  "hw53", "hw56", "hw57"
)

ir_keep <- c(
  "survey_year", "v001", "v002", "v003", "v012", "v106", "v133", "v201", "v218", "v212",
  "v130", "v190", "v025", "v024", "v445", "v457", "v701", "v705", "v715", "v717", "v714"
)

hr_keep <- c(
  "survey_year", "hv001", "hv002", "hv009", "hv025", "hv024", "hv270",
  "hv201", "hv205", "hv206", "hv225"
)

# 4. Function to load + clean one DHS round ====================================
load_and_clean_year <- function(year) {
  message("Processing ", year, "...")
  
  # File paths
  kr_file <- file.path(data_path, paste0("dhs_", year, "_kr_children.DTA"))
  ir_file <- file.path(data_path, paste0("dhs_", year, "_ir_individual.DTA"))
  hr_file <- file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))
  
  # Read data and tag year
  kr <- read_dta(kr_file) |> mutate(survey_year = year)
  ir <- read_dta(ir_file) |> mutate(survey_year = year)
  hr <- read_dta(hr_file) |> mutate(survey_year = year)
  
  # Keep only selected variables
  kr <- kr |> select(any_of(kr_keep))
  ir <- ir |> select(any_of(ir_keep))
  hr <- hr |> select(any_of(hr_keep))
  
  # Anthropometry prep: compute HAZ, WAZ, WHZ
  kr <- kr |>
    mutate(
      age_months = b9,
      sex = b4,
      weight = ifelse(!is.na(hw2), hw2 / 1000, NA),   # convert grams → kg
      height = ifelse(!is.na(hw1), hw1 / 10, NA)      # convert mm → cm
    ) |>
    mutate(
      haz = ifelse(!is.na(sex) & !is.na(age_months) & !is.na(height),
                   getHaz(sex, age_months, height)$zscore, NA),
      waz = ifelse(!is.na(sex) & !is.na(age_months) & !is.na(weight),
                   getWaz(sex, age_months, weight)$zscore, NA),
      whz = ifelse(!is.na(sex) & !is.na(height) & !is.na(weight),
                   getWhz(sex, height, weight)$zscore, NA)
    ) |>
    mutate(
      stunted       = if_else(haz < -2, 1, 0),
      underwt       = if_else(waz < -2, 1, 0),
      wasted        = if_else(whz < -2, 1, 0),
      malnourished  = if_else(stunted == 1 | underwt == 1 | wasted == 1, 1, 0)
    )
  
  # Merge child (KR) with mother (IR) and household (HR) data
  merged <- kr |>
    left_join(ir, by = c("survey_year", "v001", "v002", "v003")) |>
    left_join(hr, by = c("survey_year", "v001" = "hv001", "v002" = "hv002"))
  
  return(merged)
}

# 5. Merge all years into one panel ============================================
dhs_all <- map_dfr(survey_years, load_and_clean_year)

# 6. Add IDs and survey weights ================================================
dhs_all <- dhs_all |>
  mutate(
    cluster_id   = v001,                                 # cluster number
    household_id = paste(v001, v002, sep = "-"),         # unique HH identifier
    child_weight = v005 / 1e6                             # normalize DHS sample weight
  )

# 7. Fertility and round flags ==================================================
dhs_all <- dhs_all |>
  mutate(
    parity_3plus         = if_else(v201 >= 3, 1, 0),
    completed_fertility  = if_else(v012 >= 40, 1, 0),
    teen_birth           = if_else(v212 < 20, 1, 0),
    high_risk_birth      = if_else(b11 < 24 | v201 >= 4 | v212 < 18, 1, 0),
    short_birth_interval = if_else(b11 < 24, 1, 0),
    firstborn_flag       = if_else(bord == 1, 1, 0),
    
    # Round-specific indicators
    round_1993 = if_else(survey_year == 1993, 1, 0),
    round_1998 = if_else(survey_year == 1998, 1, 0),
    round_2003 = if_else(survey_year == 2003, 1, 0),
    round_2008 = if_else(survey_year == 2008, 1, 0),
    round_2013 = if_else(survey_year == 2013, 1, 0)
    
  )

# 8. Child and maternal flags ===================================================
dhs_all <- dhs_all |>
  mutate(
    birth_order_4plus    = if_else(bord >= 4, 1, 0),
    male                 = if_else(b4 == 1, 1, 0),
    infant               = if_else(b9 < 12, 1, 0),
    toddler              = if_else(b9 >= 12 & b9 < 36, 1, 0),
    preschooler          = if_else(b9 >= 36 & b9 < 60, 1, 0),
    
    mother_teen          = if_else(v012 < 20, 1, 0),
    mother_low_education = if_else(v106 %in% c(0, 1), 1, 0),
    mother_high_education= if_else(v106 == 3, 1, 0),
    mother_underweight   = if_else(v445 < 1850, 1, 0),
    mother_overweight    = if_else(v445 >= 2500, 1, 0),
    mother_unemployed    = if_else(v714 == 0, 1, 0),
    
    stunted_severe       = if_else(haz < -3, 1, 0),
    wasted_severe        = if_else(whz < -3, 1, 0),
    underweight_severe   = if_else(waz < -3, 1, 0),
    any_anthro_missing   = if_else(is.na(haz) | is.na(waz) | is.na(whz), 1, 0),
    
    full_immunization = if_else(
      h2 == 1 & h3 == 1 & h4 == 1 & h5 == 1 & h6 == 1 & h7 == 1, 1, 0
    )
  )

# 9. Save final output ==========================================================
write_rds(dhs_all, "data-analysis/clean-data/dhs_merged_panel.rds")
message("DHS merged panel written to clean-data/")
