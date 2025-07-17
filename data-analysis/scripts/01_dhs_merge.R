# ==============================================================================
# dhs_merge_under5_panel.R -
# Purpose = Build a unified child-level panel (under age 5) from Philippine DHS:
#           combines 1993–2013 KR, IR, and HR files; computes health indicators;
#           adds flags for birth risk, maternal profile, immunization, and illness.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads KR (child), IR (maternal), and HR (household) files for each DHS round.
#   2) Keeps only relevant variables for fertility, health, and demographics.
#   3) Merges maternal and household data.
#   4) Adds custom flags (e.g., parity, birthweight, illness episodes, maternal BMI).
#   5) Adds cluster ID, household ID, and normalized survey weights.
#   6) Saves final merged panel as RDS.
# ==============================================================================

# 1. Load libraries ============================================================
library(haven)     # read .dta files
library(dplyr)     # data wrangling
library(purrr)     # for map_dfr
library(readr)     # for write_rds

# 2. Define survey years and file paths ========================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Variable shortlists =======================================================
# KR = child recode, IR = individual (mother), HR = household
kr_keep <- c(
  "survey_year", "v001", "v002", "v003", "v005", "bidx", "b4", "b5", "bord", "b9", "b11",
  "m14", "m15", "m3a", "m3b", "m3c", "m3h", "m18", "m19", "h2", "h3", "h4", "h5", "h6", "h7",
  "h8", "h9", "h11", "h22", "h31"
)

ir_keep <- c(
  "survey_year", "v001", "v002", "v003", "v012", "v106", "v133", "v201", "v218", "v212",
  "v130", "v190", "v025", "v024", "v445", "v457", "v701", "v705", "v715", "v717", "v714"
)

hr_keep <- c(
  "survey_year", "hv001", "hv002", "hv009", "hv025", "hv024", "hv270", "hv271", "hv201",
  "hv205", "hv206", "hv225"
)

# 4. Function to load + clean one DHS round ====================================
load_and_clean_year <- function(year) {
  message("Processing ", year, "...")
  
  # File paths for KR, IR, and HR datasets
  kr_file <- file.path(data_path, paste0("dhs_", year, "_kr_children.DTA"))
  ir_file <- file.path(data_path, paste0("dhs_", year, "_ir_individual.DTA"))
  hr_file <- file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))
  
  # Load each dataset and tag with year
  kr <- read_dta(kr_file) |> mutate(survey_year = year)
  ir <- read_dta(ir_file) |> mutate(survey_year = year)
  hr <- read_dta(hr_file) |> mutate(survey_year = year)
  
  # Keep only needed variables for each file
  kr <- kr |> select(any_of(kr_keep))
  ir <- ir |> select(any_of(ir_keep))
  hr <- hr |> select(any_of(hr_keep))
  
  # Merge child with mother and household records
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
    cluster_id   = v001,                                  # DHS cluster number
    household_id = paste(v001, v002, sep = "-"),          # unique household ID
    child_weight = v005 / 1e6                             # normalized sample weight
  )

# 7. Fertility and round flags ==================================================
dhs_all <- dhs_all |>
  mutate(
    parity_3plus         = if_else(v201 >= 3, 1, 0),       # mother had ≥ 3 births
    completed_fertility  = if_else(v012 >= 40, 1, 0),      # mother age ≥ 40
    teen_birth           = if_else(v212 < 20, 1, 0),       # mother age at birth < 20
    high_risk_birth      = if_else(b11 < 24 | v201 >= 4 | v212 < 18, 1, 0),
    short_birth_interval = if_else(b11 < 24, 1, 0),        # < 24 months spacing
    firstborn_flag       = if_else(bord == 1, 1, 0),       # firstborn child
    # Year-round indicators
    round_1993 = if_else(survey_year == 1993, 1, 0),
    round_1998 = if_else(survey_year == 1998, 1, 0),
    round_2003 = if_else(survey_year == 2003, 1, 0),
    round_2008 = if_else(survey_year == 2008, 1, 0),
    round_2013 = if_else(survey_year == 2013, 1, 0)
  )

# 8. Child, maternal, and household health flags ================================
dhs_all <- dhs_all |>
  mutate(
    # Child demographics
    male                 = if_else(b4 == 1, 1, 0),
    infant               = if_else(b9 < 12, 1, 0),
    toddler              = if_else(b9 >= 12 & b9 < 36, 1, 0),
    preschooler          = if_else(b9 >= 36 & b9 < 60, 1, 0),
    
    # Maternal profile
    mother_teen          = if_else(v012 < 20, 1, 0),
    mother_low_education = if_else(v106 %in% c(0, 1), 1, 0),
    mother_high_education= if_else(v106 == 3, 1, 0),
    mother_underweight   = if_else(v445 < 1850, 1, 0),
    mother_overweight    = if_else(v445 >= 2500, 1, 0),
    mother_unemployed    = if_else(v714 == 0, 1, 0),
    
    # Health outcomes: vaccination + illness
    full_immunization    = if_else(h2 == 1 & h3 == 1 & h4 == 1 & h5 == 1 & h6 == 1 & h7 == 1, 1, 0),
    illness_cough        = if_else(h11 == 1, 1, 0),
    illness_fever        = if_else(h31 == 1, 1, 0),
    illness_diarrhea     = if_else(h22 == 1, 1, 0),
    
    # Nutrition proxy
    low_birthweight      = if_else(m19 < 2.5 & !is.na(m19), 1, 0),
    perceived_small      = if_else(m18 %in% c(1, 2), 1, 0),
    
    # Food security and sanitation
    household_hunger     = if_else(hv225 %in% c(2, 3), 1, 0),
    unimproved_water     = if_else(hv201 %in% c(10, 11, 12, 13, 14, 96), 1, 0),
    unimproved_sanitation= if_else(hv205 %in% c(11, 12, 13, 96), 1, 0),
    no_toilet            = if_else(hv205 == 96, 1, 0)
  )

# 9. Save final output ==========================================================
write_rds(dhs_all, "data-analysis/clean-data/dhs_merged_panel.rds")
message("DHS merged panel written to clean-data/")
