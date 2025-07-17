# ==============================================================================
# dhs_merge_under5_panel.R -
# Purpose = Build a unified child-level panel (under age 5) from Philippine DHS:
#           combines 1993–2013 KR, IR, and HR files; computes health indicators;
#           adds flags for birth risk, maternal profile, immunization, illness;
#           and handles missing data with imputation and IP weighting.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads KR (child), IR (maternal), and HR (household) files for each DHS round.
#   2) Keeps only relevant variables for fertility, health, and demographics.
#   3) Merges maternal and household data.
#   4) Adds custom flags (e.g., parity, birthweight, illness episodes, WASH).
#   5) Applies missing-indicator method for partial covariates.
#   6) Adds cluster ID, household ID, and normalized survey weights.
#   7) Saves final merged panel as RDS.
#   8) Prints variable-level missingness summary for audit.
#   9) Flags partial-response variables for robustness tracking.
#  10) Robustness: multiple imputation & inverse-probability weights (IPW).
#  11) Exports adjusted datasets for robustness checks.
# ==============================================================================

# 1. Load libraries ============================================================
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(mice)         # for multiple imputation
library(survey)       # for complex survey design + IPW

# 2. Define survey years and file paths ========================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Define variable shortlists for each file type =============================
kr_keep <- c(
  "survey_year", "v001", "v002", "v003", "v005", "bidx", "b4", "b5", "bord", "b9", "b11",
  "m14", "m15", "m18", "m19", "m3a", "m3b", "m3c", "m3h",
  "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h11", "h22", "h31"
)

ir_keep <- c(
  "survey_year", "v001", "v002", "v003", "v012", "v106", "v133", "v201", "v218", "v212",
  "v130", "v190", "v025", "v024", "v701", "v705", "v715", "v717", "v714"
)

hr_keep <- c(
  "survey_year", "hv001", "hv002", "hv009", "hv025", "hv024", "hv270", "hv271",
  "hv201", "hv205", "hv206", "hv225"
)

# 4. Function to load and clean one DHS round ==================================
load_and_clean_year <- function(year) {
  message("Processing ", year, "...")
  
  kr <- read_dta(file.path(data_path, paste0("dhs_", year, "_kr_children.DTA"))) |>
    mutate(survey_year = year) |> select(any_of(kr_keep))
  
  ir <- read_dta(file.path(data_path, paste0("dhs_", year, "_ir_individual.DTA"))) |>
    mutate(survey_year = year) |> select(any_of(ir_keep))
  
  hr <- read_dta(file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))) |>
    mutate(survey_year = year) |> select(any_of(hr_keep))
  
  merged <- kr |>
    left_join(ir, by = c("survey_year", "v001", "v002", "v003")) |>
    left_join(hr, by = c("survey_year", "v001" = "hv001", "v002" = "hv002"))
  
  return(merged)
}

# 5. Merge all years into one panel ============================================
dhs_all <- map_dfr(survey_years, load_and_clean_year)

# 6. Add IDs and normalized weights ============================================
dhs_all <- dhs_all |>
  mutate(
    cluster_id   = v001,
    household_id = paste(v001, v002, sep = "-"),
    child_weight = v005 / 1e6
  )

# 7. Generate health and demographic flags =====================================
dhs_all <- dhs_all |>
  mutate(
    # Fertility
    parity_3plus        = if_else(v201 >= 3, 1, 0),
    completed_fertility = if_else(v012 >= 40, 1, 0),
    teen_birth          = if_else(v212 < 20, 1, 0),
    short_birth_interval= if_else(b11 < 24, 1, 0),
    firstborn_flag      = if_else(bord == 1, 1, 0),
    
    # Health outcomes
    low_birthweight     = if_else(m19 < 2500, 1, 0),
    perceived_small     = if_else(m18 == 1, 1, 0),
    
    illness_diarrhea    = if_else(h11 == 1, 1, 0),
    illness_fever       = if_else(h22 == 1, 1, 0),
    illness_cough       = if_else(h31 == 1, 1, 0),
    
    full_immunization   = if_else(h2 == 1 & h3 == 1 & h4 == 1 & h5 == 1 &
                                    h6 == 1 & h7 == 1, 1, 0),
    
    # Maternal & household flags
    mother_low_education = if_else(v106 %in% c(0, 1), 1, 0),
    mother_high_education= if_else(v106 == 3, 1, 0),
    mother_teen          = if_else(v012 < 20, 1, 0),
    mother_unemployed    = if_else(v714 == 0, 1, 0),
    
    household_hunger     = if_else(hv225 %in% c(2, 3), 1, 0),
    unimproved_water     = if_else(hv201 %in% c(11, 12, 13, 14, 95), 1, 0),
    unimproved_sanitation= if_else(hv205 %in% c(11, 12, 13, 14, 95), 1, 0),
    no_toilet            = if_else(hv205 == 95, 1, 0),
    
    # Round-specific dummies
    round_1993 = if_else(survey_year == 1993, 1, 0),
    round_1998 = if_else(survey_year == 1998, 1, 0),
    round_2003 = if_else(survey_year == 2003, 1, 0),
    round_2008 = if_else(survey_year == 2008, 1, 0),
    round_2013 = if_else(survey_year == 2013, 1, 0)
  )

# 8. Audit: Print % missing for each variable ==================================
audit_missing <- dhs_all |>
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "pct_missing") |>
  arrange(desc(pct_missing))

readr::write_csv(audit_missing, "data-analysis/outputs/missingness_audit.csv")

# 9. Create missing-indicator flags for partial covariates =====================
dhs_all <- dhs_all |>
  mutate(
    m18_missing   = is.na(m18),
    m19_missing   = is.na(m19),
    hv225_missing = is.na(hv225),
    hv270_missing = is.na(hv270),
    hv271_missing = is.na(hv271),
    v133_missing  = is.na(v133),
    v190_missing  = is.na(v190)
  )

# 10. Robustness: Multiple Imputation & IP Weights ==============================

# Select variables and convert labelled → numeric if needed
impute_vars <- dhs_all |> 
  select(v190, v133, hv225, hv270, hv271, m18, m19) |>
  mutate(
    across(
      everything(),
      ~ if (inherits(., "haven_labelled")) as.numeric(as_factor(.)) else .
    )
  )

# Run multiple imputation using mice (5 imputed datasets)
mice_imputations <- mice(impute_vars, m = 5, seed = 123)

# Extract the first complete imputed dataset
imputed_data1 <- complete(mice_imputations, 1)

# Merge imputed values back into main dataset
dhs_all_imputed <- dhs_all |>
  select(-c(v190, v133, hv225, hv270, hv271, m18, m19)) |>
  bind_cols(imputed_data1)

# Create complex survey design object
svy_design <- svydesign(
  id = ~cluster_id,
  weights = ~child_weight,
  data = dhs_all,
  nest = TRUE
)

# Fit model predicting missingness of full_immunization (on complete cases)
ipw_data <- dhs_all |>
  select(full_immunization, v190, hv225, hv270) |>
  mutate(
    across(
      everything(),
      ~ if (inherits(., "haven_labelled")) as.numeric(as_factor(.)) else .
    )
  ) |>
  drop_na(v190, hv225, hv270)

ipw_model <- glm(!is.na(full_immunization) ~ v190 + hv225 + hv270, 
                 family = binomial, data = ipw_data)

# Safely assign IP weights back to dhs_all
dhs_all$ip_weight <- NA
model_rows <- complete.cases(dhs_all$v190, dhs_all$hv225, dhs_all$hv270)
dhs_all$ip_weight[model_rows] <- 1 / fitted(ipw_model)

# 11. Save all versions of dataset =============================================

# Save original merged panel (without imputation/IPW)
write_rds(dhs_all, "data-analysis/clean-data/dhs_merged_panel.rds")
# Save imputed version
write_rds(dhs_all_imputed, "data-analysis/clean-data/dhs_imputed_panel.rds")
# Save IP-weighted version (same as dhs_all but with added ip_weight column)
write_rds(dhs_all, "data-analysis/clean-data/dhs_panel_with_ipw.rds")
message("All datasets saved: merged, imputed, and IPW versions.")
