# ==============================================================================
# dhs_merge_under5_panel.R -
# Purpose = Build a unified child-level panel (under age 5) from Philippine DHS:
#           combines 1993–2013 KR, IR, HR, and WI files; computes health indicators;
#           adds flags for birth risk, maternal profile, immunization, illness;
#           merges wealth index via WHHID; handles missing data with imputation/IPW.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu)
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads and selects KR, IR, HR, and WI files for each DHS year (1993–2013).
#   2) Renames and harmonizes key variables across rounds, including wealth index.
#   3) Constructs new indicators related to fertility, birth risk, immunization, WASH.
#   4) Applies missingness tagging, multiple imputation, and inverse probability weighting.
#   5) Outputs final cleaned, imputed, and IP-weighted panel datasets.
# ==============================================================================

# 1. Load Required Libraries ===================================================
library(haven)     # For loading .DTA files
library(dplyr)     # For data manipulation
library(purrr)     # For mapping over years
library(readr)     # For saving outputs
library(tidyr)     # For reshaping data
library(stringr)   # For WHHID string formatting
library(mice)      # For multiple imputation
library(survey)    # For inverse probability weights

options(dplyr.summarise.inform = FALSE)

# 2. Define Survey Years and Paths =============================================
survey_years <- c(1993, 1998, 2003, 2008, 2013)
data_path <- "data-analysis/raw-data/dhs_raw/"

# 3. Define Variable Shortlists ================================================
# Variable groupings and labels included in each shortlist.

kr_keep <- c(
  # identifiers
  "survey_year", "v001", "v002", "v003", "v005", "bidx", "b1", "b2",
  # child sex, survival, birth order, spacing
  "b4", "b5", "bord", "b9", "b11",
  # delivery and birth size
  "m14", "m15", "m18", "m19",
  # vaccination
  "m3a", "m3b", "m3c", "m3h",
  # illness/immunization
  "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h11", "h22", "h31",
  # legacy wealth
  "wlthindf", "wlthind5"
)

ir_keep <- c(
  "survey_year", "v001", "v002", "v003",
  "v012", "v106", "v133", "v201", "v218", "v212",
  "v130", "v190", "v025", "v024",
  "v701", "v705", "v715", "v717", "v714"
)

hr_keep <- c(
  "survey_year", "hv001", "hv002", "hv009",
  "hv025", "hv024", "hv270", "hv271",
  "hv201", "hv205", "hv206", "hv225"
)

# 4. Construct WHHID ===========================================================
construct_whhid <- function(df) {
  df |> mutate(
    v001_str = str_pad(as.character(v001), 4, pad = "0"),
    v002_str = str_pad(as.character(v002), 3, pad = "0"),
    whhid = paste0(v001_str, v002_str)
  )
}

# 5. Load and Clean One DHS Round ==============================================
load_and_clean_year <- function(year) {
  message("Processing ", year, "...")
  
  kr <- read_dta(file.path(data_path, paste0("dhs_", year, "_kr_children.DTA"))) |>
    mutate(survey_year = year) |>
    select(any_of(kr_keep))
  
  if (year > 1998) {
    kr <- kr |> select(-any_of(c("wlthindf", "wlthind5")))
  }
  
  ir <- read_dta(file.path(data_path, paste0("dhs_", year, "_ir_individual.DTA"))) |>
    mutate(survey_year = year) |>
    select(any_of(ir_keep))
  
  hr <- read_dta(file.path(data_path, paste0("dhs_", year, "_hr_household.DTA"))) |>
    mutate(survey_year = year) |>
    select(any_of(hr_keep))
  
  wi_file <- file.path(data_path, paste0("dhs_", year, "_wi_household.DTA"))
  wi <- if (file.exists(wi_file)) read_dta(wi_file) else NULL
  
  if (!is.null(wi) && "whhid" %in% names(wi)) {
    wi <- wi |> mutate(whhid = str_trim(str_remove_all(whhid, '["\']')))
    kr <- kr |> construct_whhid()
    kr <- kr |> left_join(wi, by = "whhid")
  }
  
  if (year == 1993 && "wlthindf" %in% names(kr)) {
    kr <- kr |> rename(wealth_index = wlthindf)
  } else if (year == 1998 && "wlthind5" %in% names(kr)) {
    kr <- kr |> rename(wealth_index = wlthind5)
  } else if (year >= 2003 && "v190" %in% names(ir)) {
    ir <- ir |> rename(wealth_index = v190)
  } else if (year >= 2003 && "hv270" %in% names(hr)) {
    hr <- hr |> rename(wealth_index = hv270)
  }
  
  merged <- kr |>
    left_join(ir, by = c("survey_year", "v001", "v002", "v003")) |>
    left_join(hr, by = c("survey_year", "v001" = "hv001", "v002" = "hv002"))
  
  required_vars <- c("v001", "v002", "v005")
  for (var in required_vars) {
    if (!var %in% names(merged)) {
      merged[[var]] <- NA_integer_
      warning(paste("Missing variable", var, "in year", year, "— filled with NA."))
    }
  }
  
  return(merged)
}

# 6. Merge All Years Into Unified Panel ========================================
# Applies the year-specific loader to each DHS round (1993–2013) and combines them.
dhs_all <- map_dfr(survey_years, load_and_clean_year)

# 7. Add Cluster ID, Household ID, Normalized Weights ==========================
# These identifiers are required for modeling and survey-weighted estimation.
dhs_all <- dhs_all |>
  mutate(
    cluster_id   = v001,
    household_id = paste(v001, v002, sep = "-"),
    child_weight = v005 / 1e6
  )

# 8. Construct Binary Health and Demographic Flags =============================
# These derived variables simplify downstream regression and descriptive analysis.
dhs_all <- dhs_all |>
  mutate(
    # Fertility and birth history
    parity_3plus         = if_else(v201 >= 3, 1, 0),
    completed_fertility  = if_else(v012 >= 40, 1, 0),
    teen_birth           = if_else(v212 < 20, 1, 0),
    short_birth_interval = if_else(b11 < 24, 1, 0),
    firstborn_flag       = if_else(bord == 1, 1, 0),
    
    # Child health and size
    low_birthweight      = if_else(m19 < 2500, 1, 0),
    perceived_small      = if_else(m18 == 1, 1, 0),
    
    # Illness episodes
    illness_diarrhea     = if_else(h11 == 1, 1, 0),
    illness_fever        = if_else(h22 == 1, 1, 0),
    illness_cough        = if_else(h31 == 1, 1, 0),
    
    # Immunization (based on 6 required vaccines)
    full_immunization    = if_else(h2 == 1 & h3 == 1 & h4 == 1 & h5 == 1 & h6 == 1 & h7 == 1, 1, 0),
    
    # Maternal demographics
    mother_low_education  = if_else(v106 %in% c(0, 1), 1, 0),
    mother_high_education = if_else(v106 == 3, 1, 0),
    mother_teen           = if_else(v012 < 20, 1, 0),
    mother_unemployed     = if_else(v714 == 0, 1, 0),
    
    # Household-level WASH indicators
    household_hunger      = if_else(hv225 %in% c(2, 3), 1, 0),
    unimproved_water      = if_else(hv201 %in% c(11, 12, 13, 14, 95), 1, 0),
    unimproved_sanitation = if_else(hv205 %in% c(11, 12, 13, 14, 95), 1, 0),
    no_toilet             = if_else(hv205 == 95, 1, 0)
  )

# 9. Create Missingness Flags ==================================================
# Used in robustness checks (missing-indicator method)
dhs_all <- dhs_all |>
  mutate(
    m18_missing           = is.na(m18),
    m19_missing           = is.na(m19),
    hv225_missing         = is.na(hv225),
    hv270_missing         = is.na(hv270),
    hv271_missing         = is.na(hv271),
    v133_missing          = is.na(v133),
    wealth_index_missing  = is.na(wealth_index)
  )

# 10. Run Multiple Imputation ==================================================
# Impute partially missing covariates using chained equations (5 imputations)

never_impute <- c(
  "survey_year", "v001", "v002", "v003", "bidx", "v005",      # identifiers
  "cluster_id", "household_id", "child_weight",              # constructed vars
  "full_immunization", "ip_weight",                          # outcomes & weights
  grep("_missing$", names(dhs_all), value = TRUE)            # missing flags
)

# Identify partially missing variables (not fully missing, not fully observed)
partial_vars <- dhs_all |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "na_pct") |>
  filter(na_pct > 0, na_pct < 1, !variable %in% never_impute) |>
  pull(variable)

# Extract and convert these variables for imputation
impute_vars <- dhs_all |>
  select(all_of(partial_vars)) |>
  mutate(across(everything(), ~ if (inherits(., "haven_labelled")) as.numeric(as_factor(.)) else .))

# Run chained equations (m = 5 imputations)
mice_imp <- mice(impute_vars, m = 5, seed = 123)

# Select first completed dataset (can also pool later)
imputed_data1 <- complete(mice_imp, 1)

# Merge imputed values back into original dataset
dhs_all_imputed <- dhs_all |>
  select(-all_of(partial_vars)) |>
  bind_cols(imputed_data1)

# 11. Compute Inverse-Probability Weights ======================================
# Used to adjust for non-random missingness in the full_immunization outcome
# Approach: logistic regression on observed covariates to estimate P(Observed)

# Identify rows with complete covariates needed for model
model_rows <- complete.cases(dhs_all$wealth_index, dhs_all$hv225, dhs_all$hv270)

# Fit logistic model only on complete cases
ipw_model <- glm(!is.na(full_immunization) ~ wealth_index + hv225 + hv270,
                 family = binomial,
                 data = dhs_all[model_rows, ])

# Assign weights: 1 / P(observed outcome) to complete rows only
dhs_all$ip_weight <- NA_real_
dhs_all$ip_weight[model_rows] <- 1 / fitted(ipw_model)

# 12. Final Missingness Audit ==================================================
# Summary after imputation to assess improvements
final_missing <- dhs_all_imputed |>
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") |>
  arrange(desc(pct_missing))

write_csv(final_missing, "data-analysis/outputs/final_missingness_after_imputation.csv")

# 13. Save Final Datasets ======================================================
# Includes original, imputed, and IP-weighted versions
write_rds(dhs_all,         "data-analysis/clean-data/dhs_merged_panel.rds")
write_rds(dhs_all_imputed, "data-analysis/clean-data/dhs_imputed_panel.rds")
write_rds(dhs_all,         "data-analysis/clean-data/dhs_panel_with_ipw.rds")

message("All datasets saved: merged, imputed, IPW versions + final audit.")
