# ==============================================================================
# Script Name : 06_missing_data_imputation.R
# Purpose     : Diagnose and handle missing data in DHS under-5 panel
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   - Flags key variables with missingness for robustness checks
#   - Identifies and excludes non-imputable or problematic variables:
#       * Identifiers, weights, outcomes, geography (non-stochastic)
#       * Fully missing or nearly complete variables
#       * Highly collinear variables (to avoid convergence issues)
#   - Runs parallelized MICE imputation with progress checks
#   - Computes IPW weights for missingness on birth weight
#   - Saves the imputed and weighted datasets for downstream use
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/06_missing_data_imputation.R")

# 1. Load Libraries ============================================================
library(mice)
library(dplyr)
library(here)
library(ggplot2)
library(haven)

# 2. Load DHS Panel with Geographic Data =======================================
dhs_all <- readr::read_rds(here("data-analysis", "clean-data", "dhs_under5_panel_with_geo_final.rds"))

# 3. Flag Missingness for Key Variables ========================================
dhs_all <- dhs_all |>
  mutate(
    birth_weight_missing    = is.na(birth_weight_grams),
    perceived_size_missing  = is.na(perceived_size),
    full_immunized_missing  = is.na(full_immunized),
    wealth_index_missing    = is.na(wealth_index_score),
    mother_educ_missing     = is.na(mother_educ),
    urban_rural_missing     = is.na(urban_rural)
  )

# 4. Variables NEVER to Impute =================================================
never_impute <- c(
  "cluster_id", "household_id", "mother_line", "child_index",
  "sample_weight", 
  "region_code", "province_name", "municipality_name",
  "region_name", "province_source", "municipality_source",
  "birth_weight_missing", "perceived_size_missing", 
  "full_immunized_missing", "wealth_index_missing", 
  "mother_educ_missing", "urban_rural_missing"
)

# 5. Identify Variables with Some Missingness ==================================
impute_candidates <- names(dhs_all)[
  sapply(dhs_all, function(x) mean(is.na(x)) > 0 & mean(is.na(x)) < 1)
]

# 6. Remove Highly Collinear Variables =========================================
# Dropping variables with high pairwise correlation (r > 0.85)
# Justification: Retain one representative variable per domain
collinear_vars <- c(
  # Time variables (near-perfect correlation)
  "interview_date", "survey_year",
  
  # Family structure (redundant with birth_order)
  "total_children", "living_children",
  
  # Education (keep mother_educyrs, drop mother_educ)
  "mother_educ",
  
  # Health symptom overlaps (keep 'fever', drop other highly correlated)
  "cough", "diarrhea", "seek_fever", "seek_cough", "seek_diarrhea",
  
  # Age (keep year_birth, drop 'year')
  "year"
)

# 7. Finalize Variables to Impute ==============================================
impute_vars <- setdiff(impute_candidates, union(never_impute, collinear_vars))

# 8. Subset Dataset for Imputation =============================================
dhs_impute <- dhs_all[, c(impute_vars, never_impute)]

# 9. Clean haven_labelled Variables ============================================
dhs_impute <- dhs_impute |>
  mutate(across(where(haven::is.labelled), ~ haven::as_factor(.)))

# 10. Run Parallel MICE Imputation =============================================
set.seed(123)
imp <- mice(
  dhs_impute,
  m = 5,
  maxit = 10,
  method = "pmm",
  predictorMatrix = quickpred(dhs_impute, exclude = never_impute, mincor = 0.05),
  printFlag = TRUE,
  seed = 123
)

# 11. Save MICE Outputs ========================================================
complete_data <- mice::complete(imp, "long", include = TRUE)
readr::write_rds(imp, here("data-analysis", "clean-data", "dhs_mice_object.rds"))
readr::write_rds(complete_data, here("data-analysis", "clean-data", "dhs_imputed_long.rds"))

# 12. Visual Check: Missingness Pattern ========================================
png(here("data-analysis", "figures", "missing_pattern.png"), width = 800)
mice::md.pattern(dhs_impute)
dev.off()

# 13. Inverse Probability Weighting (IPW) ======================================

# Define model formula for missing birth weight
model_formula <- bw_missing ~ mother_educyrs + urban_rural + wealth_index_score + child_sex + region_code + year_birth

# Identify complete cases for all variables in the model
model_vars <- all.vars(model_formula)
ipw_rows <- complete.cases(dhs_all[, model_vars])  # Ensures logical vector matches nrow(dhs_all)

# Fit logistic regression model to predict probability of missingness
ipw_model <- glm(model_formula, data = dhs_all[ipw_rows, ], family = binomial)

# Predict only on complete rows
predicted_probs <- predict(ipw_model, newdata = dhs_all[ipw_rows, ], type = "response")

# Assign predicted probabilities and IPW weights back to full dataset
dhs_all <- dhs_all |>
  mutate(
    p_missing = NA_real_,
    p_missing = replace(p_missing, ipw_rows, predicted_probs),
    ipw_weight = ifelse(!is.na(p_missing), 1 / (1 - p_missing), NA_real_)
  )

# 14. Save IPW Dataset =========================================================
readr::write_rds(dhs_all, here("data-analysis", "clean-data", "dhs_panel_ipw.rds"))
haven::write_dta(dhs_all, here("data-analysis", "clean-data", "dhs_panel_ipw.dta"))

# 15. Notes ====================================================================
# - Use `dhs_imputed_long` for multiply imputed models using `with(imp, lm(...))`
# - Use `dhs_panel_ipw` for models weighted by inverse probability of missing birth weight
