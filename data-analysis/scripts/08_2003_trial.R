# ==============================================================================
# Script Name : 01_did_setup_2003_manila.R
# Purpose     : Prepares analytic sample from 2003 DHS KR file for DiD analysis
#               comparing Manila City (NCR 1) vs. rest of PH
# Author      : [Your Name]
# Project     : EO003 Policy Evaluation – Birthweight and Vaccine Outcomes
# ==============================================================================

# 1. Load Libraries ============================================================
library(haven)
library(dplyr)

# 2. Load KR Data ==============================================================

kr2003 <- read_dta("../raw-data/dhs_raw/dhs_2003_kr_children.DTA")  # <-- Adjust to correct path

# 3. Construct Key Variables ===================================================

kr2003 <- kr2003 %>%
  mutate(
    # Age in years at survey
    age_years = floor(b19 / 12),
    
    # Year of birth (interview year - age)
    birth_year = v007 - age_years,
    
    # Indicator for children born post-policy
    post2000 = ifelse(birth_year >= 2001, 1, 0),
    
    # Indicator for children born in Manila City (NCR 1 = SHPROV 39)
    manila = ifelse(SHPROV == 39, 1, 0),
    
    # Treatment group: born in Manila and born after 2000
    treat = ifelse(post2000 == 1 & manila == 1, 1, 0)
  )

# 4. Restrict Sample to Treated & Control Cohorts ==============================

kr2003_subset <- kr2003 %>%
  filter(age_years %in% c(0, 1, 4, 5))  # 0–1 = treated; 4–5 = controls

# 5. Birthweight Outcome =======================================================

# Use m19: birthweight in grams
summary(kr2003_subset$m19)

# Optional: Flag low birthweight < 2500g
kr2003_subset <- kr2003_subset %>%
  mutate(
    lbw = ifelse(m19 < 2500 & !is.na(m19), 1, 0)
  )

# 6. Vaccine Outcome ===========================================================

# Construct full immunization indicator (BCG, 3 DPT, Measles)
kr2003_subset <- kr2003_subset %>%
  mutate(
    full_vax = ifelse(
      h2 == 1 & h3 == 1 & h5 == 1 & h7 == 1 & h9 == 1, 1, 0
    )
  )

# 7. Save Output ===============================================================

write_rds(kr2003_subset, "outputs/kr2003_did_manila_sample.rds")

# 8. Summary Diagnostics =======================================================

cat("\nSample size by treatment status:\n")
print(table(kr2003_subset$treat))

cat("\nSample size by birth year and manila status:\n")
print(table(kr2003_subset$birth_year, kr2003_subset$manila))
