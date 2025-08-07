# ===============================================================
# ECONOMETRIC ANALYSIS: EO 003 Impact on Child Outcomes
# Dataset: dhs_kr_children_with_geo.rds
# Author: [Your Name]
# ===============================================================

# 1. Load Required Libraries ------------------------------------
library(tidyverse)
library(fixest)
library(ggplot2)
library(broom)
library(MatchIt)
library(quantreg)
library(AER)

# 2. Load Data --------------------------------------------------
dhs_data <- readRDS("Data-analysis/clean-data/dhs_kr_children_with_geo.rds")

# 3. Create Treatment Variables ---------------------------------
dhs_data <- dhs_data %>%
  mutate(
    # Numeric birth date (YYYYMM)
    birth_ym = birth_year * 100 + birth_month,
    
    # EO003 implemented Jan 2001: 1 = post-policy
    post_policy = as.integer(birth_ym >= 200101),
    
    # Accurate Manila assignment using cleaned geo
    manila = as.integer(
      (str_to_lower(province_name) == "metro manila" & str_to_lower(municipality_name) == "manila") |
        (survey_year == 2003 & cluster_id %in% c(564:581, 643)) |
        (survey_year == 2008 & cluster_id == 8)
    ),
    
    # For event study
    birth_year_relative_to_policy = birth_year - 2000
  )

# Optional: Relevel child_sex
dhs_data$child_sex <- factor(dhs_data$child_sex, levels = c("male", "female"))

# 4. Estimate Event Study ---------------------------------------
event_model <- feols(
  birth_weight ~ i(birth_year_relative_to_policy, ref = -1) + manila +
    wealth_index + mother_educ_level,
  data = dhs_data
)

# 5. Plot Event Study Coefficients ------------------------------
event_plot_data <- tidy(event_model) %>%
  filter(grepl("birth_year_relative_to_policy::", term)) %>%
  mutate(year = as.numeric(gsub("birth_year_relative_to_policy::", "", term)))

ggplot(event_plot_data, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Event Study: Effect of EO 003 on Birth Weight",
       x = "Years Since Policy (2000)", y = "Estimate (vs. -1)") +
  theme_minimal()

# 6. Estimate Heterogeneous Effects by Wealth Quintile ----------
dhs_data <- dhs_data %>%
  mutate(wealth_quintile = ntile(wealth_index, 5))

het_model <- feols(
  birth_weight ~ post_policy * manila * factor(wealth_quintile) +
    mother_educ_level + birth_order,
  data = dhs_data
)

# 7. Plot Heterogeneous Effects ---------------------------------
het_plot_data <- tidy(het_model) %>%
  filter(str_detect(term, "post_policy:manila:factor\\(wealth_quintile\\)")) %>%
  mutate(quintile = as.numeric(str_extract(term, "\\d")))

ggplot(het_plot_data, aes(x = quintile, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.3) +
  labs(title = "Heterogeneous Effects of EO 003 by Wealth Quintile",
       x = "Wealth Quintile", y = "Estimated Effect") +
  theme_minimal()
