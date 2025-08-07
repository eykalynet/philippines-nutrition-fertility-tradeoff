# ================================
# Summary Tables (Transposed, 8pt)
# ================================

# 1. Load packages
library(tidyverse)
library(kableExtra)
library(haven)

options(modelsummary_factory_default = "kableExtra")

dhs1 <- readRDS("../clean-data/dhs_kr_children_with_geo.rds")

dhs1 <- dhs1 %>%
  mutate(
    # Cleaned birth weight in grams (set special codes to NA)
    birth_weight_grams = ifelse(birth_weight %in% c(9996, 9998, 9999), NA, birth_weight),
    
    # Cleaned perceived birth size (numeric 1–5; NA if don't know or missing)
    birth_size_clean = ifelse(birth_size %in% c(8, 9), NA, birth_size),
    
    # Optional: labeled factor for perceived size
    birth_size_factor = case_when(
      birth_size == 1 ~ "Very large",
      birth_size == 2 ~ "Larger than average",
      birth_size == 3 ~ "Average",
      birth_size == 4 ~ "Smaller than average",
      birth_size == 5 ~ "Very small",
      birth_size %in% c(8, 9) ~ NA_character_
    ) %>% factor(levels = c("Very large", "Larger than average", "Average",
                            "Smaller than average", "Very small"))
  )


dhs2 <- dhs1 |>
  filter(survey_year %in% c(1998, 2003, 2008)) |>
  haven::zap_labels() |>
  mutate(
    municipality_name = case_when(
      survey_year == 2008 & latitude == 14.6055 & longitude == 120.9485 ~ "Manila",
      survey_year == 2008 & latitude == 14.56937 & longitude == 120.96649 ~ "Manila",
      survey_year == 2008 & latitude == 14.56748 & longitude == 120.97882 ~ "Manila",
      survey_year == 2008 & latitude == 14.56274 & longitude == 120.98432 ~ "Manila",
      survey_year == 2008 & latitude == 14.59441 & longitude == 121.01443 ~ "Manila",
      TRUE ~ municipality_name
    ),
    manila = factor(str_to_lower(municipality_name) == "manila", levels = c(FALSE, TRUE)),
    treated = as.integer(manila == TRUE),
    birth_year = if_else(birth_year < 100, birth_year + 1900, birth_year),
    birth_year_relative_to_policy = birth_year - 2000,
    post_policy = as.integer(birth_year >= 2001),
    sample_weight = sample_weight / 1e6,
    urban = case_when(
      str_to_lower(residence_type) %in% c("urban", "1") ~ 1L,
      str_to_lower(residence_type) %in% c("rural", "2") ~ 0L,
      TRUE ~ NA_integer_
    ),
    mother_educ_years = case_when(
      !is.na(mother_educ_attain) ~ as.numeric(mother_educ_attain),
      TRUE ~ NA_real_
    ),
    number_of_siblings = living_children - 1,
    ncr = (region_dhs == 14),
    num_vaccines = rowSums(across(
      c(received_dpt1, received_dpt2, received_dpt3,
        received_polio1, received_polio2, received_polio3,
        received_measles, received_bcg),
      ~ as.integer(.x == 1 | .x == TRUE)
    ), na.rm = TRUE)
  )

# 3. Construct summary dataset
dhs_summary <- dhs2 |>
  mutate(
    Group = case_when(
      manila == TRUE & post_policy == 0 ~ "Manila, Pre-Policy",
      manila == TRUE & post_policy == 1 ~ "Manila, Post-Policy",
      region_dhs == 14 & manila == FALSE & post_policy == 0 ~ "Other NCR Cities, Pre-Policy",
      region_dhs == 14 & manila == FALSE & post_policy == 1 ~ "Other NCR Cities, Post-Policy",
      region_dhs != 14 & post_policy == 0 ~ "Rest of the Philippines, Pre-Policy",
      region_dhs != 14 & post_policy == 1 ~ "Rest of the Philippines, Post-Policy",
      TRUE ~ NA_character_
    ),
    Group = factor(Group, levels = c(
      "Manila, Pre-Policy", "Manila, Post-Policy",
      "Other NCR Cities, Pre-Policy", "Other NCR Cities, Post-Policy",
      "Rest of the Philippines, Pre-Policy", "Rest of the Philippines, Post-Policy"
    )),
    birth_size = factor(birth_size,
                        levels = c(5, 4, 3, 2, 1, 8, 9),
                        labels = c(
                          "Very small", "Smaller than average", "Average size",
                          "Larger than average", "Very large", "Unknown", "Missing"
                        )
    ),
    vaccine_cat = case_when(
      num_vaccines == 0 ~ "No Vaccines",
      num_vaccines %in% 1:2 ~ "1–2 Vaccines",
      num_vaccines %in% 3:4 ~ "3–4 Vaccines",
      num_vaccines >= 5 ~ "5+ Vaccines"
    )
  ) |>
  select(Group, birth_weight_grams, birth_size_factor, vaccine_cat)

# 4. Table: Birth Weight (Mean (SD))
birth_weight_summary_t <- dhs_summary |>
  filter(!is.na(Group)) |>
  group_by(Group) |>
  summarise(
    `Birth weight (g)` = sprintf("%.2f (%.2f), n = %d",
                                 mean(birth_weight_grams, na.rm = TRUE),
                                 sd(birth_weight_grams, na.rm = TRUE),
                                 sum(!is.na(birth_weight_grams)))
  )

sink("../outputs/dhs_summary_birth_weight_t.tex")
kable(birth_weight_summary_t,
      format = "latex", booktabs = TRUE,
      caption = "Birth Weight by Location and Policy Period",
      align = "l c") |>
  kable_styling(latex_options = "hold_position")
sink()

# 5. Table: Perceived Birth Size (no Unknown or Missing)
birth_size_summary_t <- dhs_summary |>
  filter(!is.na(birth_size_factor), !is.na(Group)) |>
  filter(!birth_size_factor %in% c("Unknown", "Missing")) |>  # Exclude unclear responses
  group_by(Group, birth_size_factor) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Group) |>
  mutate(
    percent = count / sum(count),
    display = sprintf("\\makecell{%d \\\\ (%.1f\\%%)}", count, percent * 100)
  ) |>
  select(Group, birth_size_factor, display) |>
  pivot_wider(
    names_from = birth_size_factor,
    values_from = display
  ) |>
  # Optional: order columns manually for consistent reporting
  select(
    Group,
    `Very large`,
    `Larger than average`,
    `Average`,
    `Smaller than average`,
    `Very small`
  )

colnames(birth_size_summary_t) <- c(
  "Group",
  "\\makecell{Very\\\\small}",
  "\\makecell{Smaller\\\\than\\\\average}",
  "\\makecell{Average\\\\size}",
  "\\makecell{Larger\\\\than\\\\average}",
  "\\makecell{Very\\\\large}"
)

# Export to LaTeX
sink("../outputs/dhs_summary_birth_size_t.tex")
kable(birth_size_summary_t,
      format = "latex", booktabs = TRUE,
      caption = "Perceived Birth Size by Location and Policy Period",
      align = "lccccc",
      escape = FALSE, linesep = "") |>
  kable_styling(latex_options = "hold_position")
sink()



# 6. Table: Vaccine Categories (no makecell formatting)
vaccine_summary_t <- dhs_summary |>
  filter(!is.na(vaccine_cat), !is.na(Group)) |>
  group_by(Group, vaccine_cat) |>
  summarise(n = n(), .Groups = "drop") |>
  group_by(Group) |>
  mutate(
    pct = n / sum(n),
    value = sprintf("%d (%.1f%%)", n, pct * 100)
  ) |>
  select(Group, vaccine_cat, value) |>
  pivot_wider(names_from = vaccine_cat, values_from = value) |>
  select(Group, everything())

sink("../outputs/dhs_summary_vaccines_t.tex")
kable(vaccine_summary_t,
      format = "latex", booktabs = TRUE,
      caption = "Number of Vaccines Received by Location and Policy Period",
      align = "lcccc") |>
  kable_styling(latex_options = "hold_position")
sink()