# --- Load libraries ---
library(data.table)
library(kableExtra)

# --- Load census data ---
census1 <- readRDS("../clean-data/census_1990_2010.rds")
census1 <- as.data.table(census1)

# --- Ensure numeric types ---
census1[, `:=`(
  age = as.numeric(age),
  sex = as.numeric(sex),
  year = as.numeric(year),
  geo1_ph = as.numeric(geo1_ph),
  hhwt = as.numeric(hhwt)
)]

# --- Step 1: Count births (age 0–5) per year by province ---
census_kids <- census1[age >= 0 & age <= 5]
census_kids[, birth_year := year - age]

province_ids <- unique(census_kids$geo1_ph)
output_file <- "../clean-data/census_births_by_year.csv"
fwrite(data.table(), output_file)  # clear file first

for (i in seq_along(province_ids)) {
  pid <- province_ids[i]
  
  births_chunk <- census_kids[geo1_ph == pid,
                              .(num_births = sum(hhwt, na.rm = TRUE)),
                              by = birth_year
  ]
  births_chunk[, geo1_ph := pid]
  
  fwrite(births_chunk, output_file, append = (i != 1), col.names = (i == 1))
}

# --- Step 2: Load merged data for birth rate calculation ---
births_by_year <- fread("../clean-data/census_births_by_year.csv")
women_at_risk  <- fread("../clean-data/census_women_at_risk.csv")

birth_rates <- merge(births_by_year, women_at_risk, by = c("birth_year", "geo1_ph"))
birth_rates[, birth_rate := num_births / num_women]

# --- Step 3: Categorize regions and compute summary ---
birth_rates[, location_group := fcase(
  geo1_ph == 608039, "Manila",
  geo1_ph %in% c(608074, 608075, 608076), "Other NCR",
  default = "Non-NCR"
)]
birth_rates[, location_group := factor(location_group, levels = c("Manila", "Other NCR", "Non-NCR"))]

# Rename the levels
levels(birth_rates$location_group) <- c("Manila", "Other NCR Cities", "Rest of the Philippines")


birth_rates[, post_policy := birth_year >= 2000]
birth_rates[, period := ifelse(post_policy, "Post-2000", "Pre-2000")]

# --- Step 4: Compute average birth rates and % change ---
census_br_summary <- birth_rates[
  , .(avg_birth_rate = weighted.mean(birth_rate, num_women, na.rm = TRUE)),
  by = .(location_group, period)
]

census_br_summary <- dcast(
  census_br_summary,
  location_group ~ period,
  value.var = "avg_birth_rate"
)
# Round and format columns
census_br_summary_simple <- census_br_summary |>
  mutate(
    `Pre-2000` = round(`Pre-2000`, 4),
    `Post-2000` = round(`Post-2000`, 4),
    `Percent Change` = round((`Post-2000` - `Pre-2000`) / `Pre-2000` * 100, 1)
  ) |>
  select(`Region Group` = location_group, `Pre-2000`, `Post-2000`, `Percent Change`)

# Export LaTeX
output_tex <- "../outputs/census_birthrate_change.tex"

sink(output_tex)
census_br_summary_simple |>
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Average Birth Rates Before and After 2000 by Region Group"
  ) |>
  kable_styling(latex_options = "hold_position") |>
  print()
sink()
