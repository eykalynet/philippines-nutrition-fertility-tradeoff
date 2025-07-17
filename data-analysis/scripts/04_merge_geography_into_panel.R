# ==============================================================================
# merge_geography_into_panel.R -
# Purpose = Add municipality + province names to DHS child panel (1993–2013),
#           using cluster-to-GADM Level-3 mapping from 2003 and 2008 GPS shapefiles.
#           Since GPS not available for 1993 and 1998, 2003 cluster matches are reused.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu)
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads the under-5 DHS panel.
#   2) Loads 2003 and 2008 cluster-to-municipality mappings.
#   3) Reuses 2003 GPS mappings for the 1993 and 1998 rounds.
#   4) Merges municipality and province data into full panel.
#   5) Saves the updated dataset.
# ==============================================================================

# 1. Load libraries ============================================================
library(dplyr)
library(readr)
library(here)

# 2. Load child panel and GPS-to-municipality mappings =========================
panel_path <- here("data-analysis", "clean-data", "dhs_merged_panel.rds")
dhs_all <- read_rds(panel_path)

gps2003 <- read.csv(here("data-analysis", "clean-data", "gps2003_with_municipality.csv"))
gps2008 <- read.csv(here("data-analysis", "clean-data", "gps2008_with_municipality.csv"))

# 3. Standardize + tag mappings ================================================
gps2003_clean <- gps2003 |>
  rename(cluster_id = DHSCLUST) |>
  mutate(
    NAME_2 = trimws(NAME_2),  # Province
    NAME_3 = trimws(NAME_3)   # Municipality/City
  ) |>
  select(cluster_id, NAME_2, NAME_3)

gps2008_clean <- gps2008 |>
  rename(cluster_id = DHSCLUST) |>
  mutate(
    survey_year = 2008,
    NAME_2 = trimws(NAME_2),
    NAME_3 = trimws(NAME_3)
  ) |>
  select(cluster_id, survey_year, NAME_2, NAME_3)

# 4. Assign 2003 locations to pre-GPS rounds (1993 & 1998) ======================
# Assume 1:1 match between cluster ID in 1993/1998 and in 2003 — document as approximation
gps_proxy_9398 <- gps2003_clean |>
  mutate(survey_year = NA_integer_) |>
  slice(rep(1:n(), each = 1))  # no change; placeholder if we later match probabilistically

# Duplicate this GPS mapping to 1993 and 1998 rounds
gps_1993 <- gps_proxy_9398 |> mutate(survey_year = 1993)
gps_1998 <- gps_proxy_9398 |> mutate(survey_year = 1998)
gps_2003 <- gps2003_clean |> mutate(survey_year = 2003)

# Combine all geography
gps_all <- bind_rows(gps_1993, gps_1998, gps_2003, gps2008_clean)

# 5. Merge into panel ==========================================================
dhs_geo <- dhs_all |>
  left_join(gps_all, by = c("survey_year", "cluster_id"))

# 6. Save updated panel with NAME_2 (province) and NAME_3 (municipality) =======
write_rds(dhs_geo, here("data-analysis", "clean-data", "dhs_merged_panel_with_geo.rds"))
message("DHS panel with approximate municipality data saved.")
