# ==============================================================================
# merge_geography_into_panel.R -
# Purpose = Add municipality + province names to DHS child panels (1993–2013),
#           using cluster-to-GADM Level-3 mapping from 2003 and 2008 GPS shapefiles.
#           Reuses 2003 mappings for 1993 and 1998 where GPS is unavailable.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu)
# Project : Manila Contraceptive Ban & the Quantity–Quality Trade-off
#
# In plain English this script:
#   1) Loads all 3 DHS panel variants (merged, imputed, IPW).
#   2) Loads GPS-based cluster–municipality mappings for 2003 and 2008.
#   3) Reuses 2003 GPS matches for 1993 and 1998 rounds.
#   4) Merges province and municipality info into each panel version.
#   5) Saves updated datasets with `_with_geo` suffix.
# ==============================================================================

# 1. Load libraries ============================================================
library(dplyr)
library(readr)
library(here)

# 2. Load child panels =========================================================
merged_path <- here("data-analysis", "clean-data", "dhs_merged_panel.rds")
imputed_path <- here("data-analysis", "clean-data", "dhs_imputed_panel.rds")
ipw_path     <- here("data-analysis", "clean-data", "dhs_panel_with_ipw.rds")

dhs_merged <- read_rds(merged_path)
dhs_imputed <- read_rds(imputed_path)
dhs_ipw <- read_rds(ipw_path)

# 3. Load GPS–municipality mappings ============================================
gps2003 <- read.csv(here("data-analysis", "clean-data", "gps2003_with_municipality.csv"))
gps2008 <- read.csv(here("data-analysis", "clean-data", "gps2008_with_municipality.csv"))

# 4. Standardize and prepare GPS data ==========================================
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

# 5. Reuse 2003 GPS clusters for 1993 and 1998 ================================
gps_proxy_9398 <- gps2003_clean
gps_1993 <- gps_proxy_9398 |> mutate(survey_year = 1993)
gps_1998 <- gps_proxy_9398 |> mutate(survey_year = 1998)
gps_2003 <- gps2003_clean |> mutate(survey_year = 2003)

# Combine all known GPS matches
gps_all <- bind_rows(gps_1993, gps_1998, gps_2003, gps2008_clean)

# 6. Merge geography into each dataset =========================================
dhs_merged_geo <- dhs_merged |>
  left_join(gps_all, by = c("survey_year", "cluster_id"))

dhs_imputed_geo <- dhs_imputed |>
  left_join(gps_all, by = c("survey_year", "cluster_id"))

dhs_ipw_geo <- dhs_ipw |>
  left_join(gps_all, by = c("survey_year", "cluster_id"))

# 7. Save updated datasets =====================================================
write_rds(dhs_merged_geo, here("data-analysis", "clean-data", "dhs_merged_panel_with_geo.rds"))
write_rds(dhs_imputed_geo, here("data-analysis", "clean-data", "dhs_imputed_panel_with_geo.rds"))
write_rds(dhs_ipw_geo, here("data-analysis", "clean-data", "dhs_panel_with_ipw_with_geo.rds"))

message("All panel variants updated with province and municipality labels.")
