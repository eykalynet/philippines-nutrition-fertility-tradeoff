# ==============================================================================
# Script Name : 05_append_geo_to_dhs_panel.R
# Purpose     : Attach geographic metadata (region, province, municipality)
#               to DHS cluster IDs using GPS coordinates and modal mapping.
# Years       : 2003, 2008 (GPS-based); 1993, 1998, 2013 (modal guess)
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads the under-5 DHS child panel without geographic metadata
#     - Imports 2003 and 2008 DHS GPS cluster shapefiles
#     - Maps GPS clusters to municipalities using GADM Level-3 shapefiles
#     - Constructs modal municipality-province map by region × urbanicity
#     - Guesses province/municipality for non-GPS years using modal map
#     - Joins region names and saves final geo-tagged child-level panel
#     - Includes diagnostics for unmatched or missing geographic data
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/05_append_geo_to_dhs_panel.R")

# 1. Load libraries ============================================================
library(dplyr)
library(sf)
library(haven)
library(readr)
library(here)
library(stringr)

# 2. Load DHS panel dataset ====================================================
dhs_all <- read_rds(here("data-analysis", "clean-data", "dhs_under5_panel_no_geo.rds"))

# 3. Load 2013 region shapefile and fix REGCODE formatting =====================
region_sf <- st_read(here("data-analysis", "raw-data", "dhs_raw", "2013_gps", "sdr_subnational_boundaries.shp")) |>
  select(REGCODE, REGNAME) |>
  mutate(REGCODE = str_pad(as.character(REGCODE), width = 2, pad = "0")) |>
  st_drop_geometry() |>
  distinct()

# 4. Load 2003 and 2008 GPS cluster shapefiles =================================
gps_2003 <- st_read(here("data-analysis", "raw-data", "dhs_raw", "2003_gps", "PHGE43FL.shp")) |>
  mutate(year = 2003)

gps_2008 <- st_read(here("data-analysis", "raw-data", "dhs_raw", "2008_gps", "PHGE52FL.shp")) |>
  mutate(year = 2008)

gps_combined <- bind_rows(gps_2003, gps_2008)

# 5. Load GADM level-3 shapefile for provinces + municipalities ================
gadm_muni <- st_read(here("data-analysis", "raw-data", "dhs_raw", "gadm", "gadm41_PHL_3.shp")) |>
  select(province_name = NAME_2, municipality_name = NAME_3)

# 6. Spatial join: assign each GPS cluster to a municipality ===================
gps_combined <- st_transform(gps_combined, crs = st_crs(gadm_muni))

clusters_with_geo <- st_join(gps_combined, gadm_muni, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(municipality_name)) |>
  mutate(
    region_code = str_pad(as.character(DHSREGCO), width = 2, pad = "0"),
    urban_rural = case_when(
      str_to_lower(URBAN_RURA) %in% c("u", "urban") ~ "u",
      str_to_lower(URBAN_RURA) %in% c("r", "rural") ~ "r",
      TRUE ~ NA_character_
    )
  ) |>
  select(region_code, urban_rural, province_name, municipality_name)

# 7. Build modal map of municipality + province by region × urban_rural ========
mode_geo_map <- clusters_with_geo |>
  group_by(region_code, urban_rural, province_name, municipality_name) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n)) |>
  group_by(region_code, urban_rural) |>
  slice(1) |>
  ungroup()

# 8. Prepare DHS panel for joins ===============================================
dhs_all <- dhs_all |>
  mutate(
    year = 1900 + floor(interview_date / 12),
    region_code = str_pad(as.character(region), width = 2, pad = "0"),
    urban_rural = case_when(
      urban_rural %in% c("1", 1) ~ "u",
      urban_rural %in% c("2", 2) ~ "r",
      TRUE ~ NA_character_
    )
  )

# Fix: Add missing region codes 41 and 42
region_sf <- region_sf |>
  bind_rows(
    tibble(
      REGCODE = c("41", "42"),
      REGNAME = c("Region IVA - CALABARZON", "Region IVB - MIMAROPA")
    )
  )

# 9. Join region name ==========================================================
dhs_all <- dhs_all |>
  left_join(region_sf, by = c("region_code" = "REGCODE")) |>
  rename(region_name = REGNAME)

# 10. Join province and municipality using modal map ===========================
dhs_all <- dhs_all |>
  left_join(mode_geo_map, by = c("region_code", "urban_rural")) |>
  mutate(
    province_source = ifelse(year %in% c(2003, 2008), "GPS-derived", "guessed"),
    municipality_source = ifelse(year %in% c(2003, 2008), "GPS-derived", "guessed")
  )

# 11. Reorder variables: geo info first ========================================
geo_vars <- c(
  "cluster_id", "region_code", "region_name",
  "province_name", "province_source",
  "municipality_name", "municipality_source",
  "urban_rural", "year"
)
other_vars <- setdiff(names(dhs_all), geo_vars)
dhs_all <- dhs_all[, c(geo_vars, other_vars)]

# 12. Save final dataset =======================================================
clean_path <- here("data-analysis", "clean-data")
write_rds(dhs_all, file.path(clean_path, "dhs_under5_panel_with_geo_final.rds"))
write_dta(dhs_all, file.path(clean_path, "dhs_under5_panel_with_geo_final.dta"))

# 13. Diagnostic: Check for missing municipality assignment ====================
table(dhs_all$year, is.na(dhs_all$municipality_name))
colSums(is.na(dhs_all[, c("region_name", "province_name", "municipality_name")]))