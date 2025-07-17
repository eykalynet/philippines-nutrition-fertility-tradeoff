# ==============================================================================
# gps2003_municipality_mapping.R -
# Purpose = Map 2003 DHS GPS clusters to municipalities using GADM level-3 shapefile.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity-Quality Trade-off (nutrition arm)
#
# In plain English this script:
#   1) Loads the 2003 DHS GPS coordinates from usdb.dta.
#   2) Converts to spatial features and filters out invalid points.
#   3) Loads GADM level-3 shapefile for the Philippines.
#   4) Spatially joins GPS coordinates to municipalities (NAME_3).
#   5) Outputs a clean CSV with cluster-to-municipality mapping.
# ==============================================================================

# 1. Load libraries ============================================================
# install.packages(c("haven", "sf", "dplyr", "here"))
library(haven)  # read_dta()
library(sf)     # spatial data
library(dplyr)  # data wrangling
library(here)   # platform-independent paths

# 2. File paths ================================================================
gps_dir   <- here("data-analysis", "raw-data", "dhs_raw", "2003_gps", "PHGE43FL")
gadm_dir  <- here("data-analysis", "raw-data", "dhs_raw", "gadm")
save_dir  <- here("data-analysis", "clean-data")
output_fn <- "gps2003_with_municipality.csv"

gps_file   <- file.path(gps_dir, "usdb.dta")
gadm_file  <- file.path(gadm_dir, "gadm41_PHL_3.shp")
output_csv <- file.path(save_dir, output_fn)

# 3. Load & clean GPS data =====================================================
gps2003 <- read_dta(gps_file) |>
  filter(!is.na(LATNUM), !is.na(LONGNUM)) |>
  mutate(cluster_id = row_number())

# 4. Convert to spatial ========================================================
gps_sf <- st_as_sf(gps2003, coords = c("LONGNUM", "LATNUM"), crs = 4326)

# 5. Load GADM level-3 shapefile ===============================================
gadm3 <- st_read(gadm_file)

# 6. Spatial join: assign municipality to each cluster =========================
gps_with_muni <- st_join(gps_sf, gadm3[, c("NAME_3", "NAME_2", "NAME_1")])

# 7. Save output =============================================================== 
write.csv(st_drop_geometry(gps_with_muni), output_csv, row.names = FALSE)

message("Done. Municipality data for 2003 saved to: ", normalizePath(output_csv))
