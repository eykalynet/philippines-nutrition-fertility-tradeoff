# ==============================================================================
# gps2008_municipality_mapping.R -
# Purpose = Map 2008 DHS GPS clusters to municipalities using GADM level-3 shapefile.
# Author  : Erika Salvador '28 (esalvador28@amherst.edu) 
# Project : Manila Contraceptive Ban & the Quantity-Quality Trade-off (nutrition arm)
#
# In plain English this script:
#   1) Loads the 2008 DHS GPS shapefile directly (no .dta used).
#   2) Filters valid cluster points (with coordinates).
#   3) Loads GADM level-3 shapefile for the Philippines.
#   4) Spatially joins GPS coordinates to municipalities (NAME_3).
#   5) Outputs a clean CSV with cluster-to-municipality mapping.
# ==============================================================================

# 1. Load libraries ============================================================
# install.packages(c("sf", "dplyr", "here"))
library(sf)     # for reading shapefiles & spatial joins
library(dplyr)  # for data wrangling
library(here)   # for path management

# 2. File paths ================================================================
gps_dir   <- here("data-analysis", "raw-data", "dhs_raw", "2008_gps", "PHGE52FL")
gadm_dir  <- here("data-analysis", "raw-data", "dhs_raw", "gadm")
save_dir  <- here("data-analysis", "clean-data")
output_fn <- "gps2008_with_municipality.csv"

gps_file   <- file.path(gps_dir, "PHGE52FL.shp")               # 2008 DHS shapefile
gadm_file  <- file.path(gadm_dir, "gadm41_PHL_3.shp")          # GADM Level-3
output_csv <- file.path(save_dir, output_fn)

# 3. Load GPS shapefile ========================================================
gps_sf <- st_read(gps_file)

# 4. Filter valid coordinates ==================================================
# Replace LATNUM/LONGNUM with the actual column names (likely the same as 2003)
gps_sf <- gps_sf |> filter(!is.na(LATNUM), !is.na(LONGNUM))

# Optional: add cluster ID if missing
if (!"DHSCLUST" %in% names(gps_sf)) {
  gps_sf <- gps_sf |> mutate(cluster_id = row_number())
}

# 5. Load GADM Level-3 shapefile ===============================================
gadm3 <- st_read(gadm_file)

# 6. Spatial join: assign municipality to each cluster =========================
gps_with_muni <- st_join(gps_sf, gadm3[, c("NAME_3", "NAME_2", "NAME_1")])

# 7. Save output =============================================================== 
write.csv(st_drop_geometry(gps_with_muni), output_csv, row.names = FALSE)

message("Done. Municipality data for 2008 saved to: ", normalizePath(output_csv))
