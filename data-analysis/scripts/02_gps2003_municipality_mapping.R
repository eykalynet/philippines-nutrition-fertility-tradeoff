# ==============================================================================
# Script Name : gps2003_municipality_mapping.R
# Purpose     : Map 2003 DHS GPS clusters to municipalities using GADM level-3 shapefile
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
#
# Description :
#   This script performs the following steps:
#     - Loads the 2003 DHS GPS shapefile directly (no .dta used)
#     - Filters out invalid or missing cluster coordinates
#     - Loads the GADM level-3 administrative boundary shapefile for the Philippines
#     - Performs a spatial join of GPS points to municipal boundaries (NAME_3)
#     - Outputs a clean .csv file mapping GPS clusters to municipalities
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/02_gps2003_municipality_mapping.R")

# 1. Load libraries ============================================================
# install.packages(c("sf", "dplyr", "here"))
library(sf)     # For reading shapefiles & spatial joins
library(dplyr)  # For data wrangling
library(here)   # For path management

# 2. File paths ================================================================
gps_dir   <- here("data-analysis", "raw-data", "dhs_raw", "2003_gps", "PHGE43FL")
gadm_dir  <- here("data-analysis", "raw-data", "dhs_raw", "gadm")
save_dir  <- here("data-analysis", "clean-data")
output_fn <- "gps2003_with_municipality.csv"

gps_file   <- file.path(gps_dir, "PHGE43FL.shp")               # shapefile
gadm_file  <- file.path(gadm_dir, "gadm41_PHL_3.shp")          # GADM Level-3
output_csv <- file.path(save_dir, output_fn)

# 3. Load GPS shapefile ========================================================
gps_sf <- st_read(gps_file)

# 4. Filter valid points (if needed) ===========================================
# Check for NA coordinates, though most points should be valid
# Update these column names if necessary (use names(gps_sf))
gps_sf <- gps_sf |> filter(!is.na(LATNUM), !is.na(LONGNUM))

# Optional: add cluster ID if not present
if (!"DHSCLUST" %in% names(gps_sf)) {
  gps_sf <- gps_sf |> mutate(cluster_id = row_number())
}

# 5. Load GADM Level-3 shapefile ===============================================
gadm3 <- st_read(gadm_file)

# 6. Spatial join: assign municipality to each cluster =========================
gps_with_muni <- st_join(gps_sf, gadm3[, c("NAME_3", "NAME_2", "NAME_1")])

# 7. Save output =============================================================== 
write.csv(st_drop_geometry(gps_with_muni), output_csv, row.names = FALSE)

message("Done. Municipality data for 2003 saved to: ", normalizePath(output_csv))
