# ==============================================================================
# Script Name : 05_append_geo_to_dhs_panel.R
# Purpose     : Append cleaned geographic metadata (region, province, municipality)
#               to the harmonized DHS cluster-level dataset using GPS-based mapping
# Author      : Erika Salvador '28 (esalvador28@amherst.edu)
# Project     : Empirical Evidence of the Quantity–Quality Trade-off in the Philippines
# ==============================================================================

# 0. Set Project Root ==========================================================
library(here)
here::i_am("data-analysis/scripts/05_append_geo_to_dhs_panel.R")

# 1. Load Required Libraries ===================================================
library(sf)        # for shapefiles
library(readxl)    # for Excel GPS code files
library(dplyr)     # for data manipulation
library(stringr)   # for string cleaning
library(haven)     # for saving to .dta
library(janitor)   # for cleaning variable names

# 2. Load Harmonized DHS Geo Variables ========================================
dhs_all <- readRDS(here("data-analysis", "clean-data", "dhs_kr_children_harmonized.rds"))

dhs_geo <- readRDS(here("data-analysis", "clean-data", "dhs_kr_children_harmonized.rds")) |>
  select(survey_year, cluster_id, region, province, municipality)

# 3. Load and Clean 2003 GPS-Municipality Data ================================
gps2003 <- readr::read_csv(here("data-analysis", "clean-data", "gps2003_with_municipality.csv")) |>
  select(-ADM1FIPS, -ADM1FIPSNA, -ADM1SALBNA, -ADM1SALBCO) |>
  rename(
    cluster_id        = DHSCLUST,
    municipality_name = NAME_2,
    province_name     = NAME_1
  ) |>
  mutate(survey_year = 2003)

# 4. Load and Clean 2008 GPS-Municipality Data ================================
gps2008 <- readr::read_csv(here("data-analysis", "clean-data", "gps2008_with_municipality.csv")) |>
  select(-ADM1FIPS, -ADM1FIPSNA, -ADM1SALBNA, -ADM1SALBCO) |>
  rename(
    cluster_id        = DHSCLUST,
    municipality_name = NAME_2,
    province_name     = NAME_1
  ) |>
  mutate(survey_year = 2008)

# 5B. Create Proxy Year Copies for Missing GPS Years ==========================

# Use 2003 GPS for 1993 and 1998
gps_1993 <- gps2003 |> mutate(survey_year = 1993)
gps_1998 <- gps2003 |> mutate(survey_year = 1998)

# Use 2008 GPS for 2013
gps_2013 <- gps2008 |> mutate(survey_year = 2013)

# Combine original + proxy GPS datasets
gps_muni <- bind_rows(gps2003, gps2008, gps_1993, gps_1998, gps_2013) |>
  mutate(
    province_name     = str_to_title(str_trim(province_name)),
    municipality_name = str_to_title(str_trim(municipality_name))
  )

# 6. Merge Harmonized DHS with GPS Metadata ===================================
dhs_geo <- dhs_geo |>
  left_join(gps_muni, by = c("survey_year", "cluster_id"))

# 7. Rename and Reorganize Columns ============================================
dhs_geo <- dhs_geo |>
  rename(
    year               = survey_year,
    cluster            = cluster_id,
    region_dhs         = region,
    province_dhs       = province,
    municipality_dhs   = municipality,
    dhsid              = DHSID,
    dhscc              = DHSCC,
    dhsyear            = DHSYEAR,
    adm1dhs            = ADM1DHS,
    cc_fips            = CCFIPS,
    source             = SOURCE,
    urban_rural        = URBAN_RURA,
    datum              = DATUM,
    admin_region_name  = ADM1NAME,
    admin_region_code  = DHSREGCO,
    admin_region_label = DHSREGNA,
    latitude           = LATNUM,
    longitude          = LONGNUM,
    altitude_gps       = ALT_GPS,
    altitude_dem       = ALT_DEM,
    barangay_name      = NAME_3,
    municipality_name  = municipality_name,
    province_name      = province_name
  )

# 8. Drop Redundant Columns ===================================================
dhs_geo <- dhs_geo |> 
  select(
    -dhsyear,                  # same as year
    -adm1dhs,                  # redundant
    -admin_region_code,        # same as region
    -admin_region_label        # same as admin_region_name
  )

# 9. Reorder Columns for Clarity ==============================================
dhs_geo <- dhs_geo |>
  select(
    # --- Core Identifiers ---
    year,
    dhsid,
    dhscc,
    cluster,
    
    # --- Geographic Hierarchy ---
    region_dhs,
    admin_region_name,
    province_dhs,
    province_name,
    municipality_dhs,
    municipality_name,
    barangay_name,
    
    # --- Location Metadata ---
    latitude,
    longitude,
    altitude_gps,
    altitude_dem,
    
    # --- Administrative + Source Metadata ---
    cc_fips,
    source,
    urban_rural,
    datum
  )

# 10. Standardize Region Variable =============================================
# Create `region_name` based on `region_dhs` and `year`
dhs_geo <- dhs_geo %>%
  mutate(
    region_dhs = case_when(
      # --- For 2003 mappings ---
      region_dhs == 3  & year == 2003 ~ 1,
      region_dhs == 4  & year == 2003 ~ 2,
      region_dhs == 5  & year == 2003 ~ 3,
      region_dhs == 6  & year == 2003 ~ 4,
      region_dhs == 8  & year == 2003 ~ 5,
      region_dhs == 9  & year == 2003 ~ 6,
      region_dhs == 10 & year == 2003 ~ 7,
      region_dhs == 11 & year == 2003 ~ 8,
      region_dhs == 12 & year == 2003 ~ 9,
      region_dhs == 13 & year == 2003 ~ 10,
      region_dhs == 14 & year == 2003 ~ 11,
      region_dhs == 15 & year == 2003 ~ 12,
      region_dhs == 1  & year == 2003 ~ 13,
      region_dhs == 2  & year == 2003 ~ 14,
      region_dhs == 17 & year == 2003 ~ 15,
      region_dhs == 16 & year == 2003 ~ 16,
      region_dhs == 7  & year == 2003 ~ 17,
      
      # --- For 2008 mappings ---
      region_dhs == 1  & year == 2008 ~ 1,
      region_dhs == 2  & year == 2008 ~ 2,
      region_dhs == 3  & year == 2008 ~ 3,
      region_dhs == 41 & year == 2008 ~ 4,
      region_dhs == 5  & year == 2008 ~ 5,
      region_dhs == 6  & year == 2008 ~ 6,
      region_dhs == 7  & year == 2008 ~ 7,
      region_dhs == 8  & year == 2008 ~ 8,
      region_dhs == 9  & year == 2008 ~ 9,
      region_dhs == 10 & year == 2008 ~ 10,
      region_dhs == 11 & year == 2008 ~ 11,
      region_dhs == 12 & year == 2008 ~ 12,
      region_dhs == 14 & year == 2008 ~ 13,
      region_dhs == 15 & year == 2008 ~ 14,
      region_dhs == 16 & year == 2008 ~ 15,
      region_dhs == 13 & year == 2008 ~ 16,
      region_dhs == 42 & year == 2008 ~ 17,
      
      # --- Retain original value for all other years ---
      TRUE ~ region_dhs
    )
  )

# 10. Province Variable ========================================================
# Region 1
dhs_geo <- dhs_geo |>
  mutate(
    province_dhs = case_when(
      province_name == "Ilocos Norte" & region_dhs == 1 ~ 28,
      province_name == "Ilocos Sur"   & region_dhs == 1 ~ 29,
      province_name == "La Union"     & region_dhs == 1 ~ 33,
      province_name == "Pangasinan"   & region_dhs == 1 ~ 55,
      TRUE ~ province_dhs
    )
  )

# Region 2
dhs_geo <- dhs_geo |>
  mutate(
    # Assign province_dhs = 15 for CAGAYAN or inferred municipalities in Region II
    province_dhs = case_when(
      (province_name == "Cagayan" & region_dhs == 2) ~ 15,
      (province_name == "" & region_dhs == 2 & municipality_name %in% c("Iguig", "Lal-Lo", "Pamplona")) ~ 15,
      TRUE ~ province_dhs
    ),
    # Fix province_name based on year and cluster ID
    province_name = case_when(
      year == 2008 & cluster == 231 ~ "Isabela",
      year == 2008 & cluster == 235 ~ "Nueva Vizcaya",
      TRUE ~ province_name
    )
  ) |>
  mutate(
    # Assign final province_dhs values for other Region II provinces
    province_dhs = case_when(
      province_name == "Isabela"       & region_dhs == 2 ~ 31,
      province_name == "Nueva Vizcaya" & region_dhs == 2 ~ 50,
      province_name == "Quirino"       & region_dhs == 2 ~ 57,
      province_name == "Batanes"       & region_dhs == 2 ~ 9,
      TRUE ~ province_dhs
    )
  )

# Region 3
dhs_geo <- dhs_geo |>
  # Recode province_name based on year and cluster
  mutate(
    province_name = case_when(
      province_name == "Pampanga"     & year == 2003 & cluster == 77  ~ "Bulacan",
      province_name == "Metro Manila" & year == 2003 & cluster == 82  ~ "Bulacan",
      province_name == "Metro Manila" & year == 2008 & cluster %in% c(251, 252, 253, 254) ~ "Bulacan",
      province_name == "Bulakan"      & region_dhs == 3               ~ "Bulacan",
      cluster == 109 & year == 2003                                 ~ "Pampanga",
      TRUE ~ province_name
    ),
    
    municipality_name = case_when(
      cluster == 109 & year == 2003 ~ "Macabebe",
      TRUE ~ municipality_name
    )
  ) |> 
  # Recode province_dhs based on updated province_name and region_dhs
  mutate(
    province_dhs = case_when(
      province_name == "Bataan"       & region_dhs == 3 ~ 8,
      province_name == "Bulacan"      & region_dhs == 3 ~ 14,
      province_name == "Nueva Ecija"  & region_dhs == 3 ~ 49,
      province_name == "Pampanga"     & region_dhs == 3 ~ 54,
      province_name == "Tarlac"       & region_dhs == 3 ~ 69,
      province_name == "Zambales"     & region_dhs == 3 ~ 71,
      province_name == "Aurora"       & region_dhs == 3 ~ 77,
      TRUE ~ province_dhs
    )
  )

# Region 4
dhs_geo <- dhs_geo |>
  # Fix province_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster >= 311 & cluster <= 316 ~ "Cavite",
      year == 2008 & cluster == 344                  ~ "Laguna",
      year == 2008 & cluster == 366                  ~ "Rizal",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on corrected province_name and region_dhs
  mutate(
    province_dhs = case_when(
      province_name == "Batangas" & region_dhs == 4 ~ 10,
      province_name == "Cavite"   & region_dhs == 4 ~ 21,
      province_name == "Laguna"   & region_dhs == 4 ~ 34,
      province_name == "Quezon"   & region_dhs == 4 ~ 56,
      province_name == "Rizal"    & region_dhs == 4 ~ 58,
      TRUE ~ province_dhs
    )
  )

# Region 5
dhs_geo <- dhs_geo |>
  # Fix province_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster %in% c(418, 419) ~ "Camarines Norte",
      year == 2008 & cluster == 444           ~ "Sorsogon",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on province_name and region_dhs
  mutate(
    province_dhs = case_when(
      province_name == "Albay"           & region_dhs == 5 ~ 5,
      province_name == "Camarines Norte" & region_dhs == 5 ~ 16,
      province_name == "Camarines Sur"   & region_dhs == 5 ~ 17,
      province_name == "Catanduanes"     & region_dhs == 5 ~ 20,
      province_name %in% c("Masabate", "Masbate") & region_dhs == 5 ~ 41,
      province_name == "Sorsogon"        & region_dhs == 5 ~ 62,
      TRUE ~ province_dhs
    )
  )

# Region 6
dhs_geo <- dhs_geo |>
  # Fix province_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster == 459 ~ "Iloilo",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on province_name and region_dhs
  mutate(
    province_dhs = case_when(
      province_name == "Aklan"                    & region_dhs == 6 ~ 4,
      province_name == "Antique"                  & region_dhs == 6 ~ 6,
      province_name == "Iloilo"                   & region_dhs == 6 ~ 30,
      province_name == "Capiz"                    & region_dhs == 6 ~ 19,
      province_name %in% c("Negros Occidental", "6101 Negros Occidental") & region_dhs == 6 ~ 45,
      province_name == "Guimaras"                 & region_dhs == 6 ~ 79,
      TRUE ~ province_dhs
    )
  )

# Region 7
dhs_geo <- dhs_geo |>
  mutate(
    province_dhs = case_when(
      province_name == "Bohol"                     & region_dhs == 7 ~ 12,
      province_name %in% c("Cebu", "6000 Cebu", "6014 Cebu") & region_dhs == 7 ~ 22,
      province_name %in% c("Negros Oriental", "6219 Negros Oriental") & region_dhs == 7 ~ 46,
      province_name == "Siquijor"                 & region_dhs == 7 ~ 61,
      TRUE ~ province_dhs
    )
  )

# Region 8
dhs_geo <- dhs_geo |>
  # Fix province_name based on cluster and year
  mutate(
    province_name = case_when(
      year == 2008 & cluster == 547 ~ "Leyte",
      year == 2008 & cluster == 561 ~ "Northern Samar",
      province_name == "Northern Samar" & municipality_name == "Calbayog City" ~ "Samar",
      TRUE ~ province_name
    ),
    # Fix municipality_name if needed
    municipality_name = case_when(
      province_name == "Guiuan" & region_dhs == 8 ~ "Guiuan",
      province_name == "Baybay City" & region_dhs == 8 ~ "Baybay City",
      TRUE ~ municipality_name
    )
  ) |>
  # Recode province_dhs based on cleaned province/municipality and region
  mutate(
    province_dhs = case_when(
      province_name == "Eastern Samar" & region_dhs == 8 ~ 26,
      province_name == "Guiuan"        & region_dhs == 8 ~ 26,
      province_name == "Leyte"         & region_dhs == 8 ~ 37,
      province_name == "Baybay City"   & region_dhs == 8 ~ 37,
      province_name == "Northern Samar" & region_dhs == 8 ~ 48,
      province_name == "Samar"         & region_dhs == 8 ~ 60,
      province_name == "Southern Leyte" & region_dhs == 8 ~ 64,
      province_name == "Biliran"       & region_dhs == 8 ~ 78,
      TRUE ~ province_dhs
    )
  )

# Region 9
dhs_geo <- dhs_geo |>
  # Fix municipality_name based on province or cluster
  mutate(
    municipality_name = case_when(
      province_name == "Molave"                         ~ "Molave",
      cluster == 445 & year == 2003                     ~ "Zamboanga",
      cluster == 612 & year == 2008                     ~ "Zamboanga",
      cluster == 609 & year == 2008                     ~ "Buug",
      TRUE ~ municipality_name
    ),
    # Fix province_name based on municipality, cluster, or year
    province_name = case_when(
      province_name == "Molave" & region_dhs == 9                          ~ "Zamboanga Del Sur",
      province_name == "Zamboanga Del Norte" & municipality_name == "Aurora" ~ "Zamboanga Del Sur",
      cluster == 580 & year == 2008                                       ~ "Zamboanga Del Norte",
      year == 2008 & cluster %in% c(587, 589)                             ~ "Zamboanga Del Sur",
      cluster == 609 & year == 2008                                       ~ "Zamboanga Sibugay",
      municipality_name == "Zamboanga"                                    ~ "Zamboanga Del Sur",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on updated province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Zamboanga Del Norte" & region_dhs == 9 ~ 72,
      province_name == "Zamboanga Del Sur"   & region_dhs == 9 ~ 73,
      province_name == "Zamboanga Sibugay"   & region_dhs == 9 ~ 83,
      TRUE ~ province_dhs
    )
  )

# Region 10
dhs_geo <- dhs_geo |>
  # Fix municipality_name based on cluster
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 463 ~ "Pantar",
      year == 2003 & cluster == 475 ~ "Kinoguitan",
      TRUE ~ municipality_name
    )
  ) |>
  # Fix province_name based on cluster, municipality, or year
  mutate(
    province_name = case_when(
      cluster %in% c(629, 630)       & year == 2008 ~ "Lanao Del Norte",
      cluster >= 469 & cluster <= 474 & year == 2003 ~ "Misamis Oriental",
      cluster >= 637 & cluster <= 650 & year == 2008 ~ "Misamis Oriental",
      municipality_name == "Cagayan De Oro" & region_dhs == 10 ~ "Misamis Oriental",
      cluster == 463 & year == 2003  ~ "Lanao Del Norte",
      cluster == 475 & year == 2003  ~ "Misamis Oriental",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on updated province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Bukidnon"           & region_dhs == 10 ~ 13,
      province_name == "Camiguin"           & region_dhs == 10 ~ 18,
      province_name == "Lanao Del Norte"    & region_dhs == 10 ~ 35,
      province_name == "Misamis Occidental" & region_dhs == 10 ~ 42,
      province_name == "Misamis Oriental"   & region_dhs == 10 ~ 43,
      TRUE ~ province_dhs
    )
  )

# Region 11
dhs_geo <- dhs_geo |>
  # Fix province_name based on municipality_name and region
  mutate(
    province_name = case_when(
      municipality_name %in% c("Davao City", "Magsaysay") & 
        province_name == "Davao Del Norte" & region_dhs == 11 ~ "Davao Del Sur",
      
      municipality_name == "Panabo" &
        province_name == "Davao Del Sur" & region_dhs == 11 ~ "Davao Del Norte",
      
      TRUE ~ province_name
    )
  ) |>
  # Assign province_dhs codes based on updated province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Davao Del Norte"     & region_dhs == 11 ~ 23,
      province_name == "Compostela Valley"   & region_dhs == 11 ~ 82,
      province_name == "Davao Del Sur"       & region_dhs == 11 ~ 24,
      province_name == "Davao Oriental"      & region_dhs == 11 ~ 25,
      TRUE ~ province_dhs
    )
  )

# Region 12
dhs_geo <- dhs_geo |>
  # Fix province_name based on cluster and year
  mutate(
    province_name = case_when(
      year == 2008 & cluster %in% c(693, 700) ~ "Cotabato",
      year == 2003 & cluster %in% c(774, 775) ~ "Cotabato",
      TRUE ~ province_name
    )
  ) |>
  # 2. Recode province_dhs based on updated province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Cotabato"         & region_dhs == 12 ~ 47,
      province_name == "South Cotabato"   & region_dhs == 12 ~ 63,
      province_name %in% c("Sultan Kudarat", "Sultan Kadarat") & region_dhs == 12 ~ 65,
      province_name == "Sarangani"        & region_dhs == 12 ~ 80,
      TRUE ~ province_dhs
    )
  )

# First District
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2003 & cluster >= 564 & cluster <= 581 ~ "Metro Manila",
      year == 2003 & cluster %in% c(642, 643)        ~ "Metro Manila",
      year == 2008 & cluster == 8                    ~ "Metro Manila",
      TRUE ~ province_name
    ),
    
    municipality_name = case_when(
      year == 2003 & cluster >= 564 & cluster <= 581 ~ "Manila",
      year == 2003 & cluster == 643                  ~ "Manila",
      year == 2008 & cluster == 8                    ~ "Manila",
      TRUE ~ municipality_name
    )
  ) |>
  # Recode province_dhs for Manila
  mutate(
    province_dhs = case_when(
      municipality_name == "Manila" & province_name == "Metro Manila" & region_dhs == 13 ~ 39,
      year == 2008 & cluster == 8 ~ 39,
      TRUE ~ province_dhs
    )
  )

# Second District
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on cluster and year
  mutate(
    province_name = case_when(
      year == 2003 & cluster %in% 582:624            ~ "Metro Manila",
      year == 2003 & cluster %in% 625:626            ~ "Metro Manila",
      year == 2003 & cluster == 630                  ~ "Metro Manila",
      year == 2008 & cluster %in% c(28, 34, 41)      ~ "Metro Manila",
      year == 2008 & cluster %in% 80:86              ~ "Metro Manila",
      year == 2008 & cluster %in% 120:122            ~ "Metro Manila",
      TRUE ~ province_name
    ),
    municipality_name = case_when(
      year == 2003 & cluster %in% 582:624            ~ "Quezon City",
      year == 2003 & cluster %in% 625:626            ~ "Mandaluyong",
      year == 2003 & cluster == 630                  ~ "Marikina",
      year == 2003 & cluster == 642                  ~ "Pasig",
      year == 2008 & cluster %in% c(28, 34, 41)      ~ "Quezon City",
      year == 2008 & cluster %in% 80:86              ~ "Quezon City",
      year == 2008 & cluster %in% c(61, 63, 64)      ~ "Pasig",
      year == 2008 & cluster %in% 120:122            ~ "Las Pinas",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs = 74 for Second District cities
  mutate(
    province_dhs = case_when(
      municipality_name %in% c("Mandaluyong", "Marikina", "Pasig", "Quezon City", "San Juan") & region_dhs == 13 ~ 74,
      year == 2008 & cluster %in% c(28, 34, 41) ~ 74,
      TRUE ~ province_dhs
    )
  )


# Third District
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on cluster and year
  mutate(
    province_name = case_when(
      year == 2008 & cluster %in% 98:104        ~ "Metro Manila",
      year == 2003 & cluster %in% 674:676       ~ "Metro Manila",
      year == 2003 & cluster == 662             ~ "Metro Manila",
      TRUE ~ province_name
    ),
    municipality_name = case_when(
      year == 2008 & cluster %in% 98:104        ~ "Valenzuela",
      year == 2003 & cluster %in% 674:676       ~ "Valenzuela",
      year == 2008 & cluster == 88              ~ "Malabon",
      year == 2003 & cluster == 662             ~ "Malabon",
      year == 2003 & cluster == 706             ~ "Muntinlupa",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs = 75 for Third District cities
  mutate(
    province_dhs = case_when(
      municipality_name %in% c("Caloocan", "Malabon", "City of Malabon", "Navotas", "City of Navotas", "Valenzuela") & region_dhs == 13 ~ 75,
      TRUE ~ province_dhs
    )
  )

# Fourth District
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on cluster and year
  mutate(
    province_name = case_when(
      year == 2008 & cluster %in% 125:140 ~ "Metro Manila",
      year == 2008 & cluster %in% 105:115 ~ "Metro Manila",
      TRUE ~ province_name
    ),
    
    municipality_name = case_when(
      year == 2008 & cluster %in% 125:140 ~ "Parañaque",
      year == 2008 & cluster %in% 105:111 ~ "Makati",
      year == 2008 & cluster == 115      ~ "Taguig",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs = 76 for Fourth District cities
  mutate(
    province_dhs = case_when(
      municipality_name %in% c("Las Pinas", "Makati", "Muntinlupa", "Paranaque", "Parañaque", "Pasay", "Pateros", "Taguig") & region_dhs == 13 ~ 76,
      TRUE ~ province_dhs
    )
  )

# Region 14 (CAR)
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster == 160 ~ "Ifugao",
      TRUE ~ province_name
    ),
    
    municipality_name = case_when(
      year == 2008 & cluster == 160                          ~ "Lagawe",
      year == 2003 & cluster %in% c(736, 738)                ~ "La Trinidad",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs codes for CAR provinces
  mutate(
    province_dhs = case_when(
      province_name == "Abra"               & region_dhs == 14 ~ 1,
      province_name == "Benguet"            & region_dhs == 14 ~ 11,
      province_name == "Ifugao"             & region_dhs == 14 ~ 27,
      province_name == "Kalinga"            & region_dhs == 14 ~ 32,
      province_name == "Mountain Province"  & region_dhs == 14 ~ 44,
      province_name == "Apayao"             & region_dhs == 14 ~ 81,
      TRUE ~ province_dhs
    )
  )

# Region 15 (ARMM)
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2003 & cluster == 761                ~ "Lanao Del Sur",
      year == 2003 & cluster == 787                ~ "Tawi-Tawi",
      year == 2008 & cluster == 733                ~ "Lanao Del Sur",
      province_name == "Cotabato" & municipality_name == "Wao" ~ "Lanao Del Sur",
      TRUE ~ province_name
    ),
    
    municipality_name = case_when(
      year == 2003 & cluster == 787 ~ "Sapa-Sapa",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs codes for ARMM provinces
  mutate(
    province_dhs = case_when(
      province_name == "Basilan"         & region_dhs == 15 ~ 7,
      province_name == "Lanao Del Sur"   & region_dhs %in% c(10, 15) ~ 36,
      province_name == "Cotabato"        & region_dhs == 15 ~ 36,
      province_name == "Maguindanao"     & region_dhs == 15 ~ 38,
      province_name == "Sulu"            & region_dhs == 15 ~ 66,
      province_name == "Tawi-Tawi"       & region_dhs == 15 ~ 70,
      TRUE ~ province_dhs
    )
  )

# Region 16 (CARAGA)
dhs_geo <- dhs_geo |>
  # Fix province_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster %in% c(780, 786) ~ "Dinagat Islands",
      TRUE ~ province_name
    )
  ) |>
  # Recode province_dhs based on updated province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Agusan Del Norte"      & region_dhs == 16 ~ 2,
      province_name == "Agusan Del Sur"        & region_dhs == 16 ~ 3,
      province_name == "Surigao Del Norte"     & region_dhs == 16 ~ 67,
      province_name == "Surigao Del Sur"       & region_dhs == 16 ~ 68,
      province_name == "Dinagat Islands"       & region_dhs == 16 ~ 85,
      TRUE ~ province_dhs
    )
  )

# Region 17 (MIMAROPA)
dhs_geo <- dhs_geo |>
  # Fix province_name and municipality_name based on year and cluster
  mutate(
    province_name = case_when(
      year == 2008 & cluster == 386 ~ "Oriental Mindoro",
      region_dhs == 17 & municipality_name %in% c(
        "Baco", "Bansud", "Bongabong", "Roxas", "Pola", "Victoria",
        "Pinamalayan", "Bulalacao", "Calapan", "Gloria", "Puerto Galera"
      ) ~ "Oriental Mindoro",
      TRUE ~ province_name
    ),
    municipality_name = case_when(
      year == 2008 & cluster == 386 ~ "Baco",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign province_dhs based on province_name and region
  mutate(
    province_dhs = case_when(
      province_name == "Marinduque"          & region_dhs == 17 ~ 40,
      province_name == "Occidental Mindoro"  & region_dhs == 17 ~ 51,
      province_name == "Palawan"             & region_dhs == 17 ~ 53,
      province_name == "Romblon"             & region_dhs == 17 ~ 59,
      province_name == "Oriental Mindoro"    & region_dhs == 17 ~ 52,
      TRUE ~ province_dhs
    )
  )

# 11. City/Municipality Variable ===============================================
# Region 1
## Ilocos Norte
dhs_geo <- dhs_geo |>
  # Fix misspelled municipality_name if needed
  mutate(
    municipality_name = case_when(
      municipality_name == "" & region_dhs == 1 & province_dhs == 28 & cluster == 172 & year == 2008 ~ "Bacarra",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign numeric municipality codes
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bucarra"  & region_dhs == 1 & province_dhs == 28 ~ 2,
      municipality_name == "Bacarra"  & region_dhs == 1 & province_dhs == 28 ~ 2,
      municipality_name == "Burgos"   & region_dhs == 1 & province_dhs == 28 ~ 6,
      municipality_name == "Pinili"   & region_dhs == 1 & province_dhs == 28 ~ 19,
      municipality_name == "Dingras"  & region_dhs == 1 & province_dhs == 28 ~ 9,
      municipality_name == "Batac"    & region_dhs == 1 & province_dhs == 28 ~ 5,
      municipality_name == "Marcos"   & region_dhs == 1 & province_dhs == 28 ~ 13,
      municipality_name == "Pagudpud" & region_dhs == 1 & province_dhs == 28 ~ 15,
      municipality_name == "Paoay"    & region_dhs == 1 & province_dhs == 28 ~ 16,
      municipality_name == "Pasuquin" & region_dhs == 1 & province_dhs == 28 ~ 17,
      TRUE ~ municipality_dhs
    )
  )

## Ilocos Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bantay"           & region_dhs == 1 & province_dhs == 29 ~ 3,
      municipality_name == "Candon"           & region_dhs == 1 & province_dhs == 29 ~ 6,
      municipality_name == "Quirino"          & region_dhs == 1 & province_dhs == 29 ~ 15,
      municipality_name == "Salcedo"          & region_dhs == 1 & province_dhs == 29 ~ 16,
      municipality_name == "Santa Catalina"   & region_dhs == 1 & province_dhs == 29 ~ 23,
      municipality_name == "Narvacan"         & region_dhs == 1 & province_dhs == 29 ~ 14,
      municipality_name == "San Juan"         & region_dhs == 1 & province_dhs == 29 ~ 20,
      municipality_name == "Sinait"           & region_dhs == 1 & province_dhs == 29 ~ 30,
      TRUE ~ municipality_dhs  
    )
  )

## La Union
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bacnotan"     & region_dhs == 1 & province_dhs == 33 ~ 3,
      municipality_name == "Bangar"       & region_dhs == 1 & province_dhs == 33 ~ 6,
      municipality_name == "Bauang"       & region_dhs == 1 & province_dhs == 33 ~ 7,
      municipality_name == "Luna"         & region_dhs == 1 & province_dhs == 33 ~ 10,
      municipality_name == "Naguilian"    & region_dhs == 1 & province_dhs == 33 ~ 11,
      municipality_name == "San Fernando" & region_dhs == 1 & province_dhs == 33 ~ 14,
      municipality_name == "Sudipen"      & region_dhs == 1 & province_dhs == 33 ~ 19,
      municipality_name == "Tubao"        & region_dhs == 1 & province_dhs == 33 ~ 20,
      TRUE ~ municipality_dhs
    )
  )

## Pangasinan
dhs_geo <- dhs_geo |>
  # Fix blank municipality_name values
  mutate(
    municipality_name = case_when(
      municipality_name == "" & region_dhs == 1 & province_dhs == 55 & cluster == 193 & year == 2008 ~ "Calasiao",
      municipality_name == "" & region_dhs == 1 & province_dhs == 55 & cluster == 207 & year == 2008 ~ "Alcala",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign municipality_dhs codes
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Aguilar"           & region_dhs == 1 & province_dhs == 55 ~ 2,
      municipality_name == "Alaminos"          & region_dhs == 1 & province_dhs == 55 ~ 3,
      municipality_name == "Asingan"           & region_dhs == 1 & province_dhs == 55 ~ 6,
      municipality_name == "Bautista"          & region_dhs == 1 & province_dhs == 55 ~ 10,
      municipality_name == "Bayambang"         & region_dhs == 1 & province_dhs == 55 ~ 11,
      municipality_name == "Binalonan"         & region_dhs == 1 & province_dhs == 55 ~ 12,
      municipality_name == "Binmaley"          & region_dhs == 1 & province_dhs == 55 ~ 13,
      municipality_name == "Bolinao"           & region_dhs == 1 & province_dhs == 55 ~ 14,
      municipality_name == "Calasiao"          & region_dhs == 1 & province_dhs == 55 ~ 17,
      municipality_name == "Dagupan"           & region_dhs == 1 & province_dhs == 55 ~ 18,
      municipality_name == "Dasol"             & region_dhs == 1 & province_dhs == 55 ~ 19,
      municipality_name == "Infanta"           & region_dhs == 1 & province_dhs == 55 ~ 20,
      municipality_name == "Malasiqui"         & region_dhs == 1 & province_dhs == 55 ~ 24,
      municipality_name == "Manaoag"           & region_dhs == 1 & province_dhs == 55 ~ 25,
      municipality_name == "Mangaldan"         & region_dhs == 1 & province_dhs == 55 ~ 26,
      municipality_name == "Mapandan"          & region_dhs == 1 & province_dhs == 55 ~ 28,
      municipality_name %in% c("Pozorrubio", "Posorrubio", "Pozzorubio") & region_dhs == 1 & province_dhs == 55 ~ 30,
      municipality_name == "Rosales"           & region_dhs == 1 & province_dhs == 55 ~ 31,
      municipality_name == "San Carlos City"   & region_dhs == 1 & province_dhs == 55 ~ 32,
      municipality_name == "San Fabian"        & region_dhs == 1 & province_dhs == 55 ~ 33,
      municipality_name == "San Jacinto"       & region_dhs == 1 & province_dhs == 55 ~ 34,
      municipality_name == "San Manuel"        & region_dhs == 1 & province_dhs == 55 ~ 35,
      municipality_name == "San Nicolas"       & region_dhs == 1 & province_dhs == 55 ~ 36,
      municipality_name == "Santa Barbara"     & region_dhs == 1 & province_dhs == 55 ~ 39,
      municipality_name == "Tayug"             & region_dhs == 1 & province_dhs == 55 ~ 43,
      municipality_name == "Urdaneta City"     & region_dhs == 1 & province_dhs == 55 ~ 46,
      municipality_name == "Alcala"            & region_dhs == 1 & province_dhs == 55 ~ 4,
      TRUE ~ municipality_dhs
    )
  )

# Region 2
## Batanes
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Itbayat" & region_dhs == 2 & province_dhs == 9 ~ 2,
      TRUE ~ municipality_dhs
    )
  )

## Cagayan
dhs_geo <- dhs_geo |>
  # Fix municipality_name based on cluster ID
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster %in% c(63, 64) ~ "San Mateo",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign municipality_dhs codes for Cagayan (province_dhs = 15)
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Abulug"        & region_dhs == 2 & province_dhs == 15 ~ 1,
      municipality_name == "Alcala"        & region_dhs == 2 & province_dhs == 15 ~ 2,
      municipality_name == "Allacapan"     & region_dhs == 2 & province_dhs == 15 ~ 3,
      municipality_name == "Amulung"       & region_dhs == 2 & province_dhs == 15 ~ 4,
      municipality_name == "Baggao"        & region_dhs == 2 & province_dhs == 15 ~ 6,
      municipality_name == "Camalaniugan"  & region_dhs == 2 & province_dhs == 15 ~ 10,
      municipality_name == "Claveria"      & region_dhs == 2 & province_dhs == 15 ~ 11,
      municipality_name == "Enrile"        & region_dhs == 2 & province_dhs == 15 ~ 12,
      municipality_name == "Gattaran"      & region_dhs == 2 & province_dhs == 15 ~ 13,
      municipality_name == "Gonzaga"       & region_dhs == 2 & province_dhs == 15 ~ 14,
      municipality_name == "Iguig"         & region_dhs == 2 & province_dhs == 15 ~ 15,
      municipality_name == "Lal-Lo"        & region_dhs == 2 & province_dhs == 15 ~ 16,
      municipality_name == "Pamplona"      & region_dhs == 2 & province_dhs == 15 ~ 18,
      municipality_name == "Sanchez Mira"  & region_dhs == 2 & province_dhs == 15 ~ 22,
      municipality_name == "Santo Niño"    & region_dhs == 2 & province_dhs == 15 ~ 26,
      municipality_name == "Solana"        & region_dhs == 2 & province_dhs == 15 ~ 27,
      municipality_name == "Tuguegarao"    & region_dhs == 2 & province_dhs == 15 ~ 29,
      TRUE ~ municipality_dhs
    )
  )

## Isabela
dhs_geo <- dhs_geo |>
  # Fix municipality_name from cluster
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 231 ~ "San Manuel",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign municipality_dhs codes for Isabela
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Alicia"           & region_dhs == 2 & province_dhs == 31 ~ 1,
      municipality_name == "Benito Soliven"   & region_dhs == 2 & province_dhs == 31 ~ 4,
      municipality_name == "Burgos"           & region_dhs == 2 & province_dhs == 31 ~ 5,
      municipality_name == "Cabatúan"         & region_dhs == 2 & province_dhs == 31 ~ 7,
      municipality_name == "Cauayan City"     & region_dhs == 2 & province_dhs == 31 ~ 8,
      municipality_name == "Cordon"           & region_dhs == 2 & province_dhs == 31 ~ 9,
      municipality_name == "Echague"          & region_dhs == 2 & province_dhs == 31 ~ 12,
      municipality_name == "Ilagan City"      & region_dhs == 2 & province_dhs == 31 ~ 14,
      municipality_name == "Jones"            & region_dhs == 2 & province_dhs == 31 ~ 15,
      municipality_name == "Mallig"           & region_dhs == 2 & province_dhs == 31 ~ 19,
      municipality_name == "Quezon"           & region_dhs == 2 & province_dhs == 31 ~ 22,
      municipality_name == "Reina Mercedes"   & region_dhs == 2 & province_dhs == 31 ~ 25,
      municipality_name == "Roxas"            & region_dhs == 2 & province_dhs == 31 ~ 26,
      municipality_name == "San Mariano"      & region_dhs == 2 & province_dhs == 31 ~ 31,
      municipality_name == "San Manuel"       & region_dhs == 2 & province_dhs == 31 ~ 30,
      municipality_name == "San Mateo"        & region_dhs == 2 & province_dhs == 31 ~ 32,
      municipality_name == "Santa Maria"      & region_dhs == 2 & province_dhs == 31 ~ 34,
      municipality_name == "Santiago"         & region_dhs == 2 & province_dhs == 31 ~ 35,
      TRUE ~ municipality_dhs
    )
  )

## Nueva Vizcaya
dhs_geo <- dhs_geo |>
  # Fix municipality_name from cluster
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 235 ~ "Bagabag",
      TRUE ~ municipality_name
    )
  ) |>
  # Assign municipality_dhs codes for Nueva Vizcaya
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bagabag"  & region_dhs == 2 & province_dhs == 50 ~ 3,
      municipality_name == "Bayombong" & region_dhs == 2 & province_dhs == 50 ~ 5,
      municipality_name == "Diadi"     & region_dhs == 2 & province_dhs == 50 ~ 6,
      municipality_name == "Solano"    & region_dhs == 2 & province_dhs == 50 ~ 13,
      municipality_name == "Bambang"   & region_dhs == 2 & province_dhs == 50 ~ 4,
      municipality_name == "Quezon"    & region_dhs == 2 & province_dhs == 50 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Quirino
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Diffun"        & region_dhs == 2 & province_dhs == 57 ~ 3,
      municipality_name == "Maddela"       & region_dhs == 2 & province_dhs == 57 ~ 4,
      municipality_name == "Cabarroguis"   & region_dhs == 2 & province_dhs == 57 ~ 2,
      TRUE ~ municipality_dhs
    )
  )

# Region 3
## Bataan
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Dinalupihan" & region_dhs == 3 & province_dhs == 8 ~ 4,
      municipality_name == "Mariveles"   & region_dhs == 3 & province_dhs == 8 ~ 7,
      municipality_name == "Abucay"      & region_dhs == 3 & province_dhs == 8 ~ 1,
      municipality_name == "Orion"       & region_dhs == 3 & province_dhs == 8 ~ 10,
      municipality_name == "Morong"      & region_dhs == 3 & province_dhs == 8 ~ 8,
      TRUE ~ municipality_dhs
    )
  )

## Bulacan
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "" & cluster == 249 & year == 2008 ~ "Malolos",
      year == 2008 & cluster %in% c(251, 252, 253, 254) ~ "Marilao",
      year == 2003 & cluster == 82 ~ "Meycauayan",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Bustos"      & region_dhs == 3 & province_dhs == 14 ~ 6,
      municipality_name == "Guiguinto"   & region_dhs == 3 & province_dhs == 14 ~ 8,
      municipality_name == "Malolos"     & region_dhs == 3 & province_dhs == 14 ~ 10,
      municipality_name == "Hagonoy"     & region_dhs == 3 & province_dhs == 14 ~ 9,
      municipality_name == "Marilao"     & region_dhs == 3 & province_dhs == 14 ~ 11,
      municipality_name == "Meycauayan"  & region_dhs == 3 & province_dhs == 14 ~ 12,
      municipality_name == "Noragaray"   & region_dhs == 3 & province_dhs == 14 ~ 13,
      municipality_name == "Norzagaray"  & region_dhs == 3 & province_dhs == 14 ~ 13,  # handles spelling variant
      municipality_name == "Pulilan"     & region_dhs == 3 & province_dhs == 14 ~ 18,
      municipality_name == "San Jose Del Monte City" & region_dhs == 3 & province_dhs == 14 ~ 20,
      municipality_name == "Santa Maria" & region_dhs == 3 & province_dhs == 14 ~ 23,
      municipality_name == "Pandi"       & region_dhs == 3 & province_dhs == 14 ~ 15,
      municipality_name == "San Miguel"  & region_dhs == 3 & province_dhs == 14 ~ 21,
      municipality_name == "Bulacan"     & region_dhs == 3 & province_dhs == 14 ~ 5,
      municipality_name == "Calumpit"    & region_dhs == 3 & province_dhs == 14 ~ 7,
      municipality_name == "Obando"      & region_dhs == 3 & province_dhs == 14 ~ 14,
      municipality_name == "Obondo"      & region_dhs == 3 & province_dhs == 14 ~ 14,  # possible typo/spelling variant
      TRUE ~ municipality_dhs
    )
  )

## Nueva Ecija
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bongabon"                      & region_dhs == 3 & province_dhs == 49 ~ 2,
      municipality_name == "Cabanatuan City"              & region_dhs == 3 & province_dhs == 49 ~ 3,
      municipality_name == "Cabiao"                       & region_dhs == 3 & province_dhs == 49 ~ 4,
      municipality_name == "Carranglan"                   & region_dhs == 3 & province_dhs == 49 ~ 5,
      municipality_name == "Cuyapo"                       & region_dhs == 3 & province_dhs == 49 ~ 6,
      municipality_name == "Gabaldon"                     & region_dhs == 3 & province_dhs == 49 ~ 7,
      municipality_name == "Gapan City"                   & region_dhs == 3 & province_dhs == 49 ~ 8,
      municipality_name == "General Mamerto Natividad"    & region_dhs == 3 & province_dhs == 49 ~ 9,
      municipality_name == "Jaen"                         & region_dhs == 3 & province_dhs == 49 ~ 12,
      municipality_name == "Palayan City"                 & region_dhs == 3 & province_dhs == 49 ~ 19,
      municipality_name == "Peñaranda"                    & region_dhs == 3 & province_dhs == 49 ~ 21,
      municipality_name == "San Isidro"                   & region_dhs == 3 & province_dhs == 49 ~ 25,
      municipality_name == "San Jose City"                & region_dhs == 3 & province_dhs == 49 ~ 26,
      municipality_name == "San Leonardo"                 & region_dhs == 3 & province_dhs == 49 ~ 27,
      municipality_name == "Talavera"                     & region_dhs == 3 & province_dhs == 49 ~ 30,
      municipality_name == "Science City of Munoz"        & region_dhs == 3 & province_dhs == 49 ~ 17,
      municipality_name == "Aliaga"                       & region_dhs == 3 & province_dhs == 49 ~ 1,
      TRUE ~ municipality_dhs
    )
  )

## Pampanga
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "" & cluster == 109 & year == 2003 ~ "Apalit",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Angeles"       & region_dhs == 3 & province_dhs == 54 ~ 1,
      municipality_name == "Apalit"        & region_dhs == 3 & province_dhs == 54 ~ 2,
      municipality_name == "Floridablanca" & region_dhs == 3 & province_dhs == 54 ~ 6,
      municipality_name == "Guagua"        & region_dhs == 3 & province_dhs == 54 ~ 7,
      municipality_name == "Lubao"         & region_dhs == 3 & province_dhs == 54 ~ 8,
      municipality_name == "Mabalacat"     & region_dhs == 3 & province_dhs == 54 ~ 9,
      municipality_name == "Macabebe"      & region_dhs == 3 & province_dhs == 54 ~ 10,
      municipality_name == "Masantol"      & region_dhs == 3 & province_dhs == 54 ~ 12,
      municipality_name == "Mexico"        & region_dhs == 3 & province_dhs == 54 ~ 13,
      municipality_name == "Minalin"       & region_dhs == 3 & province_dhs == 54 ~ 14,
      municipality_name == "San Fernando"  & region_dhs == 3 & province_dhs == 54 ~ 16,
      municipality_name == "San Luis"      & region_dhs == 3 & province_dhs == 54 ~ 17,
      municipality_name == "San Simon"     & region_dhs == 3 & province_dhs == 54 ~ 18,
      municipality_name == "Sasmuan"       & region_dhs == 3 & province_dhs == 54 ~ 22,
      TRUE ~ municipality_dhs
    )
  )

## Tarlac
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "" & cluster == 293 & year == 2008 ~ "Tarlac City",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Anao"         & region_dhs == 3 & province_dhs == 69 ~ 1,
      municipality_name == "Camiling"     & region_dhs == 3 & province_dhs == 69 ~ 3,
      municipality_name == "Capas"        & region_dhs == 3 & province_dhs == 69 ~ 4,
      municipality_name == "Concepcion"   & region_dhs == 3 & province_dhs == 69 ~ 5,
      municipality_name == "La Paz"       & region_dhs == 3 & province_dhs == 69 ~ 7,
      municipality_name == "Moncado"      & region_dhs == 3 & province_dhs == 69 ~ 9,
      municipality_name == "Moncada"      & region_dhs == 3 & province_dhs == 69 ~ 9,
      municipality_name == "Pura"         & region_dhs == 3 & province_dhs == 69 ~ 11,
      municipality_name == "Tarlac City"  & region_dhs == 3 & province_dhs == 69 ~ 16,
      TRUE ~ municipality_dhs
    )
  )

## Zambales
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "" & cluster == 298 & year == 2008 ~ "Santa Cruz",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Botolan"     & region_dhs == 3 & province_dhs == 71 ~ 1,
      municipality_name == "Cabangan"    & region_dhs == 3 & province_dhs == 71 ~ 2,
      municipality_name == "Olongapo"    & region_dhs == 3 & province_dhs == 71 ~ 7,
      municipality_name == "San Felipe"  & region_dhs == 3 & province_dhs == 71 ~ 10,
      municipality_name == "Santa Cruz"  & region_dhs == 3 & province_dhs == 71 ~ 13,
      municipality_name == "Subic"       & region_dhs == 3 & province_dhs == 71 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Aurora
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Maria Aurora" & region_dhs == 3 & province_dhs == 77 ~ 7,
      TRUE ~ municipality_dhs
    )
  )

# Region 4
## Batangas
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "San Nicolas" & region_dhs == 4 & province_dhs == 10 ~ 25,
      municipality_name == "Balayan" & region_dhs == 4 & province_dhs == 10 ~ 3,
      municipality_name == "Batangas" & region_dhs == 4 & province_dhs == 10 ~ 5,
      municipality_name == "Bauan" & region_dhs == 4 & province_dhs == 10 ~ 6,
      municipality_name == "Ibaan" & region_dhs == 4 & province_dhs == 10 ~ 10,
      municipality_name == "Laurel" & region_dhs == 4 & province_dhs == 10 ~ 11,
      municipality_name == "Lipa" & region_dhs == 4 & province_dhs == 10 ~ 14,
      municipality_name == "Lobo" & region_dhs == 4 & province_dhs == 10 ~ 15,
      municipality_name == "Malvar" & region_dhs == 4 & province_dhs == 10 ~ 17,
      municipality_name == "San Juan" & region_dhs == 4 & province_dhs == 10 ~ 23,
      municipality_name == "Talisay" & region_dhs == 4 & province_dhs == 10 ~ 30,
      municipality_name == "Tanauan" & region_dhs == 4 & province_dhs == 10 ~ 31,
      municipality_name == "Agoncillo" & region_dhs == 4 & province_dhs == 10 ~ 1,
      municipality_name == "Lemery" & region_dhs == 4 & province_dhs == 10 ~ 12,
      municipality_name == "San Pascual" & region_dhs == 4 & province_dhs == 10 ~ 26,
      TRUE ~ municipality_dhs
    )
  )

## Cavite
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster >= 311 & cluster <= 316 ~ "Bacoor",
      year == 2008 & cluster == 328 ~ "Silang",
      year == 2008 & cluster == 325 ~ "Maragondon",
      municipality_name == "Naite" & region_dhs == 4 & province_dhs == 21 ~ "Naic",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Bacoor" & region_dhs == 4 & province_dhs == 21 ~ 3,
      municipality_name == "Dasmarinas" & region_dhs == 4 & province_dhs == 21 ~ 6,
      municipality_name == "General Trias" & region_dhs == 4 & province_dhs == 21 ~ 8,
      municipality_name == "Tagaytay" & region_dhs == 4 & province_dhs == 21 ~ 19,
      municipality_name == "Tanza" & region_dhs == 4 & province_dhs == 21 ~ 20,
      municipality_name == "Carmona" & region_dhs == 4 & province_dhs == 21 ~ 4,
      municipality_name == "Cavite City" & region_dhs == 4 & province_dhs == 21 ~ 5,
      municipality_name == "Dasmariñas" & region_dhs == 4 & province_dhs == 21 ~ 6,
      municipality_name == "Imus" & region_dhs == 4 & province_dhs == 21 ~ 9,
      municipality_name == "Kawit" & region_dhs == 4 & province_dhs == 21 ~ 11,
      municipality_name == "Rosario" & region_dhs == 4 & province_dhs == 21 ~ 17,
      municipality_name == "Silang" & region_dhs == 4 & province_dhs == 21 ~ 18,
      municipality_name == "Maragondon" & region_dhs == 4 & province_dhs == 21 ~ 13,
      municipality_name == "Naic" & region_dhs == 4 & province_dhs == 21 ~ 15,
      TRUE ~ municipality_dhs
    )
  )

## Laguna
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 344 ~ "San Pedro",
      municipality_name == "NAITE" & region_dhs == 4 & province_dhs == 34 ~ "Naic",  # adjust if needed
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Alaminos"     & region_dhs == 4 & province_dhs == 34 ~ 1,
      municipality_name == "Biñan"        & region_dhs == 4 & province_dhs == 34 ~ 3,
      municipality_name == "Calamba"      & region_dhs == 4 & province_dhs == 34 ~ 5,
      municipality_name == "Lumban"       & region_dhs == 4 & province_dhs == 34 ~ 13,
      municipality_name == "Magdalena"    & region_dhs == 4 & province_dhs == 34 ~ 15,
      municipality_name == "Majayjay"     & region_dhs == 4 & province_dhs == 34 ~ 16,
      municipality_name == "Nagcarlan"    & region_dhs == 4 & province_dhs == 34 ~ 17,
      municipality_name == "Paete"        & region_dhs == 4 & province_dhs == 34 ~ 18,
      municipality_name == "Pagsanjan"    & region_dhs == 4 & province_dhs == 34 ~ 19,
      municipality_name == "Pakil"        & region_dhs == 4 & province_dhs == 34 ~ 20,
      municipality_name == "Pila"         & region_dhs == 4 & province_dhs == 34 ~ 22,
      municipality_name == "Rizal"        & region_dhs == 4 & province_dhs == 34 ~ 23,
      municipality_name == "San Pablo City" & region_dhs == 4 & province_dhs == 34 ~ 24,
      municipality_name == "San Pedro"    & region_dhs == 4 & province_dhs == 34 ~ 25,
      municipality_name == "Santa Cruz"   & region_dhs == 4 & province_dhs == 34 ~ 26,
      municipality_name == "Santa Rosa"   & region_dhs == 4 & province_dhs == 34 ~ 28,
      TRUE ~ municipality_dhs
    )
  )

## Quezon
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 355 ~ "Calauag",
      year == 2008 & cluster == 348 ~ "Tagkawayan",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Atimonan"      & region_dhs == 4 & province_dhs == 56 ~ 3,
      municipality_name == "Candelaria"    & region_dhs == 4 & province_dhs == 56 ~ 8,
      municipality_name == "Catanuan"      & region_dhs == 4 & province_dhs == 56 ~ 10,
      municipality_name == "Gumaca"        & region_dhs == 4 & province_dhs == 56 ~ 19,
      municipality_name == "Lucena"        & region_dhs == 4 & province_dhs == 56 ~ 24,
      municipality_name == "Macalelon"     & region_dhs == 4 & province_dhs == 56 ~ 25,
      municipality_name == "Pagbilao"      & region_dhs == 4 & province_dhs == 56 ~ 30,
      municipality_name == "Pitogo"        & region_dhs == 4 & province_dhs == 56 ~ 34,
      municipality_name == "Real"          & region_dhs == 4 & province_dhs == 56 ~ 38,
      municipality_name == "San Antonio"   & region_dhs == 4 & province_dhs == 56 ~ 41,
      municipality_name == "San Francisco" & region_dhs == 4 & province_dhs == 56 ~ 42,
      municipality_name == "Sariaya"       & region_dhs == 4 & province_dhs == 56 ~ 45,
      municipality_name == "Tagkawayan"    & region_dhs == 4 & province_dhs == 56 ~ 46,
      municipality_name == "Tiaong"        & region_dhs == 4 & province_dhs == 56 ~ 48,
      municipality_name == "Calauag"       & region_dhs == 4 & province_dhs == 56 ~ 7,
      TRUE ~ municipality_dhs
    )
  )

## Rizal
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 366 ~ "Antipolo",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Antipolo"    & region_dhs == 4 & province_dhs == 58 ~ 2,
      municipality_name == "Binangonan"  & region_dhs == 4 & province_dhs == 58 ~ 4,
      municipality_name == "Cainta"      & region_dhs == 4 & province_dhs == 58 ~ 5,
      municipality_name == "Cardona"     & region_dhs == 4 & province_dhs == 58 ~ 6,
      municipality_name == "Jala-Jala"   & region_dhs == 4 & province_dhs == 58 ~ 7,
      municipality_name == "Rodriguez"   & region_dhs == 4 & province_dhs == 58 ~ 8,
      municipality_name == "Morong"      & region_dhs == 4 & province_dhs == 58 ~ 9,
      municipality_name == "Pililla"     & region_dhs == 4 & province_dhs == 58 ~ 10,
      municipality_name == "San Mateo"   & region_dhs == 4 & province_dhs == 58 ~ 11,
      municipality_name == "Tanay"       & region_dhs == 4 & province_dhs == 58 ~ 12,
      municipality_name == "Taytay"      & region_dhs == 4 & province_dhs == 58 ~ 13,
      TRUE ~ municipality_dhs
    )
  )

# Region 5
## Albay
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 270 ~ "Palanas",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Bacacay"        & region_dhs == 5 & province_dhs == 5  ~ 1,
      municipality_name == "Camalig"        & region_dhs == 5 & province_dhs == 5  ~ 2,
      municipality_name == "Daraga"         & region_dhs == 5 & province_dhs == 5  ~ 3,
      municipality_name == "Guinobatan"     & region_dhs == 5 & province_dhs == 5  ~ 4,
      municipality_name == "Legazpi City"   & region_dhs == 5 & province_dhs == 5  ~ 6,
      municipality_name == "Libon"          & region_dhs == 5 & province_dhs == 5  ~ 7,
      municipality_name == "Ligao"          & region_dhs == 5 & province_dhs == 5  ~ 8,
      municipality_name == "Manito"         & region_dhs == 5 & province_dhs == 5  ~ 11,
      municipality_name == "Oas"            & region_dhs == 5 & province_dhs == 5  ~ 12,
      municipality_name == "Santo Domingo"  & region_dhs == 5 & province_dhs == 5  ~ 16,
      municipality_name == "Tiwi"           & region_dhs == 5 & province_dhs == 5  ~ 18,
      TRUE ~ municipality_dhs
    )
  )

## Camarines Norte
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 419 ~ "Vinzons",
      year == 2008 & cluster == 418 ~ "Santa Elena",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Basud"             & region_dhs == 5 & province_dhs == 16 ~ 1,
      municipality_name == "Daet"              & region_dhs == 5 & province_dhs == 16 ~ 3,
      municipality_name == "San Lorenzo Ruiz"  & region_dhs == 5 & province_dhs == 16 ~ 4,
      municipality_name == "Jose Panganiban"   & region_dhs == 5 & province_dhs == 16 ~ 5,
      municipality_name == "Santa Elena"       & region_dhs == 5 & province_dhs == 16 ~ 10,
      municipality_name == "Vinzons"           & region_dhs == 5 & province_dhs == 16 ~ 12,
      TRUE ~ municipality_dhs
    )
  )

## Camarines Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "POBLACION, BATO" ~ "Bato",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Baao"           & region_dhs == 5 & province_dhs == 17 ~ 1,
      municipality_name == "Bato"           & region_dhs == 5 & province_dhs == 17 ~ 3,
      municipality_name == "Bula"           & region_dhs == 5 & province_dhs == 17 ~ 6,
      municipality_name == "Cabusao"        & region_dhs == 5 & province_dhs == 17 ~ 7,
      municipality_name == "Calabanga"      & region_dhs == 5 & province_dhs == 17 ~ 8,
      municipality_name == "Camaligan"      & region_dhs == 5 & province_dhs == 17 ~ 9,
      municipality_name == "Canaman"        & region_dhs == 5 & province_dhs == 17 ~ 10,
      municipality_name == "City of Iriga"  & region_dhs == 5 & province_dhs == 17 ~ 16,
      municipality_name == "Libmanan"       & region_dhs == 5 & province_dhs == 17 ~ 18,
      municipality_name == "Milaor"         & region_dhs == 5 & province_dhs == 17 ~ 21,
      municipality_name == "Naga"           & region_dhs == 5 & province_dhs == 17 ~ 24,
      municipality_name == "Pasacao"        & region_dhs == 5 & province_dhs == 17 ~ 27,
      municipality_name == "San Fernando"   & region_dhs == 5 & province_dhs == 17 ~ 32,
      municipality_name == "Sipocot"        & region_dhs == 5 & province_dhs == 17 ~ 34,
      TRUE ~ municipality_dhs
    )
  )

## Catanduanes
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Baras"       & region_dhs == 5 & province_dhs == 20 ~ 2,
      municipality_name == "Caramoran"   & region_dhs == 5 & province_dhs == 20 ~ 4,
      municipality_name == "Pandan"      & region_dhs == 5 & province_dhs == 20 ~ 6,
      municipality_name == "Virac"       & region_dhs == 5 & province_dhs == 20 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Masbate
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 436 ~ "Milagros",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Aroroy"          & region_dhs == 5 & province_dhs == 41 ~ 1,
      municipality_name == "Balud"           & region_dhs == 5 & province_dhs == 41 ~ 3,
      municipality_name == "Batuan"          & region_dhs == 5 & province_dhs == 41 ~ 4,
      municipality_name == "Cawayan"         & region_dhs == 5 & province_dhs == 41 ~ 6,
      municipality_name == "Milagros"        & region_dhs == 5 & province_dhs == 41 ~ 12,
      municipality_name == "Palanas"         & region_dhs == 5 & province_dhs == 41 ~ 15,
      municipality_name == "Pio V. Corpus"   & region_dhs == 5 & province_dhs == 41 ~ 16,
      municipality_name == "Placer"          & region_dhs == 5 & province_dhs == 41 ~ 17,
      municipality_name == "San Jacinto"     & region_dhs == 5 & province_dhs == 41 ~ 19,
      municipality_name == "Uson"            & region_dhs == 5 & province_dhs == 41 ~ 21,
      TRUE ~ municipality_dhs
    )
  )

## Sorsogon
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 444 ~ "Sorsogon City",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Barcelona"         & region_dhs == 5 & province_dhs == 62 ~ 2,
      municipality_name == "Bulan"             & region_dhs == 5 & province_dhs == 62 ~ 3,
      municipality_name == "Casiguran"         & region_dhs == 5 & province_dhs == 62 ~ 5,
      municipality_name == "Castilla"          & region_dhs == 5 & province_dhs == 62 ~ 6,
      municipality_name == "Matnog"            & region_dhs == 5 & province_dhs == 62 ~ 12,
      municipality_name == "Pilar"             & region_dhs == 5 & province_dhs == 62 ~ 13,
      municipality_name == "Santa Magdalena"   & region_dhs == 5 & province_dhs == 62 ~ 15,
      municipality_name == "Sorsogon City"     & region_dhs == 5 & province_dhs == 62 ~ 16,
      TRUE ~ municipality_dhs
    )
  )

# Region 6
## Aklan
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Batan"           & region_dhs == 6 & province_dhs == 4 ~ 4,
      municipality_name == "Kalibo"          & region_dhs == 6 & province_dhs == 4 ~ 7,
      municipality_name == "Nabas"           & region_dhs == 6 & province_dhs == 4 ~ 14,
      municipality_name == "New Washington"  & region_dhs == 6 & province_dhs == 4 ~ 15,
      municipality_name == "Tangalan"        & region_dhs == 6 & province_dhs == 4 ~ 17,
      TRUE ~ municipality_dhs
    )
  )

## Antique
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Anini-y"                     & region_dhs == 6 & province_dhs == 6 ~ 1,
      municipality_name == "Bugasong"                    & region_dhs == 6 & province_dhs == 6 ~ 4,
      municipality_name == "Culasi"                      & region_dhs == 6 & province_dhs == 6 ~ 6,
      municipality_name == "Hamtic"                      & region_dhs == 6 & province_dhs == 6 ~ 8,
      municipality_name == "Libertad"                    & region_dhs == 6 & province_dhs == 6 ~ 10,
      municipality_name == "San Jose de Buenavista"      & region_dhs == 6 & province_dhs == 6 ~ 13,
      TRUE ~ municipality_dhs
    )
  )

## Capiz
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Dao"        & region_dhs == 6 & province_dhs == 19 ~ 2,
      municipality_name == "Ivisan"     & region_dhs == 6 & province_dhs == 19 ~ 5,
      municipality_name == "Jamindan"   & region_dhs == 6 & province_dhs == 19 ~ 6,
      municipality_name == "Mambusao"   & region_dhs == 6 & province_dhs == 19 ~ 8,
      municipality_name == "Panay"      & region_dhs == 6 & province_dhs == 19 ~ 9,
      municipality_name == "Panitan"    & region_dhs == 6 & province_dhs == 19 ~ 10,
      municipality_name == "Roxas City" & region_dhs == 6 & province_dhs == 19 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Iloilo
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Anilao"        & region_dhs == 6 & province_dhs == 30 ~ 3,
      municipality_name == "Barotac Nuevo" & region_dhs == 6 & province_dhs == 30 ~ 7,
      municipality_name == "Barotac Viejo" & region_dhs == 6 & province_dhs == 30 ~ 8,
      municipality_name == "Batad"         & region_dhs == 6 & province_dhs == 30 ~ 9,
      municipality_name == "Carles"        & region_dhs == 6 & province_dhs == 30 ~ 14,
      municipality_name == "Concepcion"    & region_dhs == 6 & province_dhs == 30 ~ 15,
      municipality_name == "Dingle"        & region_dhs == 6 & province_dhs == 30 ~ 16,
      municipality_name == "Dumangas"      & region_dhs == 6 & province_dhs == 30 ~ 18,
      municipality_name == "Igbaras"       & region_dhs == 6 & province_dhs == 30 ~ 21,
      municipality_name == "Iloilo City"   & region_dhs == 6 & province_dhs == 30 ~ 22,
      municipality_name == "Janiuay"       & region_dhs == 6 & province_dhs == 30 ~ 23,
      municipality_name == "Leganes City"  & region_dhs == 6 & province_dhs == 30 ~ 26,
      municipality_name == "Leon"          & region_dhs == 6 & province_dhs == 30 ~ 28,
      municipality_name == "Maasin"        & region_dhs == 6 & province_dhs == 30 ~ 29,
      municipality_name == "Miagao"        & region_dhs == 6 & province_dhs == 30 ~ 30,
      municipality_name == "Passi City"    & region_dhs == 6 & province_dhs == 30 ~ 35,
      municipality_name == "Pavia"         & region_dhs == 6 & province_dhs == 30 ~ 36,
      municipality_name == "San Enrique"   & region_dhs == 6 & province_dhs == 30 ~ 39,
      municipality_name == "San Joaquin"   & region_dhs == 6 & province_dhs == 30 ~ 40,
      municipality_name == "Tigbauan"      & region_dhs == 6 & province_dhs == 30 ~ 45,
      municipality_name == "Zarraga"       & region_dhs == 6 & province_dhs == 30 ~ 47,
      TRUE ~ municipality_dhs
    )
  )

## Negros Occidental
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bacolod"                  & region_dhs == 6 & province_dhs == 45 ~ 1,
      municipality_name == "Bago City"                & region_dhs == 6 & province_dhs == 45 ~ 2,
      municipality_name == "Cadiz City"              & region_dhs == 6 & province_dhs == 45 ~ 4,
      municipality_name == "Candoni"                  & region_dhs == 6 & province_dhs == 45 ~ 6,
      municipality_name == "Cauayan"                  & region_dhs == 6 & province_dhs == 45 ~ 7,
      municipality_name == "Enrique B. Magalona"      & region_dhs == 6 & province_dhs == 45 ~ 8,
      municipality_name == "Escalante City"           & region_dhs == 6 & province_dhs == 45 ~ 9,
      municipality_name == "Himamaylan City"          & region_dhs == 6 & province_dhs == 45 ~ 10,
      municipality_name == "Isabela"                  & region_dhs == 6 & province_dhs == 45 ~ 14,
      municipality_name == "Kabankalan"               & region_dhs == 6 & province_dhs == 45 ~ 15,
      municipality_name == "Pontevedra"               & region_dhs == 6 & province_dhs == 45 ~ 21,
      municipality_name == "Sagay City"               & region_dhs == 6 & province_dhs == 45 ~ 23,
      municipality_name == "San Carlos City"          & region_dhs == 6 & province_dhs == 45 ~ 24,
      municipality_name == "San Enrique"              & region_dhs == 6 & province_dhs == 45 ~ 25,
      municipality_name == "Silay City"               & region_dhs == 6 & province_dhs == 45 ~ 26,
      municipality_name == "Talisay City"             & region_dhs == 6 & province_dhs == 45 ~ 28,
      municipality_name == "Toboso"                   & region_dhs == 6 & province_dhs == 45 ~ 29,
      municipality_name == "Valladolid"               & region_dhs == 6 & province_dhs == 45 ~ 30,
      municipality_name == "Don Salvador Benedicto"   & region_dhs == 6 & province_dhs == 45 ~ 32,
      TRUE ~ municipality_dhs
    )
  )

## Guimaras
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Nueva Valencia" & region_dhs == 6 & province_dhs == 79 ~ 3,
      municipality_name == "Buenavista"     & region_dhs == 6 & province_dhs == 79 ~ 1,
      TRUE ~ municipality_dhs
    )
  )

# Region 7
## Bohol 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Buenavista" & region_dhs == 7 & province_dhs == 12 ~ 9,
      municipality_name == "Guindulman" & region_dhs == 7 & province_dhs == 12 ~ 23,
      municipality_name == "San Isidro" & region_dhs == 7 & province_dhs == 12 ~ 37,
      municipality_name == "San Miguel" & region_dhs == 7 & province_dhs == 12 ~ 38,
      municipality_name == "Balilihan" & region_dhs == 7 & province_dhs == 12 ~ 6,
      municipality_name == "Tubigon" & region_dhs == 7 & province_dhs == 12 ~ 45,
      municipality_name == "Bien Unido" & region_dhs == 7 & province_dhs == 12 ~ 48,
      municipality_name == "Clarin" & region_dhs == 7 & province_dhs == 12 ~ 14,
      municipality_name == "Inabanga" & region_dhs == 7 & province_dhs == 12 ~ 24,
      municipality_name == "Jagna" & region_dhs == 7 & province_dhs == 12 ~ 25,
      municipality_name == "Loay" & region_dhs == 7 & province_dhs == 12 ~ 28,
      municipality_name == "Mabini" & region_dhs == 7 & province_dhs == 12 ~ 31,
      municipality_name == "Panglao" & region_dhs == 7 & province_dhs == 12 ~ 33,
      municipality_name == "Tagbilaran City" & region_dhs == 7 & province_dhs == 12 ~ 42,
      TRUE ~ municipality_dhs
    )
  )

## Cebu 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Alegria" & region_dhs == 7 & province_dhs == 22 ~ 3,
      municipality_name == "Dalaguete" & region_dhs == 7 & province_dhs == 22 ~ 22,
      municipality_name == "Argao" & region_dhs == 7 & province_dhs == 22 ~ 5,
      municipality_name == "Balamban" & region_dhs == 7 & province_dhs == 22 ~ 8,
      municipality_name == "Carcar City" & region_dhs == 7 & province_dhs == 22 ~ 14,
      municipality_name == "Cebu City" & region_dhs == 7 & province_dhs == 22 ~ 17,
      municipality_name == "Mandaue City" & region_dhs == 7 & province_dhs == 22 ~ 30,
      municipality_name == "Cordova" & region_dhs == 7 & province_dhs == 22 ~ 20,
      municipality_name == "Lapu-Lapu City" & region_dhs == 7 & province_dhs == 22 ~ 26,
      municipality_name == "Consolacion" & region_dhs == 7 & province_dhs == 22 ~ 19,
      municipality_name == "Malabuyoc" & region_dhs == 7 & province_dhs == 22 ~ 29,
      municipality_name == "Moalboal" & region_dhs == 7 & province_dhs == 22 ~ 33,
      municipality_name == "Ronda" & region_dhs == 7 & province_dhs == 22 ~ 39,
      municipality_name == "Talisay City" & region_dhs == 7 & province_dhs == 22 ~ 50,
      municipality_name == "Toledo City" & region_dhs == 7 & province_dhs == 22 ~ 51,
      municipality_name == "Tuburan" & region_dhs == 7 & province_dhs == 22 ~ 52,
      municipality_name == "Asturias" & region_dhs == 7 & province_dhs == 22 ~ 6,
      municipality_name == "Compostela" & region_dhs == 7 & province_dhs == 22 ~ 18,
      municipality_name == "Minglanilla" & region_dhs == 7 & province_dhs == 22 ~ 32,
      municipality_name == "San Fernando" & region_dhs == 7 & province_dhs == 22 ~ 41,
      municipality_name == "San Remigio" & region_dhs == 7 & province_dhs == 22 ~ 43,
      TRUE ~ municipality_dhs
    )
  )

## Negros Oriental 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bindoy" & region_dhs == 7 & province_dhs == 46 ~ 7,
      municipality_name == "Dumaguete" & region_dhs == 7 & province_dhs == 46 ~ 10,
      municipality_name == "Guihulngan City" & region_dhs == 7 & province_dhs == 46 ~ 11,
      municipality_name == "Tayasan" & region_dhs == 7 & province_dhs == 46 ~ 22,
      municipality_name == "San Jose" & region_dhs == 7 & province_dhs == 46 ~ 17,
      municipality_name == "Santa Catalina" & region_dhs == 7 & province_dhs == 46 ~ 18,
      municipality_name == "Siaton" & region_dhs == 7 & province_dhs == 46 ~ 19,
      municipality_name == "Tanjay City" & region_dhs == 7 & province_dhs == 46 ~ 21,
      municipality_name == "Vallehermoso" & region_dhs == 7 & province_dhs == 46 ~ 24,
      municipality_name == "Jimalalud" & region_dhs == 7 & province_dhs == 46 ~ 12,
      municipality_name == "Mabinay" & region_dhs == 7 & province_dhs == 46 ~ 14,
      municipality_name == "Valencia" & region_dhs == 7 & province_dhs == 46 ~ 23,
      municipality_name == "Bais" & region_dhs == 7 & province_dhs == 46 ~ 4,
      municipality_name == "Sibulan" & region_dhs == 7 & province_dhs == 46 ~ 20,
      TRUE ~ municipality_dhs
    )
  )

## Siquijor
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      municipality_name == "" & province_name == "Siquijor" & year == 2003 ~ "Siquijor",
      TRUE ~ municipality_name
    )
  ) |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Siquijor" & region_dhs == 7 & province_dhs == 61 ~ 6,
      municipality_name == "Lazi"     & region_dhs == 7 & province_dhs == 61 ~ 3,
      TRUE ~ municipality_dhs
    )
  )

# Region 8
## Eastern Samar 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 540 & year == 2008 ~ "Can-avid",
      TRUE ~ municipality_name
    )
  ) |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Maydolong"     & region_dhs == 8 & province_dhs == 26 ~ 15,
      municipality_name == "Guiuan"        & region_dhs == 8 & province_dhs == 26 ~ 9,
      municipality_name == "Hernani"       & region_dhs == 8 & province_dhs == 26 ~ 10,
      municipality_name == "San Policarpo" & region_dhs == 8 & province_dhs == 26 ~ 21,
      municipality_name == "Sulat"         & region_dhs == 8 & province_dhs == 26 ~ 22,
      municipality_name == "Taft"          & region_dhs == 8 & province_dhs == 26 ~ 23,
      municipality_name == "Can-avid"      & region_dhs == 8 & province_dhs == 26 ~ 5,
      TRUE ~ municipality_dhs
    )
  )

## Leyte 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 547 & year == 2008 ~ "Hilongos",
      cluster == 551 & year == 2008 ~ "Capoocan",
      cluster == 387 & year == 2003 ~ "Matalom",
      TRUE ~ municipality_name
    )
  ) |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Abuyog"       & region_dhs == 8 & province_dhs == 37 ~ 1,
      municipality_name == "Babatngon"    & region_dhs == 8 & province_dhs == 37 ~ 5,
      municipality_name == "Barugo"       & region_dhs == 8 & province_dhs == 37 ~ 6,
      municipality_name == "Bato"         & region_dhs == 8 & province_dhs == 37 ~ 7,
      municipality_name == "Baybay City"  & region_dhs == 8 & province_dhs == 37 ~ 8,
      municipality_name == "Calubian"     & region_dhs == 8 & province_dhs == 37 ~ 13,
      municipality_name == "Hilongos"     & region_dhs == 8 & province_dhs == 37 ~ 19,
      municipality_name == "Javier"       & region_dhs == 8 & province_dhs == 37 ~ 24,
      municipality_name == "Matalom"      & region_dhs == 8 & province_dhs == 37 ~ 34,
      municipality_name == "Ormoc"        & region_dhs == 8 & province_dhs == 37 ~ 38,
      municipality_name == "San Isidro"   & region_dhs == 8 & province_dhs == 37 ~ 42,
      municipality_name == "San Miguel"   & region_dhs == 8 & province_dhs == 37 ~ 43,
      municipality_name == "Tacloban City"& region_dhs == 8 & province_dhs == 37 ~ 47,
      municipality_name == "Carigara"     & region_dhs == 8 & province_dhs == 37 ~ 15,
      municipality_name == "Isabel"       & region_dhs == 8 & province_dhs == 37 ~ 22,
      municipality_name == "Jaro"         & region_dhs == 8 & province_dhs == 37 ~ 23,
      municipality_name == "Matag-ob"     & region_dhs == 8 & province_dhs == 37 ~ 33,
      municipality_name == "Mayorga"      & region_dhs == 8 & province_dhs == 37 ~ 35,
      municipality_name == "Villaba"      & region_dhs == 8 & province_dhs == 37 ~ 51,
      municipality_name == "Capoocan"     & region_dhs == 8 & province_dhs == 37 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Northern Samar 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 561 & year == 2008 ~ "Las Navas",
      TRUE ~ municipality_name
    )
  ) |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Allen"     & region_dhs == 8 & province_dhs == 48 ~ 1,
      municipality_name == "Gamay"     & region_dhs == 8 & province_dhs == 48 ~ 7,
      municipality_name == "Lavezares" & region_dhs == 8 & province_dhs == 48 ~ 11,
      municipality_name == "Catarman"  & region_dhs == 8 & province_dhs == 48 ~ 5,
      municipality_name == "Las Navas" & region_dhs == 8 & province_dhs == 48 ~ 10,
      municipality_name == "Rosario"   & region_dhs == 8 & province_dhs == 48 ~ 16,
      TRUE ~ municipality_dhs
    )
  )

## Samar
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Calbayog City" & region_dhs == 8 & province_dhs == 60 ~ 3,
      municipality_name == "Basey"         & region_dhs == 8 & province_dhs == 60 ~ 2,
      municipality_name == "Daram"         & region_dhs == 8 & province_dhs == 60 ~ 6,
      municipality_name == "Marabut"       & region_dhs == 8 & province_dhs == 60 ~ 10,
      municipality_name == "Paranas"       & region_dhs == 8 & province_dhs == 60 ~ 22,
      TRUE ~ municipality_dhs
    )
  )

## Southern Leyte 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Hinunangan"     & region_dhs == 8 & province_dhs == 64 ~ 3,
      municipality_name == "Libagon"        & region_dhs == 8 & province_dhs == 64 ~ 5,
      municipality_name == "Maasin City"    & region_dhs == 8 & province_dhs == 64 ~ 7,
      municipality_name == "Bontoc"         & region_dhs == 8 & province_dhs == 64 ~ 2,
      municipality_name == "Pintuyan"       & region_dhs == 8 & province_dhs == 64 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Biliran 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Cabucgayan" & region_dhs == 8 & province_dhs == 78 ~ 3,
      municipality_name == "Caibiran"   & region_dhs == 8 & province_dhs == 78 ~ 4,
      municipality_name == "Almeria"    & region_dhs == 8 & province_dhs == 78 ~ 1,
      municipality_name == "Naval"      & region_dhs == 8 & province_dhs == 78 ~ 8,
      TRUE ~ municipality_dhs
    )
  )

# Region 9
## Zamboanga del Norte
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 416 & year == 2003 ~ "Sindangan",
      cluster == 410 & year == 2003 ~ "Dapitan City",
      cluster == 579 & year == 2008 ~ "Pres. Manuel A. Roxas",
      cluster == 580 & year == 2008 ~ "Sergio Osmena Sr.",
      cluster == 582 & year == 2008 ~ "Sindangan",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Dapitan City"          & region_dhs == 9 & province_dhs == 72 ~ 1,
      municipality_name == "Mutia"                 & region_dhs == 9 & province_dhs == 72 ~ 8,
      municipality_name == "Polanco"               & region_dhs == 9 & province_dhs == 72 ~ 10,
      municipality_name == "Siayan"                & region_dhs == 9 & province_dhs == 72 ~ 15,
      municipality_name == "Sindangan"             & region_dhs == 9 & province_dhs == 72 ~ 18,
      municipality_name == "Manukan"               & region_dhs == 9 & province_dhs == 72 ~ 7,
      municipality_name == "Gutalac"               & region_dhs == 9 & province_dhs == 72 ~ 23,
      municipality_name == "Dipolog City"          & region_dhs == 9 & province_dhs == 72 ~ 2,
      municipality_name == "Liloy"                 & region_dhs == 9 & province_dhs == 72 ~ 6,
      municipality_name == "Sibuco"                & region_dhs == 9 & province_dhs == 72 ~ 16,
      municipality_name == "Sirawai"               & region_dhs == 9 & province_dhs == 72 ~ 20,
      municipality_name == "Pres. Manuel A. Roxas" & region_dhs == 9 & province_dhs == 72 ~ 11,
      municipality_name == "Sergio Osmena Sr."     & region_dhs == 9 & province_dhs == 72 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Zamboanga del Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 421 ~ "Kumalarang",
      year == 2008 & cluster == 587 ~ "Pagadian City",
      year == 2008 & cluster == 589 ~ "Josefina",
      year == 2008 & cluster == 593 ~ "Tukuran",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Aurora"               & region_dhs == 9 & province_dhs == 73 ~ 2,
      municipality_name == "Margosatubig"         & region_dhs == 9 & province_dhs == 73 ~ 17,
      municipality_name == "Tambulig"             & region_dhs == 9 & province_dhs == 73 ~ 28,
      municipality_name == "Molave"               & region_dhs == 9 & province_dhs == 73 ~ 19,
      municipality_name == "Pagadian City"        & region_dhs == 9 & province_dhs == 73 ~ 22,
      municipality_name == "Vincenzo A. Sagun"    & region_dhs == 9 & province_dhs == 73 ~ 41,
      municipality_name == "Guipos"               & region_dhs == 9 & province_dhs == 73 ~ 43,
      municipality_name == "Zamboanga"            & region_dhs == 9 & province_dhs == 73 ~ 32,
      municipality_name == "Dumingag"             & region_dhs == 9 & province_dhs == 73 ~ 8,
      municipality_name == "Midsalip"             & region_dhs == 9 & province_dhs == 73 ~ 18,
      municipality_name == "Tabina"               & region_dhs == 9 & province_dhs == 73 ~ 27,
      municipality_name == "Tukuran"              & region_dhs == 9 & province_dhs == 73 ~ 30,
      municipality_name == "Josefina"             & region_dhs == 9 & province_dhs == 73 ~ 37,
      municipality_name == "Kumalarang"           & region_dhs == 9 & province_dhs == 73 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Zamboanga Sibugay
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster %in% c(608, 610) ~ "Buug",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Diplahan"           & region_dhs == 9 & province_dhs == 83 ~ 3,
      municipality_name == "Malangas"           & region_dhs == 9 & province_dhs == 83 ~ 8,
      municipality_name == "Payao"              & region_dhs == 9 & province_dhs == 83 ~ 11,
      municipality_name == "Roseller T Lim"     & region_dhs == 9 & province_dhs == 83 ~ 12,
      municipality_name == "Siay"               & region_dhs == 9 & province_dhs == 83 ~ 13,
      municipality_name == "Alicia"             & region_dhs == 9 & province_dhs == 83 ~ 1,
      municipality_name == "Buug"               & region_dhs == 9 & province_dhs == 83 ~ 2,
      TRUE ~ municipality_dhs
    )
  )

# Region 10
## Bukidnon -
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Kibawe"           & region_dhs == 10 & province_dhs == 13 ~ 8,
      municipality_name == "Malaybalay City"  & region_dhs == 10 & province_dhs == 13 ~ 12,
      municipality_name == "Malitbog"         & region_dhs == 10 & province_dhs == 13 ~ 13,
      municipality_name == "Manolo Fortich"   & region_dhs == 10 & province_dhs == 13 ~ 14,
      municipality_name == "Libona"           & region_dhs == 10 & province_dhs == 13 ~ 11,
      municipality_name == "Pangantucan"      & region_dhs == 10 & province_dhs == 13 ~ 16,
      municipality_name == "Quezon"           & region_dhs == 10 & province_dhs == 13 ~ 17,
      municipality_name == "Talakag"          & region_dhs == 10 & province_dhs == 13 ~ 20,
      municipality_name == "Valencia City"    & region_dhs == 10 & province_dhs == 13 ~ 21,
      municipality_name == "Cabanglasan"      & region_dhs == 10 & province_dhs == 13 ~ 22,
      municipality_name == "Don Carlos"       & region_dhs == 10 & province_dhs == 13 ~ 4,
      municipality_name == "Lantapan"         & region_dhs == 10 & province_dhs == 13 ~ 10,
      municipality_name == "Maramag"          & region_dhs == 10 & province_dhs == 13 ~ 15,
      TRUE ~ municipality_dhs
    )
  )

## Camiguin 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Mahinog"   & region_dhs == 10 & province_dhs == 18 ~ 3,
      municipality_name == "Mambajao"  & region_dhs == 10 & province_dhs == 18 ~ 4,
      TRUE ~ municipality_dhs
    )
  )

## Lanao del Norte
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 624 & year == 2008 ~ "Bacolod",
      cluster == 625 & year == 2008 ~ "Baloi",
      cluster %in% c(628, 630) & year == 2008 ~ "Iligan City",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Iligan City"           & region_dhs == 10 & province_dhs == 35 ~ 4,
      municipality_name == "Sultan Naga Dimaporo"  & region_dhs == 10 & province_dhs == 35 ~ 6,
      municipality_name == "Lala"                  & region_dhs == 10 & province_dhs == 35 ~ 9,
      municipality_name == "Baroy"                 & region_dhs == 10 & province_dhs == 35 ~ 3,
      municipality_name == "Sapad"                 & region_dhs == 10 & province_dhs == 35 ~ 19,
      municipality_name == "Tagoloan"              & region_dhs == 10 & province_dhs == 35 ~ 20,
      municipality_name == "Bacolod"               & region_dhs == 10 & province_dhs == 35 ~ 1,
      municipality_name == "Baloi"                 & region_dhs == 10 & province_dhs == 35 ~ 2,
      municipality_name == "Pantar"                & region_dhs == 10 & province_dhs == 35 ~ 23,
      TRUE ~ municipality_dhs
    )
  )

## Misamis Occidental
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster %in% c(467, 468) ~ "Oroquieta City",
      year == 2008 & cluster == 632 ~ "Jimenez",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Bonifacio"       & region_dhs == 10 & province_dhs == 42 ~ 3,
      municipality_name == "Oroquieta City"  & region_dhs == 10 & province_dhs == 42 ~ 9,
      municipality_name == "Aloran"          & region_dhs == 10 & province_dhs == 42 ~ 1,
      municipality_name == "Ozamiz City"     & region_dhs == 10 & province_dhs == 42 ~ 10,
      municipality_name == "Plaridel"        & region_dhs == 10 & province_dhs == 42 ~ 12,
      municipality_name == "Sinacaban"       & region_dhs == 10 & province_dhs == 42 ~ 14,
      municipality_name == "Jimenez"         & region_dhs == 10 & province_dhs == 42 ~ 7,
      TRUE ~ municipality_dhs
    )
  )

## Misamis Oriental 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 650 ~ "Salay",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Laguindingan"        & region_dhs == 10 & province_dhs == 43 ~ 14,
      municipality_name == "Balingasag"          & region_dhs == 10 & province_dhs == 43 ~ 2,
      municipality_name == "Claveria"            & region_dhs == 10 & province_dhs == 43 ~ 6,
      municipality_name == "Gingoog"             & region_dhs == 10 & province_dhs == 43 ~ 8,
      municipality_name == "Gitagum"             & region_dhs == 10 & province_dhs == 43 ~ 9,
      municipality_name == "Cagayan de Oro"      & region_dhs == 10 & province_dhs == 43 ~ 5,
      municipality_name == "El Salvador City"    & region_dhs == 10 & province_dhs == 43 ~ 7,
      municipality_name == "Initao"              & region_dhs == 10 & province_dhs == 43 ~ 10,
      municipality_name == "Jasaan"              & region_dhs == 10 & province_dhs == 43 ~ 11,
      municipality_name == "Manticao"            & region_dhs == 10 & province_dhs == 43 ~ 18,
      municipality_name == "Opol"                & region_dhs == 10 & province_dhs == 43 ~ 21,
      municipality_name == "Salay"               & region_dhs == 10 & province_dhs == 43 ~ 22,
      municipality_name == "Kinoguitan"          & region_dhs == 10 & province_dhs == 43 ~ 12,
      TRUE ~ municipality_dhs
    )
  )

# Region 11
## Davao del Norte 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 653 ~ "Asuncion",
      year == 2008 & cluster == 656 ~ "Tagum City",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Carmen"                       & region_dhs == 11 & province_dhs == 23 ~ 3,
      municipality_name == "New Corella"                 & region_dhs == 11 & province_dhs == 23 ~ 14,
      municipality_name == "Island Garden City of Samal" & region_dhs == 11 & province_dhs == 23 ~ 17,
      municipality_name == "Tagum City"                  & region_dhs == 11 & province_dhs == 23 ~ 19,
      municipality_name == "San Isidro"                  & region_dhs == 11 & province_dhs == 23 ~ 24,
      municipality_name == "Asuncion"                    & region_dhs == 11 & province_dhs == 23 ~ 1,
      municipality_name == "Panabo"                      & region_dhs == 11 & province_dhs == 23 ~ 15,
      TRUE ~ municipality_dhs
    )
  )

## Davao de Oro 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 686 ~ "Maragusan",
      municipality_name == "Loreto" & region_dhs == 11 & province_dhs == 82 ~ "Laak",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Monkayo"      & region_dhs == 11 & province_dhs == 82 ~ 7,
      municipality_name == "Montevista"   & region_dhs == 11 & province_dhs == 82 ~ 8,
      municipality_name == "Nabunturan"   & region_dhs == 11 & province_dhs == 82 ~ 9,
      municipality_name == "New Bataan"   & region_dhs == 11 & province_dhs == 82 ~ 10,
      municipality_name == "Pantukan"     & region_dhs == 11 & province_dhs == 82 ~ 11,
      municipality_name == "Laak"         & region_dhs == 11 & province_dhs == 82 ~ 2,
      municipality_name == "Maragusan"    & region_dhs == 11 & province_dhs == 82 ~ 5,
      TRUE ~ municipality_dhs
    )
  )

## Davao del Sur 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      cluster == 661 & year == 2008 ~ "Davao City",
      municipality_name == "Bunawan" & region_dhs == 11 & province_dhs == 24 ~ "Davao City",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Davao City"          & region_dhs == 11 & province_dhs == 24 ~ 2,
      municipality_name == "Digos City"          & region_dhs == 11 & province_dhs == 24 ~ 3,
      municipality_name == "Jose Abad Santos"    & region_dhs == 11 & province_dhs == 24 ~ 5,
      municipality_name == "Kiblawan"            & region_dhs == 11 & province_dhs == 24 ~ 6,
      municipality_name == "Matanao"             & region_dhs == 11 & province_dhs == 24 ~ 10,
      municipality_name == "Malita"              & region_dhs == 11 & province_dhs == 24 ~ 9,
      municipality_name == "Magsaysay"           & region_dhs == 11 & province_dhs == 24 ~ 7,
      municipality_name == "Don Marcelino"       & region_dhs == 11 & province_dhs == 24 ~ 16,
      municipality_name == "Santa Maria"         & region_dhs == 11 & province_dhs == 24 ~ 13,
      TRUE ~ municipality_dhs
    )
  )

## Davao Oriental 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Baganga"    & region_dhs == 11 & province_dhs == 25 ~ 1,
      municipality_name == "Cateel"     & region_dhs == 11 & province_dhs == 25 ~ 5,
      municipality_name == "Banaybanay" & region_dhs == 11 & province_dhs == 25 ~ 2,
      municipality_name == "Mati"       & region_dhs == 11 & province_dhs == 25 ~ 9,
      municipality_name == "San Isidro" & region_dhs == 11 & province_dhs == 25 ~ 10,
      TRUE ~ municipality_dhs
    )
  )

# Region 12
## North Cotabato 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 693 ~ "Kabakan",
      year == 2008 & cluster == 700 ~ "Midsayap",
      year == 2003 & (cluster == 774 | cluster == 775) ~ "Pigcawayan",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Alamada"          & region_dhs == 12 & province_dhs == 47 ~ 1,
      municipality_name == "Kabacan"          & region_dhs == 12 & province_dhs == 47 ~ 3,
      municipality_name == "Kabakan"          & region_dhs == 12 & province_dhs == 47 ~ 3,
      municipality_name == "Matalam"          & region_dhs == 12 & province_dhs == 47 ~ 8,
      municipality_name == "Midsayap"         & region_dhs == 12 & province_dhs == 47 ~ 9,
      municipality_name == "Aleosan"          & region_dhs == 12 & province_dhs == 47 ~ 17,
      municipality_name == "Tulunan"          & region_dhs == 12 & province_dhs == 47 ~ 14,
      municipality_name == "Banisilan"        & region_dhs == 12 & province_dhs == 47 ~ 16,
      municipality_name == "M'lang"           & region_dhs == 12 & province_dhs == 47 ~ 10,
      municipality_name == "Pikit"            & region_dhs == 12 & province_dhs == 47 ~ 12,
      municipality_name == "Makilala"         & region_dhs == 12 & province_dhs == 47 ~ 7,
      municipality_name == "Carmen"           & region_dhs == 12 & province_dhs == 47 ~ 2,
      municipality_name == "President Roxas"  & region_dhs == 12 & province_dhs == 47 ~ 13,
      municipality_name == "Kidapawan City"   & region_dhs == 12 & province_dhs == 47 ~ 4,
      municipality_name == "Pigcawayan"       & region_dhs == 12 & province_dhs == 47 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## South Cotabato
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Banga"              & region_dhs == 12 & province_dhs == 63 ~ 2,
      municipality_name == "General Santos City"& region_dhs == 12 & province_dhs == 63 ~ 3,
      municipality_name == "Polomolok"          & region_dhs == 12 & province_dhs == 63 ~ 12,
      municipality_name == "Koronadal City"     & region_dhs == 12 & province_dhs == 63 ~ 6,
      municipality_name == "Norala"             & region_dhs == 12 & province_dhs == 63 ~ 11,
      municipality_name == "Surallah"           & region_dhs == 12 & province_dhs == 63 ~ 13,
      municipality_name == "T'boli"             & region_dhs == 12 & province_dhs == 63 ~ 16,
      municipality_name == "Tampakan"           & region_dhs == 12 & province_dhs == 63 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Sultan Kudarat 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 557 ~ "Lebak",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Isulan"                  & region_dhs == 12 & province_dhs == 65 ~ 4,
      municipality_name == "Bagumbayan"              & region_dhs == 12 & province_dhs == 65 ~ 1,
      municipality_name == "Palimbang"               & region_dhs == 12 & province_dhs == 65 ~ 9,
      municipality_name == "President Quirino"       & region_dhs == 12 & province_dhs == 65 ~ 10,
      municipality_name == "Lebak"                   & region_dhs == 12 & province_dhs == 65 ~ 6,
      municipality_name == "Senator Ninoy Aquino"    & region_dhs == 12 & province_dhs == 65 ~ 12,
      TRUE ~ municipality_dhs
    )
  )

## Sarangani 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Alabel"      & region_dhs == 12 & province_dhs == 80 ~ 1,
      municipality_name == "Glan"        & region_dhs == 12 & province_dhs == 80 ~ 2,
      municipality_name == "Kiamba"      & region_dhs == 12 & province_dhs == 80 ~ 3,
      municipality_name == "Maasim"      & region_dhs == 12 & province_dhs == 80 ~ 4,
      municipality_name == "Malapatan"   & region_dhs == 12 & province_dhs == 80 ~ 6,
      municipality_name == "Malungon"    & region_dhs == 12 & province_dhs == 80 ~ 7,
      TRUE ~ municipality_dhs
    )
  )

# Region 13
## NCR 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      # NCR City of Manila
      municipality_name == "Manila"                        & region_dhs == 13 & province_dhs == 39 ~ 99,
      
      # NCR Second District
      municipality_name == "Mandaluyong"                   & region_dhs == 13 & province_dhs == 74 ~ 1,
      municipality_name == "Marikina"                      & region_dhs == 13 & province_dhs == 74 ~ 2,
      municipality_name == "Pasig"                         & region_dhs == 13 & province_dhs == 74 ~ 3,
      municipality_name == "Quezon City"                   & region_dhs == 13 & province_dhs == 74 ~ 4,
      municipality_name == "Lungsod Quezon"                & region_dhs == 13 & province_dhs == 74 ~ 4,
      municipality_name == "San Juan"                      & region_dhs == 13 & province_dhs == 74 ~ 5,
      
      # NCR Third District
      municipality_name == "Caloocan"                      & region_dhs == 13 & province_dhs == 75 ~ 1,
      municipality_name == "Malabon"                       & region_dhs == 13 & province_dhs == 75 ~ 2,
      municipality_name == "City of Malabon"               & region_dhs == 13 & province_dhs == 75 ~ 2,
      municipality_name == "Navotas"                       & region_dhs == 13 & province_dhs == 75 ~ 3,
      municipality_name == "City of Navotas"               & region_dhs == 13 & province_dhs == 75 ~ 3,
      municipality_name == "Valenzuela"                    & region_dhs == 13 & province_dhs == 75 ~ 4,
      municipality_name == "Lungsod ng Valenzuela"         & region_dhs == 13 & province_dhs == 75 ~ 4,
      
      # NCR Fourth District
      municipality_name == "Las Pinas"                     & region_dhs == 13 & province_dhs == 76 ~ 1,
      municipality_name == "Makati"                        & region_dhs == 13 & province_dhs == 76 ~ 2,
      municipality_name == "Mutinlupa"                     & region_dhs == 13 & province_dhs == 76 ~ 3,
      municipality_name == "Muntinlupa"                    & region_dhs == 13 & province_dhs == 76 ~ 3,
      municipality_name == "Paranaque"                     & region_dhs == 13 & province_dhs == 76 ~ 4,
      municipality_name == "Parañaque"                     & region_dhs == 13 & province_dhs == 76 ~ 4,
      municipality_name == "Pasay"                         & region_dhs == 13 & province_dhs == 76 ~ 5,
      municipality_name == "Pateros"                       & region_dhs == 13 & province_dhs == 76 ~ 6,
      municipality_name == "Taguig"                        & region_dhs == 13 & province_dhs == 76 ~ 7,
      
      TRUE ~ municipality_dhs
    )
  )

# Region 14
## Abra
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bangued"       & region_dhs == 14 & province_dhs == 1 ~ 1,
      municipality_name == "La Paz"        & region_dhs == 14 & province_dhs == 1 ~ 8,
      municipality_name == "Lacuan-Baay"   & region_dhs == 14 & province_dhs == 1 ~ 13,
      municipality_name == "Luba"          & region_dhs == 14 & province_dhs == 1 ~ 14,
      TRUE ~ municipality_dhs
    )
  )

## Benguet
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "BAGUIO"        & region_dhs == 14 & province_dhs == 11 ~ 2,
      municipality_name == "BANGUIO"       & region_dhs == 14 & province_dhs == 11 ~ 2,
      municipality_name == "Itogon"        & region_dhs == 14 & province_dhs == 11 ~ 6,
      municipality_name == "La Trinidad"   & region_dhs == 14 & province_dhs == 11 ~ 10,
      municipality_name == "Sablan"        & region_dhs == 14 & province_dhs == 11 ~ 12,
      municipality_name == "Tuba"          & region_dhs == 14 & province_dhs == 11 ~ 13,
      municipality_name == "Atok"          & region_dhs == 14 & province_dhs == 11 ~ 1,
      municipality_name == "Mankayan"      & region_dhs == 14 & province_dhs == 11 ~ 11,
      municipality_name == "Kabayan"       & region_dhs == 14 & province_dhs == 11 ~ 7,
      municipality_name == "Kapangan"      & region_dhs == 14 & province_dhs == 11 ~ 8,
      TRUE ~ municipality_dhs
    )
  )

## Ifugao
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 743 ~ "Lagawe",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Mayoyao"       & region_dhs == 14 & province_dhs == 27 ~ 6,
      municipality_name == "Aguinaldo"     & region_dhs == 14 & province_dhs == 27 ~ 8,
      municipality_name == "Alfonso Lista" & region_dhs == 14 & province_dhs == 27 ~ 7,
      municipality_name == "Tinoc"         & region_dhs == 14 & province_dhs == 27 ~ 10,
      municipality_name == "Lagawe"        & region_dhs == 14 & province_dhs == 27 ~ 4,
      TRUE ~ municipality_dhs
    )
  )

## Kalinga
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Tabuk"         & region_dhs == 14 & province_dhs == 32 ~ 13,
      municipality_name == "Pinukpuk"      & region_dhs == 14 & province_dhs == 32 ~ 9,
      municipality_name == "Tinglayan"     & region_dhs == 14 & province_dhs == 32 ~ 15,
      TRUE ~ municipality_dhs
    )
  )

## Mountain Province
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Sadanga"       & region_dhs == 14 & province_dhs == 44 ~ 8,
      municipality_name == "Natonin"       & region_dhs == 14 & province_dhs == 44 ~ 5,
      municipality_name == "Sabangan"      & region_dhs == 14 & province_dhs == 44 ~ 7,
      municipality_name == "Bontoc"        & region_dhs == 14 & province_dhs == 44 ~ 4,
      municipality_name == "Bauko"         & region_dhs == 14 & province_dhs == 44 ~ 2,
      municipality_name == "Tadian"        & region_dhs == 14 & province_dhs == 44 ~ 10,
      TRUE ~ municipality_dhs
    )
  )

## Apayao
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Conner"        & region_dhs == 14 & province_dhs == 81 ~ 2,
      municipality_name == "Luna"          & region_dhs == 14 & province_dhs == 81 ~ 5,
      municipality_name == "Calanasan"     & region_dhs == 14 & province_dhs == 81 ~ 1,
      TRUE ~ municipality_dhs
    )
  )

# Region 15
## Basilan
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Lamitan City" & region_dhs == 15 & province_dhs == 7 ~ 2,
      municipality_name == "Tuburan"      & region_dhs == 15 & province_dhs == 7 ~ 7,
      municipality_name == "Sumisip"      & region_dhs == 15 & province_dhs == 7 ~ 5,
      TRUE ~ municipality_dhs
    )
  )

## Lanao del Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 740 ~ "Sultan Dumalondong",
      year == 2003 & cluster == 761 ~ "Wao",
      municipality_name == "La Trinidad" & region_dhs == 15 & province_dhs == 36 ~ "Saguiaran",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Sultan Dumalondong" & region_dhs == 15 & province_dhs == 36 ~ 40,
      municipality_name == "Tugaya"             & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 29,
      municipality_name == "Maguing"            & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 34,
      municipality_name == "Lumbayanague"       & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 36,
      municipality_name == "Marawi City"        & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 17,
      municipality_name == "Kapai"              & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 10,
      municipality_name == "Butig"              & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 7,
      municipality_name == "Wao"                & region_dhs %in% c(10, 15) & province_dhs == 36 ~ 30,
      municipality_name == "Balindong"          & region_dhs == 15 & province_dhs == 36 ~ 3,
      municipality_name == "Pagayawan"          & region_dhs == 15 & province_dhs == 36 ~ 20,
      municipality_name == "Tubaran"            & region_dhs == 15 & province_dhs == 36 ~ 28,
      municipality_name == "Saguiaran"          & region_dhs == 15 & province_dhs == 36 ~ 25,
      municipality_name == "Lumba - Bayabao"    & region_dhs == 15 & province_dhs == 36 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Maguindanao
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 749 ~ "Datu Anggal Midtimbang",
      year == 2003 & cluster == 769 ~ "Sultan Kudarat",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Ampatuan"             & region_dhs == 15 & province_dhs == 38 ~ 1,
      municipality_name == "Buldon"               & region_dhs == 15 & province_dhs == 38 ~ 2,
      municipality_name == "Datu Piang"           & region_dhs == 15 & province_dhs == 38 ~ 6,
      municipality_name == "Upi"                  & region_dhs == 15 & province_dhs == 38 ~ 15,
      municipality_name == "Mamasapano"           & region_dhs == 15 & province_dhs == 38 ~ 20,
      municipality_name == "Sultan Kudarat"       & region_dhs == 15 & province_dhs == 38 ~ 12,
      municipality_name == "Sultan Sa Barongis"   & region_dhs == 15 & province_dhs == 38 ~ 13,
      municipality_name == "Datu Blah Sinsuat"    & region_dhs == 15 & province_dhs == 38 ~ 30,
      municipality_name == "Cotabato City"        & region_dhs == 15 & province_dhs == 38 ~ 4,
      municipality_name == "South Upi"            & region_dhs == 15 & province_dhs == 38 ~ 17,
      municipality_name == "Sultan Mastura"       & region_dhs == 15 & province_dhs == 38 ~ 24,
      municipality_name == "Talitay"              & region_dhs == 15 & province_dhs == 38 ~ 21,
      municipality_name == "Parang"               & region_dhs == 15 & province_dhs == 38 ~ 11,
      municipality_name == "Matanog"              & region_dhs == 15 & province_dhs == 38 ~ 9,
      municipality_name == "Pagagawan"            & region_dhs == 15 & province_dhs == 38 ~ 22,
      municipality_name == "Buluan"               & region_dhs == 15 & province_dhs == 38 ~ 3,
      municipality_name == "Datu Anggal Midtimbang"& region_dhs == 15 & province_dhs == 38 ~ 31,
      TRUE ~ municipality_dhs
    )
  )

## Sulu
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 783 ~ "Tongkil",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Jolo"                  & region_dhs == 15 & province_dhs == 66 ~ 2,
      municipality_name == "Patikul"               & region_dhs == 15 & province_dhs == 66 ~ 11,
      municipality_name == "Saisi"                 & region_dhs == 15 & province_dhs == 66 ~ 12,
      municipality_name == "Siasi"                 & region_dhs == 15 & province_dhs == 66 ~ 12,
      municipality_name == "Talipao"               & region_dhs == 15 & province_dhs == 66 ~ 13,
      municipality_name == "Hadji Panglima Tahil"  & region_dhs == 15 & province_dhs == 66 ~ 6,
      municipality_name == "Old Panamao"           & region_dhs == 15 & province_dhs == 66 ~ 7,
      municipality_name == "Kalingalan Caluang"    & region_dhs == 15 & province_dhs == 66 ~ 3,
      municipality_name == "Luuk"                  & region_dhs == 15 & province_dhs == 66 ~ 4,
      municipality_name == "Tongkil"               & region_dhs == 15 & province_dhs == 66 ~ 15,
      TRUE ~ municipality_dhs
    )
  )

## Tawi-Tawi
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bongao"     & region_dhs == 15 & province_dhs == 70 ~ 2,
      municipality_name == "Simunul"    & region_dhs == 15 & province_dhs == 70 ~ 4,
      municipality_name == "Languyan"   & region_dhs == 15 & province_dhs == 70 ~ 9,
      municipality_name == "Sibutu"     & region_dhs == 15 & province_dhs == 70 ~ 11,
      municipality_name == "Sapa-Sapa"  & region_dhs == 15 & province_dhs == 70 ~ 10,
      TRUE ~ municipality_dhs
    )
  )

# Region 16
## Agusan del Norte
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2003 & cluster == 792 ~ "Las Nieves",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Butuan City"              & region_dhs == 16 & province_dhs == 2  ~ 2,
      municipality_name == "Magallanes"               & region_dhs == 16 & province_dhs == 2  ~ 8,
      municipality_name == "Santiago"                 & region_dhs == 16 & province_dhs == 2  ~ 10,
      municipality_name == "Remedios T. Romualdez"    & region_dhs == 16 & province_dhs == 2  ~ 12,
      municipality_name == "Cabadbaran City"          & region_dhs == 16 & province_dhs == 2  ~ 3,
      municipality_name == "Jabonga"                  & region_dhs == 16 & province_dhs == 2  ~ 5,
      municipality_name == "Carmen"                   & region_dhs == 16 & province_dhs == 2  ~ 4,
      municipality_name == "Las Nieves"               & region_dhs == 16 & province_dhs == 2  ~ 7,
      TRUE ~ municipality_dhs
    )
  )

## Agusan del Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Bayugan City"     & region_dhs == 16 & province_dhs == 3 ~ 1,
      municipality_name == "La Paz"           & region_dhs == 16 & province_dhs == 3 ~ 4,
      municipality_name == "Prosperidad"      & region_dhs == 16 & province_dhs == 3 ~ 6,
      municipality_name == "Rosario"          & region_dhs == 16 & province_dhs == 3 ~ 7,
      municipality_name == "San Francisco"    & region_dhs == 16 & province_dhs == 3 ~ 8,
      municipality_name == "San Luis"         & region_dhs == 16 & province_dhs == 3 ~ 9,
      municipality_name == "Esperanza"        & region_dhs == 16 & province_dhs == 3 ~ 3,
      municipality_name == "Santa Josefa"     & region_dhs == 16 & province_dhs == 3 ~ 10,
      TRUE ~ municipality_dhs
    )
  )

## Surigao del Norte
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 783 ~ "Sison",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Dapa"            & region_dhs == 16 & province_dhs == 67 ~ 7,
      municipality_name == "Del Carmen"      & region_dhs == 16 & province_dhs == 67 ~ 8,
      municipality_name == "General Luna"    & region_dhs == 16 & province_dhs == 67 ~ 10,
      municipality_name == "Surigao City"    & region_dhs == 16 & province_dhs == 67 ~ 24,
      municipality_name == "Placer"          & region_dhs == 16 & province_dhs == 67 ~ 17,
      municipality_name == "Tubod"           & region_dhs == 16 & province_dhs == 67 ~ 27,
      municipality_name == "Mainit"          & region_dhs == 16 & province_dhs == 67 ~ 14,
      municipality_name == "Sison"           & region_dhs == 16 & province_dhs == 67 ~ 22,
      TRUE ~ municipality_dhs
    )
  )

## Surigao del Sur
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 787 ~ "Bayabas",
      year == 2008 & cluster == 793 ~ "Hinatuan",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Bislig"       & region_dhs == 16 & province_dhs == 68 ~ 3,
      municipality_name == "Cagwait"      & region_dhs == 16 & province_dhs == 68 ~ 4,
      municipality_name == "Cantilan"     & region_dhs == 16 & province_dhs == 68 ~ 5,
      municipality_name == "Madrid"       & region_dhs == 16 & province_dhs == 68 ~ 13,
      municipality_name == "Lanuza"       & region_dhs == 16 & province_dhs == 68 ~ 10,
      municipality_name == "San Miguel"   & region_dhs == 16 & province_dhs == 68 ~ 16,
      municipality_name == "Tago"         & region_dhs == 16 & province_dhs == 68 ~ 18,
      municipality_name == "Carrascal"    & region_dhs == 16 & province_dhs == 68 ~ 7,
      municipality_name == "Tandag"       & region_dhs == 16 & province_dhs == 68 ~ 19,
      municipality_name == "Hinatuan"     & region_dhs == 16 & province_dhs == 68 ~ 9,
      municipality_name == "Bayabas"      & region_dhs == 16 & province_dhs == 68 ~ 2,
      TRUE ~ municipality_dhs
    )
  )

## Dinagat Islands
dhs_geo <- dhs_geo |>
  mutate(
    municipality_name = case_when(
      year == 2008 & cluster == 780 ~ "Cagdianao",
      TRUE ~ municipality_name
    ),
    municipality_dhs = case_when(
      municipality_name == "Libjo"      & region_dhs == 16 & province_dhs == 85 ~ 4,
      municipality_name == "Cagdianao"  & region_dhs == 16 & province_dhs == 85 ~ 2,
      municipality_name == "Basilisa"   & region_dhs == 16 & province_dhs == 85 ~ 1,
      TRUE ~ municipality_dhs
    )
  )

# Region 17
## Marinduque 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Gasan"       & region_dhs == 17 & province_dhs == 40 ~ 3,
      municipality_name == "Santa Cruz"  & region_dhs == 17 & province_dhs == 40 ~ 5,
      municipality_name == "Mogpog"      & region_dhs == 17 & province_dhs == 40 ~ 4,
      TRUE ~ municipality_dhs
    )
  )

## Occidental Mindoro 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name %in% c("Abra De Iilog", "Abra De Ilog") & region_dhs == 17 & province_dhs == 51 ~ 1,
      municipality_name == "Magsaysay"     & region_dhs == 17 & province_dhs == 51 ~ 5,
      municipality_name == "Sablayan"      & region_dhs == 17 & province_dhs == 51 ~ 9,
      municipality_name == "Paluan"        & region_dhs == 17 & province_dhs == 51 ~ 7,
      municipality_name == "Mamburao"      & region_dhs == 17 & province_dhs == 51 ~ 6,
      municipality_name == "San Jose"      & region_dhs == 17 & province_dhs == 51 ~ 10,
      municipality_name == "Santa Cruz"    & region_dhs == 17 & province_dhs == 51 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Oriental Mindoro 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Baco"             & region_dhs == 17 & province_dhs == 52 ~ 1,
      municipality_name == "Bansud"           & region_dhs == 17 & province_dhs == 52 ~ 2,
      municipality_name == "Bongabong"        & region_dhs == 17 & province_dhs == 52 ~ 3,
      municipality_name == "Roxas"            & region_dhs == 17 & province_dhs == 52 ~ 12,
      municipality_name == "Pola"             & region_dhs == 17 & province_dhs == 52 ~ 10,
      municipality_name == "Victoria"         & region_dhs == 17 & province_dhs == 52 ~ 15,
      municipality_name == "Pinamalayan"      & region_dhs == 17 & province_dhs == 52 ~ 9,
      municipality_name == "Bulalacao"        & region_dhs == 17 & province_dhs == 52 ~ 4,
      municipality_name == "Calapan"          & region_dhs == 17 & province_dhs == 52 ~ 5,
      municipality_name == "Gloria"           & region_dhs == 17 & province_dhs == 52 ~ 6,
      municipality_name == "Puerto Galera"    & region_dhs == 17 & province_dhs == 52 ~ 11,
      TRUE ~ municipality_dhs
    )
  )

## Palawan 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Aborlan"              & region_dhs == 17 & province_dhs == 53 ~ 1,
      municipality_name == "Cuyo"                 & region_dhs == 17 & province_dhs == 53 ~ 10,
      municipality_name == "Puerto Princesa"      & region_dhs == 17 & province_dhs == 53 ~ 16,
      municipality_name == "Quezon"               & region_dhs == 17 & province_dhs == 53 ~ 17,
      municipality_name == "Rozxas"               & region_dhs == 17 & province_dhs == 53 ~ 18,
      municipality_name %in% c("Sofronio Espanola", "Sofronio Españaola") & region_dhs == 17 & province_dhs == 53 ~ 24,
      municipality_name == "Narra"                & region_dhs == 17 & province_dhs == 53 ~ 15,
      municipality_name == "Bataraza"             & region_dhs == 17 & province_dhs == 53 ~ 5,
      municipality_name == "Taytay"               & region_dhs == 17 & province_dhs == 53 ~ 20,
      municipality_name == "Brooke's Point"       & region_dhs == 17 & province_dhs == 53 ~ 6,
      municipality_name == "San Vicente"          & region_dhs == 17 & province_dhs == 53 ~ 19,
      TRUE ~ municipality_dhs
    )
  )

## Romblon 
dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = case_when(
      municipality_name == "Calatrava"       & region_dhs == 17 & province_dhs == 59 ~ 4,
      municipality_name == "Corcuera"        & region_dhs == 17 & province_dhs == 59 ~ 6,
      municipality_name == "Looc"            & region_dhs == 17 & province_dhs == 59 ~ 7,
      municipality_name %in% c("San Agustin", "San Augustin") & region_dhs == 17 & province_dhs == 59 ~ 11,
      municipality_name == "San Fernando"    & region_dhs == 17 & province_dhs == 59 ~ 13,
      TRUE ~ municipality_dhs
    )
  )

## Fix erroneous region code from 10 → 15 for Lanao del Sur
dhs_geo <- dhs_geo |>
  mutate(
    region_dhs = case_when(
      region_dhs == 10 & province_dhs == 36 & municipality_dhs == 10 & province_name == "Lanao del Sur" ~ 15,
      TRUE ~ region_dhs
    )
  )

# Dataset is called dhs_all and has `region` and `survey_year`
dhs_geo <- dhs_geo |>
  mutate(
    region = case_when(
      year == 2013 & region_dhs == 3  ~ 1,
      year == 2013 & region_dhs == 4  ~ 2,
      year == 2013 & region_dhs == 5  ~ 3,
      year == 2013 & region_dhs == 6  ~ 4,
      year == 2013 & region_dhs == 8  ~ 5,
      year == 2013 & region_dhs == 9  ~ 6,
      year == 2013 & region_dhs == 10 ~ 7,
      year == 2013 & region_dhs == 11 ~ 8,
      year == 2013 & region_dhs == 12 ~ 9,
      year == 2013 & region_dhs == 13 ~ 10,
      year == 2013 & region_dhs == 14 ~ 11,
      year == 2013 & region_dhs == 15 ~ 12,
      year == 2013 & region_dhs == 1  ~ 13,
      year == 2013 & region_dhs == 2  ~ 14,
      year == 2013 & region_dhs == 17 ~ 15,
      year == 2013 & region_dhs == 16 ~ 16,
      year == 2013 & region_dhs == 7  ~ 17
    )
  )

dhs_geo <- dhs_geo |>
  mutate(
    municipality_dhs = if_else(province_dhs == 39, 99, municipality_dhs),
    province_dhs     = if_else(province_dhs == 98 & municipality_dhs == 4, 38, province_dhs),
    region_dhs       = if_else(province_dhs == 38 & municipality_dhs == 4, 15, region_dhs)
  )

# Apply manual corrections directly to dhs_geo and keep only 1993, 1998, 2013
dhs_geo <- dhs_geo %>%
  mutate(
    # 2013 corrections
    municipality_dhs = if_else(year == 2013 & province_dhs == 39, 99, municipality_dhs),
    province_dhs     = if_else(year == 2013 & province_dhs == 98 & municipality_dhs == 4, 38, province_dhs),
    region_dhs       = if_else(year == 2013 & province_dhs == 38 & municipality_dhs == 4, 15, region_dhs),
    
    # 1993 + 1998 region corrections
    region_dhs = case_when(
      year %in% c(1993, 1998) & province_dhs %in% c(40, 53, 59, 52, 51) ~ 17,
      year %in% c(1993, 1998) & province_dhs == 77 ~ 3,
      year %in% c(1993, 1998) & province_dhs == 7 ~ 15,
      year %in% c(1993, 1998) & province_dhs %in% c(63, 80) ~ 12,
      year %in% c(1993, 1998) & province_dhs == 35 ~ 10,
      year %in% c(1993, 1998) & province_dhs == 36 & municipality_dhs == 17 ~ 15,
      year %in% c(1993, 1998) & province_dhs == 38 & municipality_dhs == 4 ~ 15,
      TRUE ~ region_dhs
    ),
    
    # 1993 + 1998 province corrections
    province_dhs = case_when(
      year %in% c(1993, 1998) & province_dhs == 73 & municipality_dhs %in% c(4, 9, 10, 16, 26, 31, 36) ~ 83,
      year %in% c(1993, 1998) & province_dhs == 23 & municipality_dhs %in% c(4, 8, 9, 10, 16) ~ 82,
      year %in% c(1993, 1998) & province_dhs == 67 & municipality_dhs %in% c(3, 5) ~ 85,
      year %in% c(1993, 1998) & province_dhs == 98 & municipality_dhs == 17 ~ 36,
      year %in% c(1993, 1998) & province_dhs == 98 & municipality_dhs == 4 ~ 38,
      TRUE ~ province_dhs
    ),
    
    # 1993 + 1998 municipality corrections
    municipality_dhs = case_when(
      year %in% c(1993, 1998) & province_dhs == 83 & municipality_dhs == 26 ~ 13,  # Siay
      year %in% c(1993, 1998) & province_dhs == 83 & municipality_dhs == 31 ~ 16,  # Tungawan
      year %in% c(1993, 1998) & province_dhs == 83 & municipality_dhs == 36 ~ 4,   # Imelda
      year %in% c(1993, 1998) & province_dhs == 73 & municipality_dhs == 42 ~ 15,  # Mahayag
      year %in% c(1993, 1998) & province_dhs == 82 & municipality_dhs == 16 ~ 11,  # Pantukan
      year %in% c(1993, 1998) & province_dhs == 62 & municipality_dhs == 1 ~ 16,
      year %in% c(1993, 1998) & province_dhs == 39 ~ 99,
      TRUE ~ municipality_dhs
    )
  ) %>%
  distinct(year, cluster, .keep_all = TRUE)

# Save result
write_dta(dhs_geo, here("data-analysis", "clean-data", "1993-2013_manual_geocodes.dta"))
write_csv(dhs_geo, here("data-analysis", "clean-data", "1993-2013_manual_geocodes.csv"))


# 1. Extract unique year-region-province-municipality triplets
geo_crosswalk <- dhs_geo %>%
  filter(year %in% 1993:2013) %>%  # include 1993–2013
  select(year, region_dhs, province_dhs, municipality_dhs,
         admin_region_name, province_name, municipality_name) %>%
  distinct() %>%
  arrange(region_dhs, province_dhs, municipality_dhs, year)

# 2. Optional: Make sure names are in Title Case
geo_crosswalk <- geo_crosswalk %>%
  mutate(
    admin_region_name = stringr::str_to_title(admin_region_name),
    province_name      = stringr::str_to_title(province_name),
    municipality_name  = stringr::str_to_title(municipality_name)
  )

# "Due to the absence of GPS cluster coordinates for the 1993 and 1998 DHS, we used 
# 2003 GPS data as a spatial proxy, assuming relative geographic stability of cluster 
# locations. Similarly, 2008 GPS coordinates were used for 2013 clusters."


# 1. Prep dhs_geo: Keep only new geo columns to merge in ======================
geo_vars <- dhs_geo |> 
  select(
    survey_year = year,
    cluster_id = cluster,
    region_dhs,
    admin_region_name,
    province_dhs,
    province_name,
    municipality_dhs,
    municipality_name,
    barangay_name,
    latitude,
    longitude,
    altitude_gps,
    altitude_dem,
    cc_fips,
    source,
    urban_rural,
    datum
  )

# 2. Join geo variables to the full harmonized dataset ========================
dhs_all <- dhs_all |>
  left_join(geo_vars, by = c("survey_year", "cluster_id"))

saveRDS(dhs_all, here("data-analysis", "clean-data", "dhs_kr_children_with_geo.rds"))

# Furthermore, for now we focus on 2003-2008 data preliminarily.

# Filter data for the years 2023-2028 and rows where 'municipality_name' is missing
missing_data_2003_2008 <- dhs_all |>
  filter(survey_year >= 2003 & survey_year <= 2008) |>
  filter(is.na(municipality_name))

# Ensure the outputs folder exists
dir.create(here("outputs"), showWarnings = FALSE, recursive = TRUE)

# Output the filtered data
write.csv(
  missing_data_2003_2008,
  here("outputs", "missing_data_2003_2008_municipality.csv"),
  row.names = FALSE
)
