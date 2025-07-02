# ------------------------------------------------------------------------------
# LAND USE PROCESSING AND SUMMARY BY PROVINCE (SIGPAC DATA)
# ------------------------------------------------------------------------------

# Purpose:
# This script processes and summarizes annual land use data from the SIGPAC system
# for selected provinces in Spain. Each input file is a GeoPackage (.gpkg) containing
# spatial and attribute information on agricultural and non-agricultural land use.

# Workflow:
# 1. List and read all .gpkg files from a given directory.
# 2. Extract the year from each filename and add it as a column.
# 3. Select relevant fields: grid ID (CUADRICULA), SIGPAC use (USO_SIGPAC),
#    historical use (USO_2003), irrigation coefficient (COEF_REGAD), and area (area_m2).
# 4. Classify each polygon as irrigated or rainfed based on COEF_REGAD (> 50).
# 5. Aggregate total area (m²) by grid, year, land use, historical use, and irrigation type.
# 6. Export a summary table to Excel for further analysis.

# Output:
# - An Excel file named `provincia_resumen.xlsx`, with total area (m²) summarized by:
#   - Year
#   - 10×10 km grid cell (CUADRICULA)
#   - SIGPAC land use category (USO_SIGPAC)
#   - 2003 land use (USO_2003)
#   - Irrigation type (regadio, secano, otros)

# Required packages:
# - sf         → for reading .gpkg spatial files
# - dplyr      → for data manipulation and aggregation
# - stringr    → for extracting year information from filenames
# - writexl    → for exporting tables to .xlsx format

# ------------------------------------------------------------------------------

# Load required libraries
library(sf)
library(dplyr)
library(stringr)
library(writexl)

# List all GPKG files for the province of Toledo
files <- list.files("SIGPAC/outputs/all/toledo/", pattern = "\\.gpkg$", full.names = TRUE)

# Read all files and combine them into a single dataframe
usos <- lapply(files, function(f) {
  year <- as.integer(str_extract(basename(f), "\\d{4}"))
  st_read(f, quiet = TRUE) %>%
    mutate(
      ano = year,
      COEF_REGAD = as.numeric(COEF_REGAD),
      area_m2 = as.numeric(area_m2)
    ) %>%
    select(CUADRICULA, ano, COEF_REGAD, area_m2, USO_SIGPAC, USO_2003)
}) %>% bind_rows()

# Classify each polygon as irrigated, rainfed or other
usos <- usos %>%
  mutate(tipo_riego = case_when(
    COEF_REGAD > 50 ~ "regadio",
    COEF_REGAD <= 50 ~ "secano",
    TRUE ~ "otros"
  ))

# Aggregate area by grid, year, irrigation type, and land use
tabla_usos <- usos %>%
  group_by(CUADRICULA, ano, tipo_riego, USO_SIGPAC, USO_2003) %>%
  summarise(
    area_total_m2 = sum(area_m2, na.rm = TRUE),
    .groups = "drop"
  )

# Export the summary table to Excel
write_xlsx(tabla_usos, "SIGPAC/outputs/all/toledo/toledo_resumen.xlsx")