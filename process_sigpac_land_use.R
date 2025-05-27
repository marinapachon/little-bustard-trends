# ------------------------------
# SIGPAC Land Use Processing Script
# Author: Marina Pachón Mena
# Description: 
#   This script processes annual SIGPAC land use data for any Spanish province.
#   To adapt it, simply replace the folder and file names referring to the province (e.g. "toledo") 
#   with the desired province name in the input/output paths.
#
#   The script reads parcel geometries (.shp) and their attributes (.dbf),
#   intersects them with a 10x10 km grid (from a GeoPackage),
#   and exports the filtered, spatially joined results as GeoPackage (.gpkg) and Excel (.xlsx) files.
# ------------------------------

# Required packages
library(sf)
library(dplyr)
library(readr)
library(writexl)
library(tools)
library(foreign)

# Define paths
input_base_path <- "SIGPAC/inputs/toledo"
output_path <- "SIGPAC/outputs/all/toledo"
grid_path <- "SIGPAC/inputs/all_cells/10X10_toledo.gpkg"

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Initialize error log file
error_log <- file(file.path(output_path, "error_log.txt"), open = "wt")

# Load 10x10 km grid
grid_10x10 <- st_read(grid_path) %>%
  select(CUADRICULA)

# List annual folders (by year)
annual_folders <- list.dirs(path = input_base_path, full.names = TRUE, recursive = FALSE)

# Loop through each year
for (year_path in annual_folders) {
  
  year <- gsub("^.*/", "", year_path)
  cat("📁 Processing year:", year, "\n")
  
  tryCatch({
    
    # Read parcel geometry (RECFE)
    parcels <- st_read(file.path(year_path, "RECFE.shp"), quiet = TRUE)
    
    # Read attribute table (ATRRE)
    attributes <- read.dbf(file.path(year_path, "ATRRE.dbf"))
    
    # Join key fields
    attributes_filtered <- attributes %>%
      distinct(DN_OID, .keep_all = TRUE) %>%
      select(DN_OID, COEF_REGAD, USO_2003, USO_SIGPAC)
    
    parcels_with_attributes <- parcels %>%
      semi_join(attributes_filtered, by = "DN_OID") %>%
      left_join(attributes_filtered, by = "DN_OID")
    
    # Fix invalid geometries
    parcels_valid <- st_make_valid(parcels_with_attributes)
    
    # Clip parcels to the 10x10 km grid
    parcels_clipped <- st_intersection(parcels_valid, st_geometry(grid_10x10))
    
    # Spatial join to assign grid cell ID
    parcels_with_grid <- st_join(parcels_clipped, grid_10x10, join = st_intersects, left = FALSE)
    
    # Add area column
    final_output <- parcels_with_grid %>%
      mutate(area_m2 = st_area(.))
    
    # Export results
    gpkg_file <- file.path(output_path, paste0("toledo", year, ".gpkg"))
    xlsx_file <- file.path(output_path, paste0("toledo", year, ".xlsx"))
    
    st_write(final_output, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
    write_xlsx(st_drop_geometry(final_output), xlsx_file)
    
    cat("✅ Year", year, "completed.\n\n")
    
  }, error = function(e) {
    # ❌ Log any error
    cat("❌ Error in year", year, ":", conditionMessage(e), "\n")
    writeLines(paste0("Error in year ", year, ": ", conditionMessage(e)), error_log)
  })
}

# 🏁 Done!
cat("🚀 All years processed successfully.\n")

# Land use files are heavy... I'm going to sleep 😴
cat("💤 The system will shut down in 1 minute...\n")
system("shutdown /s /t 60") #