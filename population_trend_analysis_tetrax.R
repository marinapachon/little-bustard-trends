# --------------------------------------------
# Population Trend Analysis - Little Bustard (Tetrax tetrax)
# Author: Marina Pachón Mena
#
# Description:
# This script performs statistical trend analyses (Mann-Kendall test and Sen's slope)
# on population data of the Little Bustard using maximum yearly counts per UTM grid cell.
# It applies the mkac package to detect significant trends and exports the results
# to GIS and Excel-compatible formats.
# --------------------------------------------

######### LOAD PACKAGES #########

library(dplyr)
library(lubridate)
library(mkac)
library(trend)
library(ggplot2)
library(readxl)
library(writexl)
library(sf)
library(tidyverse)

######### LOAD CENSUS DATA #########

# Load maximum count per cell-year (previously prepared)
census_sison <- st_read("inputs/censo_gis/censo_tetrax_all_max_visita.gpkg")

######### MANN-KENDALL TEST BY CELL + ID #########

# Initialize empty results dataframe
results_mkac <- data.frame(
  cuadricula = character(),
  id = numeric(),
  tau = numeric(),
  p_value = numeric(),
  pendiente_sen = numeric(),
  conteo_max = numeric(),
  conteo_min = numeric(),
  numero_anos = numeric()
)

# Get unique grid-ID combinations
combinations <- unique(census_sison[, c("cuadricula", "id")])

# Loop through combinations
for (i in 1:nrow(combinations)) {
  
  subset_data <- census_sison %>%
    filter(
      cuadricula == combinations$cuadricula[i],
      id == combinations$id[i]
    )
  
  # Apply tests only if >2 data points
  if (nrow(subset_data) > 2) {
    
    subset_data$ano <- as.numeric(as.character(subset_data$ano))
    
    subset_data <- subset_data %>%
      filter(!is.na(individuos) & !is.na(ano))
    
    if (nrow(subset_data) > 2) {
      
      # Mann-Kendall test (with Z adjustment)
      kendall_result <- kendall_Z_adjusted(subset_data$individuos)
      
      # Sen's slope estimation
      theil_sen_result <- theil_sen_slope(
        y = subset_data$individuos, 
        x = subset_data$ano  
      )
      
      # Append results
      results_mkac <- results_mkac %>%
        add_row(
          cuadricula = combinations$cuadricula[i],
          id = combinations$id[i],
          tau = kendall_result$z_star,
          p_value = kendall_result$P_value_adj,
          pendiente_sen = theil_sen_result,
          conteo_max = max(subset_data$individuos, na.rm = TRUE),
          conteo_min = min(subset_data$individuos, na.rm = TRUE),
          numero_anos = length(unique(subset_data$ano))
        )
    }
  }
}

######### ADD ATTRIBUTES AND GEOMETRY #########

# Extract attributes from first row per grid-ID
attributes_df <- census_sison %>%
  group_by(cuadricula, id) %>%
  slice(1) %>%
  ungroup() %>%
  select(cuadricula, id, provincia, ccaa, provincias_asociadas,
         n_provincias, ano_inicio, ano_fin, geom)

# Join with trend results and convert to sf
results_mkac <- results_mkac %>%
  left_join(attributes_df, by = c("cuadricula", "id")) %>%
  st_as_sf()

# Filter out invalid results
results_mkac <- results_mkac %>%
  filter(!is.nan(tau), !is.nan(p_value), !is.nan(pendiente_sen))

# Add significance and trend direction
results_mkac <- results_mkac %>%
  mutate(
    significancia = ifelse(p_value <= 0.05, "yes", "no"),
    tendencia_tau = case_when(
      tau > 0 ~ "positive",
      tau < 0 ~ "negative",
      TRUE ~ "no trend"
    ),
    tendencia_sen = case_when(
      pendiente_sen > 0 ~ "positive",
      pendiente_sen < 0 ~ "negative",
      TRUE ~ "no trend"
    )
  ) %>%
  relocate(significancia, .after = p_value) %>% 
  relocate(tendencia_tau, .after = tau) %>% 
  relocate(tendencia_sen, .after = pendiente_sen)

######### EXPORT RESULTS #########

# Full results
write_xlsx(st_drop_geometry(results_mkac), "inputs/all/max/resultados_mkac_all_max.xlsx")
st_write(results_mkac, "inputs/censo_gis/resultados_mkac_all_max.gpkg", layer = "datos_censo", delete_layer = TRUE)

# Significant trends only (p ≤ 0.05)
results_signif <- results_mkac %>%
  filter(p_value <= 0.05)

write_xlsx(st_drop_geometry(results_signif), "inputs/all/max/resultados_mkac_significativos_all_max_visita.xlsx")
st_write(results_signif, "inputs/censo_gis/resultados_mkac_significativos_all_max_visita.gpkg",
         layer = "datos_censo", delete_layer = TRUE)

# Non-significant trends (p > 0.05)
results_nonsig <- results_mkac %>%
  filter(p_value > 0.05)

write_xlsx(st_drop_geometry(results_nonsig), "inputs/all/max/resultados_mkac_NOSIG_all_max_visita.xlsx")
st_write(results_nonsig, "inputs/censo_gis/resultados_mkac_NOSIG_all_max_visita.gpkg",
         layer = "datos_censo", delete_layer = TRUE)

