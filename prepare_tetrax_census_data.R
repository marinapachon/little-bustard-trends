# --------------------------------------------
# Prepare Tetrax Census Data (Georeferenced and Standardized)
# Author: Marina Pachón Mena
#
# Description:
# This script prepares a spatially explicit dataset to analyze population 
# trends of the Little Bustard (Tetrax tetrax) in Spain. It joins census records 
# with a 10x10 km UTM grid, assigns provincial and regional identities to each cell, 
# and calculates survey effort metrics. Outputs include spatial and tabular formats 
# for further statistical or GIS analysis.
# --------------------------------------------

######### LOAD PACKAGES #########

library(sf)
library(dplyr)
library(writexl)
library(stringr)
library(lubridate)

######### LOAD SPATIAL AND TABULAR DATA #########

# Load UTM grid and administrative boundaries
utm_grid <- st_read("../gis/inputs/Malla10x10/Malla10x10_Ter_p.shp") %>%
  rename(cuadricula = CUADRICULA)

regions_utm <- st_read("../gis/inputs/Limites/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp")
provinces_utm <- st_read("../gis/inputs/Limites/SHP_ETRS89/recintos_provinciales_inspire_peninbal_etrs89/recintos_provinciales_inspire_peninbal_etrs89.shp")

# Load census data
census_raw <- read.csv("inputs/censo_tetrax_initial.csv", header = TRUE, sep = ";") %>%
  mutate(fecha = as.Date(fecha, origin = "1899-12-30")) %>%
  rename(individuos = total)

######### JOIN CENSUS TO GRID #########

# Join census with UTM grid
census <- utm_grid %>%
  left_join(census_raw, by = "cuadricula") %>%
  filter(!is.na(individuos))  # Keep only records with data

# Ensure all layers share the same CRS
provinces_utm <- st_transform(provinces_utm, st_crs(census))
regions_utm <- st_transform(regions_utm, st_crs(census))

######### ASSIGN DOMINANT PROVINCE AND REGION #########

# Province by majority overlap
prov_dominant <- st_intersection(utm_grid, provinces_utm) %>%
  mutate(area = st_area(.)) %>%
  group_by(cuadricula) %>%
  slice_max(order_by = area, n = 1) %>%
  ungroup() %>%
  select(cuadricula, provincia = NAMEUNIT) %>%
  st_drop_geometry()

# Region by majority overlap
region_dominant <- st_intersection(utm_grid, regions_utm) %>%
  mutate(area = st_area(.)) %>%
  group_by(cuadricula) %>%
  slice_max(order_by = area, n = 1) %>%
  ungroup() %>%
  select(cuadricula, ccaa = NAMEUNIT) %>%
  st_drop_geometry()

# Join dominant province and region
census <- census %>%
  left_join(prov_dominant, by = "cuadricula") %>%
  left_join(region_dominant, by = "cuadricula")

######### CLEAN AND STANDARDIZE NAMES #########

census <- census %>%
  mutate(
    provincia = provincia %>%
      str_to_lower() %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
      str_replace_all("alacant/alicante", "alicante") %>% 
      str_replace_all("ciudad real", "ciudad_real") %>%  
      str_replace_all("la rioja", "la_rioja"),
    
    ccaa = ccaa %>%
      str_to_lower() %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
      str_replace_all("castilla y leon", "castilla_y_leon") %>%  
      str_replace_all("comunidad de madrid", "madrid") %>%  
      str_replace_all("castilla-la mancha", "castilla_la_mancha") %>% 
      str_replace_all("cataluña/catalunya", "catalunya") %>% 
      str_replace_all("region de murcia", "murcia") %>% 
      str_replace_all("comunidad floral de navarra", "navarra") %>% 
      str_replace_all("la rioja", "la_rioja") %>% 
      str_replace_all("comunitat valenciana", "comunitat_valenciana") %>%
      str_replace_all("cataluna/catalunya", "catalunya") %>%
      str_replace_all("comunidad foral de navarra", "navarra")
  )

######### ASSIGN MULTI-PROVINCE INFORMATION #########

provincias_info <- st_intersection(utm_grid, provinces_utm) %>%
  group_by(cuadricula) %>%
  summarise(
    provincias_asociadas = paste(unique(NAMEUNIT), collapse = ", "),
    n_provincias = n_distinct(NAMEUNIT),
    .groups = "drop"
  ) %>%
  st_drop_geometry() %>%
  mutate(
    provincias_asociadas = provincias_asociadas %>%
      str_to_lower() %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  )

# Join multi-province info
census <- left_join(census, provincias_info, by = "cuadricula")

######### ADD YEAR, VISIT AND SAMPLING METRICS #########

census <- census %>%
  mutate(ano = year(fecha)) %>%
  group_by(provincia, cuadricula, id, ano) %>%
  arrange(fecha, .by_group = TRUE) %>%
  mutate(visita = row_number()) %>%
  ungroup()

census_years <- census %>%
  group_by(cuadricula, id) %>%
  summarise(
    ano_inicio = min(ano, na.rm = TRUE),
    ano_fin = max(ano, na.rm = TRUE),
    numero_anos = n_distinct(ano),
    duracion_periodo = ano_fin - ano_inicio + 1,
    .groups = "drop"
  )

census <- left_join(census, st_drop_geometry(census_years), by = c("cuadricula", "id"))

######### SELECT RELEVANT COLUMNS #########

census <- census %>%
  select(
    cuadricula,
    id,
    fecha,
    individuos,
    provincia,
    ccaa,
    geometry,
    provincias_asociadas,
    n_provincias,
    ano,
    visita,
    ano_inicio,
    ano_fin,
    numero_anos,
    duracion_periodo
  )

######### EXPORT RESULTS #########

write.table(census, "inputs/censo_tetrax_all.csv", row.names = FALSE, sep = ";")
write_xlsx(census, "inputs/censo_tetrax_all.xlsx")
st_write(census, "inputs/censo_gis/censo_tetrax.gpkg", layer = "datos_censo", delete_layer = TRUE)

######### EXTRACT MAXIMUM COUNT PER GRID-ID-YEAR #########

census_gis <- st_read("inputs/censo_gis/censo_tetrax.gpkg")

census_max_visita <- census_gis %>%
  group_by(cuadricula, id, ano) %>%
  slice_max(order_by = individuos, n = 1, with_ties = FALSE) %>%
  ungroup()

write_xlsx(census_max_visita, "inputs/censo_tetrax_all_max_visita.xlsx")
st_write(census_max_visita, "inputs/censo_gis/censo_tetrax_all_max_visita.gpkg",
         layer = "censo_max_individuos", delete_layer = TRUE)
