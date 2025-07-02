# ------------------------------------------------------------------------------
# LAND USE TREND ANALYSIS BY GRID CELL (SIGPAC)
# ------------------------------------------------------------------------------

# Purpose:
# This script analyzes temporal trends in land use (USO_SIGPAC) across grid cells
# using the Mann-Kendall adjusted test (MKAC) and Theil-Sen slope estimation.
# It identifies significant increases or decreases in land cover across years
# without distinguishing irrigation type (done in a separate script).

# Workflow:
# 1. Read input Excel with annual surface summaries (from script: resumen_provincia_ano).
# 2. Aggregate area by CUADRICULA, USO_SIGPAC and year.
# 3. Apply MKAC test and Theil-Sen slope to each unique grid + land use combination.
# 4. Create summary table including tau, p-value, slope, significance and trend direction.
# 5. Export full and filtered (significant only) results to Excel.

# Output:
# - mkac_provincia_usos.xlsx
# - mkac_provincia_usos_significativos.xlsx

# Required packages
library(dplyr)
library(mkac)
library(readxl)
library(writexl)

# Load summary table
tabla_usos <- read_excel("SIGPAC/outputs/all/guadalajara/guadalajara_resumen.xlsx")

# Aggregate by grid, land use, year
tabla_agregada <- tabla_usos %>%
  group_by(CUADRICULA, USO_SIGPAC, ano) %>%
  summarise(area_total_m2 = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Unique combinations to test
combinaciones <- unique(tabla_agregada[, c("CUADRICULA", "USO_SIGPAC")])

# Results dataframe
resultados_mkac <- data.frame(
  CUADRICULA = character(),
  USO_SIGPAC = character(),
  tipo_riego = character(),
  tau = numeric(),
  p_value = numeric(),
  pendiente_sen = numeric()
)

# Apply MKAC + Sen slope
for(i in 1:nrow(combinaciones)) {
  datos_sub <- tabla_agregada %>%
    filter(CUADRICULA == combinaciones$CUADRICULA[i],
           USO_SIGPAC == combinaciones$USO_SIGPAC[i])
  
  if(nrow(datos_sub) > 2) {
    datos_sub$ano <- as.numeric(as.character(datos_sub$ano))
    datos_sub <- datos_sub %>% filter(!is.na(area_total_m2) & !is.na(ano))
    
    if(nrow(datos_sub) > 2) {
      kendall_result <- kendall_Z_adjusted(datos_sub$area_total_m2)
      theil_sen_result <- theil_sen_slope(datos_sub$area_total_m2, datos_sub$ano)
      resultados_mkac <- resultados_mkac %>% add_row(
        CUADRICULA = combinaciones$CUADRICULA[i],
        USO_SIGPAC = combinaciones$USO_SIGPAC[i],
        tau = kendall_result$z_star,
        p_value = kendall_result$P_value_adj,
        pendiente_sen = theil_sen_result
      )
    }
  }
}

# Add significance and direction labels
resultados_mkac <- resultados_mkac %>%
  mutate(
    significancia = ifelse(p_value <= 0.05, "si", "no"),
    tendencia_tau = case_when(tau > 0 ~ "positiva", tau < 0 ~ "negativa", TRUE ~ "igual"),
    tendencia_sen = case_when(pendiente_sen > 0 ~ "positiva", pendiente_sen < 0 ~ "negativa", TRUE ~ "igual")
  ) %>%
  select(CUADRICULA, USO_SIGPAC, tipo_riego, tau, tendencia_tau, p_value,
         significancia, pendiente_sen, tendencia_sen, everything())

# Export results
write_xlsx(resultados_mkac, "SIGPAC/outputs/all/guadalajara/mkac_guadalajara_usos.xlsx")

# Export only significant
resultados_mkac_significativos <- resultados_mkac %>% filter(significancia == "si")
write_xlsx(resultados_mkac_significativos, "SIGPAC/outputs/all/guadalajara/mkac_guadalajara_usos_significativos.xlsx")

# ------------------------------------------------------------------------------
# IRRIGATION-BASED TREND ANALYSIS
# ------------------------------------------------------------------------------

# Purpose:
# This section applies MKAC and Theil-Sen tests to detect trends in land use area
# based on irrigation type (regadio/secano/otros) across grid cells and years.

# Workflow:
# 1. Load summary Excel file with annual surfaces by grid and irrigation type.
# 2. Aggregate surface by CUADRICULA, irrigation type, and year.
# 3. Apply MKAC and Theil-Sen to each unique combination.
# 4. Compile results including tau, p-value, slope, and trend direction.
# 5. Export full and significant-only results.

# Output:
# - mkac_provincia_riego.xlsx
# - mkac_provincia_riego_significativos.xlsx

# Load irrigation summary
tabla_usos <- read_excel("SIGPAC/outputs/all/toledo/toledo_resumen.xlsx")

tabla_agregada <- tabla_usos %>%
  group_by(CUADRICULA, tipo_riego, ano) %>%
  summarise(area_total_m2 = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

combinaciones <- unique(tabla_agregada[, c("CUADRICULA", "tipo_riego")])

resultados_mkac <- data.frame(
  CUADRICULA = character(),
  USO_SIGPAC = character(),
  tau = numeric(),
  p_value = numeric(),
  pendiente_sen = numeric()
)

for(i in 1:nrow(combinaciones)) {
  datos_sub <- tabla_agregada %>%
    filter(CUADRICULA == combinaciones$CUADRICULA[i], tipo_riego == combinaciones$tipo_riego[i])
  
  if(nrow(datos_sub) > 2) {
    datos_sub$ano <- as.numeric(as.character(datos_sub$ano))
    datos_sub <- datos_sub %>% filter(!is.na(area_total_m2) & !is.na(ano))
    
    if(nrow(datos_sub) > 2) {
      kendall_result <- kendall_Z_adjusted(datos_sub$area_total_m2)
      theil_sen_result <- theil_sen_slope(datos_sub$area_total_m2, datos_sub$ano)
      resultados_mkac <- resultados_mkac %>% add_row(
        CUADRICULA = combinaciones$CUADRICULA[i],
        USO_SIGPAC = combinaciones$tipo_riego[i],
        tau = kendall_result$z_star,
        p_value = kendall_result$P_value_adj,
        pendiente_sen = theil_sen_result
      )
    }
  }
}

# Labeling results
resultados_mkac <- resultados_mkac %>%
  mutate(tipo_riego = NA_character_) %>%
  mutate(
    significancia = ifelse(p_value <= 0.05, "si", "no"),
    tendencia_tau = case_when(tau > 0 ~ "positiva", tau < 0 ~ "negativa", TRUE ~ "igual"),
    tendencia_sen = case_when(pendiente_sen > 0 ~ "positiva", pendiente_sen < 0 ~ "negativa", TRUE ~ "igual")
  ) %>%
  select(CUADRICULA, USO_SIGPAC, tipo_riego, tau, tendencia_tau, p_value,
         significancia, pendiente_sen, tendencia_sen)

write_xlsx(resultados_mkac, "SIGPAC/outputs/all/toledo/mkac_toledo_riego.xlsx")

resultados_mkac_significativos <- resultados_mkac %>% filter(significancia == "si")
write_xlsx(resultados_mkac_significativos, "SIGPAC/outputs/all/toledo/mkac_toledo_riego_significativos.xlsx")