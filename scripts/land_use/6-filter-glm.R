# ===============================================================
# FILTERING LAND USE DATA FOR GLOBAL TREND ANALYSIS
# ===============================================================

# This script applies spatial and temporal filtering criteria
# to SIGPAC land use data, including irrigation type categories.

# 1. BLOCK 1: FILTERING BY REPRESENTATIVENESS AND VARIATION
#    - Calculates annual percentage of land occupation by land use
#      and irrigation type (irrigated, rainfed, others).
#    - Retains only those combinations of CUADRICULA + USO_SIGPAC
#      with:
#        * maximum annual area > 1%,
#        * and temporal variation > 1 percentage point.
#    - Output: 'resumen_total_todas_provincias_criterios.xlsx'

# 2. BLOCK 2: FILTERING BY REPRESENTATIVENESS AND SIGNIFICANCE (with MK)
#    - Uses:
#        * Mann-Kendall results: 'significativos_total_todas_provincias.xlsx'
#        * Complete land use data: 'resumen_total_todas_provincias.xlsx'
#    - Computes mean % area per use and grid cell (media_pct).
#    - Retains only:
#        * statistically significant uses (p < 0.05),
#        * with mean occupation ≥ 1%.
#    - Only "irrigated" is considered from the irrigation types.
#    - Output: 'significativos_total_todas_provincias_MK.xlsx'

# Notes:
# - Only the "irrigated" category is retained for irrigation-based uses.
# - "Rainfed" and "others" are excluded from final output, 
#   but included in intermediate steps to compute relative shares.

# ===============================================================
# Load required libraries
library(readxl)
library(dplyr)
library(writexl)

# Read input data
resumen <- read_excel("SIGPAC/outputs/all/resumen_total_todas_provincias.xlsx")
significativos <- read_excel("SIGPAC/outputs/all/significativos_total_todas_provincias.xlsx")

# ===============================================================
# BLOCK 1: FILTERING BASED ON REPRESENTATIVENESS + TEMPORAL VARIATION
# ===============================================================

# Separate land use types from irrigation types
resumen_usos <- resumen %>% filter(!USO_SIGPAC %in% c("regadio", "secano", "otros"))
resumen_riego <- resumen %>% filter(USO_SIGPAC %in% c("regadio", "secano", "otros"))

# ----- LAND USE TYPES -----

# Total area per grid cell and year
ocupacion_usos <- resumen_usos %>%
  group_by(CUADRICULA, ano) %>%
  summarise(total_usos = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Percentage of area by use
resumen_usos_pct <- resumen_usos %>%
  left_join(ocupacion_usos, by = c("CUADRICULA", "ano")) %>%
  mutate(pct = 100 * area_total_m2 / total_usos)

# Calculate variation by use
variacion_usos <- resumen_usos_pct %>%
  group_by(CUADRICULA, USO_SIGPAC) %>%
  summarise(
    max_pct = max(pct, na.rm = TRUE),
    min_pct = min(pct, na.rm = TRUE),
    delta = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  filter(max_pct >= 1 & delta >= 1)

# Filter land use table
resumen_usos_final <- resumen_usos_pct %>%
  semi_join(variacion_usos, by = c("CUADRICULA", "USO_SIGPAC"))

# ----- IRRIGATION TYPES -----

# Total area per grid cell and year
ocupacion_riego <- resumen_riego %>%
  group_by(CUADRICULA, ano) %>%
  summarise(total_riego = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Percentage of area by irrigation type
resumen_riego_pct <- resumen_riego %>%
  left_join(ocupacion_riego, by = c("CUADRICULA", "ano")) %>%
  mutate(pct = 100 * area_total_m2 / total_riego)

# Calculate variation by irrigation type
variacion_riego <- resumen_riego_pct %>%
  group_by(CUADRICULA, USO_SIGPAC) %>%
  summarise(
    max_pct = max(pct, na.rm = TRUE),
    min_pct = min(pct, na.rm = TRUE),
    delta = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  filter(max_pct >= 1 & delta >= 1)

# Filter irrigation table
resumen_riego_final <- resumen_riego_pct %>%
  semi_join(variacion_riego, by = c("CUADRICULA", "USO_SIGPAC"))

# ----- FINAL JOIN -----

resumen_final <- bind_rows(resumen_usos_final, resumen_riego_final)

# Exclude rainfed and others from final output
resumen_final <- resumen_final %>%
  filter(USO_SIGPAC != "secano", USO_SIGPAC != "otros")

# Export
write_xlsx(resumen_final, "SIGPAC/outputs/all/resumen_total_todas_provincias_criterios.xlsx")

# ===============================================================
# BLOCK 2: FILTERING SIGNIFICANT RESULTS (MK + REPRESENTATIVENESS)
# ===============================================================

# Separate again
resumen_usos <- resumen %>% filter(!USO_SIGPAC %in% c("regadio", "secano", "otros"))
resumen_riego <- resumen %>% filter(USO_SIGPAC %in% c("regadio", "secano", "otros"))

significativos_usos <- significativos %>% filter(!USO_SIGPAC %in% c("regadio", "secano", "otros"))
significativos_riego <- significativos %>% filter(USO_SIGPAC == "regadio")  # only irrigated

# Total area by grid cell and year
ocupacion_usos <- resumen_usos %>%
  group_by(CUADRICULA, ano) %>%
  summarise(total = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

ocupacion_riego <- resumen_riego %>%
  group_by(CUADRICULA, ano) %>%
  summarise(total_riego = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Mean percentage by land use
media_usos <- resumen_usos %>%
  left_join(ocupacion_usos, by = c("CUADRICULA", "ano")) %>%
  mutate(pct = 100 * area_total_m2 / total) %>%
  group_by(CUADRICULA, USO_SIGPAC) %>%
  summarise(media_pct = mean(pct, na.rm = TRUE), .groups = "drop")

# Mean percentage for irrigated only
media_regadio <- resumen_riego %>%
  filter(USO_SIGPAC == "regadio") %>%
  left_join(ocupacion_riego, by = c("CUADRICULA", "ano")) %>%
  mutate(pct = 100 * area_total_m2 / total_riego) %>%
  group_by(CUADRICULA, USO_SIGPAC) %>%
  summarise(media_pct = mean(pct, na.rm = TRUE), .groups = "drop")

# Filter significant + representative land uses
significativos_usos_final <- significativos_usos %>%
  left_join(media_usos, by = c("CUADRICULA", "USO_SIGPAC")) %>%
  filter(significancia == "si", media_pct >= 1)

significativos_riego_final <- significativos_riego %>%
  left_join(media_regadio, by = c("CUADRICULA", "USO_SIGPAC")) %>%
  filter(significancia == "si", media_pct >= 1)

# Combine and export
significativos_final <- bind_rows(significativos_usos_final, significativos_riego_final)

write_xlsx(significativos_final, "SIGPAC/outputs/all/significativos_total_todas_provincias_MK.xlsx")