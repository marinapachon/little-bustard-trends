# ------------------------------------------------------------------------------
# LAND USE + IRRIGATION TRENDS PER GRID CELL (10x10 km)
# ------------------------------------------------------------------------------

# Purpose:
# This script generates time-series plots showing land use (% of area)
# for each 10x10 km grid cell in a given province, focusing on those
# with previously filtered and relevant land use categories.
#
# The script includes:
#   - All land use types that passed spatial and temporal relevance criteria,
#     regardless of whether their trends are statistically significant.
#   - The irrigated area (“regadio”) as an additional aggregated category,
#     but only for grid cells where its trend is statistically significant (p < 0.05).
#
# Trends are visualized through:
#   - Linear regression lines,
#   - Sen's slope,
#   - Mann-Kendall p-values.
#
# Statistical significance is indicated visually (e.g., solid vs dashed lines).

# Workflow:
# 1. Load input files:
#    - `provincia_resumen.xlsx`: land use and irrigation area by grid, year, and category.
#    - `mkac_usos_filter_provincia.xlsx`: land uses filtered by relevance and trend analysis.
#    - `mkac_provincia_riego.xlsx`: irrigation trend results.
# 2. Calculate total area per grid and year and % area per use.
# 3. Keep all land use types present in the filtered MK results, regardless of p-value.
# 4. Add the special "regadio" category (irrigated surface) only if significant.
# 5. Combine land use and irrigation data into a unified format for plotting.
# 6. For each grid cell:
#    - Generate a plot showing land use % over time.
#    - Fit linear models and calculate MK/Sen statistics.
#    - Include enriched labels with slope and p-value.
#    - Save the plot as `.png`.
# 7. Export a summary Excel table (`data_provincia.xlsx`) with all visualized data
#    and statistical results by grid cell and land use.

# Outputs:
# - One `.png` plot per grid cell showing multiple land use trends + irrigation.
# - An Excel file `data_provincia.xlsx` with trend results and data used.

# Required libraries:
library(dplyr)
library(ggplot2)
library(readxl)
library(ggtext)
library(RColorBrewer)
library(scales)
library(mkac)       # Mann-Kendall and Theil-Sen slope
library(writexl)

# Fixed color palette (ColorBrewer Qualitative)
fixed_colors <- c(
  AG = "#a6cee3", CA = "#000000", CF = "#b2df8a", CI = "#80b1d3", CS = "#fb9a99",
  ED = "#e31a1c", EP = "#fdbf6f", FS = "#ff7f00", FY = "#cab2d6", FL = "#6a3d9a",
  FV = "#ffff99", OF = "#b15928", VO = "#1b9e77", OV = "#d95f02", FO = "#7570b3",
  PR = "#e7298a", PA = "#66a61e", PS = "#e6ab02", TA = "#a6761d", TH = "#666666",
  IM = "#8dd3c7", ZU = "#bcbd22", IS = "#bebada", IV = "#fb8072", VI = "#33a02c",
  VF = "#fdb462", regadio = "#1f78b4"
)

# Load input data
land_use_data <- read_excel("SIGPAC/outputs/all/ciudad_real/ciudad_real_resumen.xlsx")
significant_landuses <- read_excel("SIGPAC/outputs/all/ciudad_real/mkac_usos_filter_ciudad_real.xlsx")
significant_irrigation <- read_excel("SIGPAC/outputs/all/ciudad_real/mkac_ciudad_real_riego.xlsx")

# Identify grid cells where "regadio" is significant
sig_irrig_grids <- significant_irrigation %>%
  filter(USO_SIGPAC == "regadio", p_value < 0.05) %>%
  pull(CUADRICULA)

# Compute total area per grid and year
total_area_by_year <- land_use_data %>%
  group_by(CUADRICULA, ano) %>%
  summarise(area_total = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Filter land uses with significant MK results
filtered_land_use <- land_use_data %>%
  semi_join(significant_landuses, by = c("CUADRICULA", "USO_SIGPAC"))

# Calculate % area per land use
landuse_pct <- filtered_land_use %>%
  group_by(CUADRICULA, USO_SIGPAC, ano) %>%
  summarise(area_total_m2 = sum(area_total_m2, na.rm = TRUE), .groups = "drop") %>%
  left_join(total_area_by_year, by = c("CUADRICULA", "ano")) %>%
  mutate(
    pct_ocupacion = 100 * area_total_m2 / area_total,
    USO_riego = USO_SIGPAC
  ) %>%
  select(CUADRICULA, ano, pct_ocupacion, USO_riego)

# Process "regadio" as aggregated irrigation category
regadio_pct <- land_use_data %>%
  filter(tipo_riego == "regadio", CUADRICULA %in% sig_irrig_grids) %>%
  group_by(CUADRICULA, ano) %>%
  summarise(area_regadio = sum(area_total_m2, na.rm = TRUE), .groups = "drop") %>%
  left_join(total_area_by_year, by = c("CUADRICULA", "ano")) %>%
  mutate(
    pct_ocupacion = 100 * area_regadio / area_total,
    USO_riego = "regadio"
  ) %>%
  select(CUADRICULA, ano, pct_ocupacion, area_regadio, USO_riego)

# Merge land uses and irrigation
final_table <- bind_rows(landuse_pct, regadio_pct)

# Grouped structure for plotting
grouped_table <- final_table %>%
  group_by(CUADRICULA, ano, USO_riego) %>%
  summarise(pct_ocupacion = sum(pct_ocupacion, na.rm = TRUE), .groups = "drop")

# Generate plots per grid cell (detailed plotting logic not shown here)
# Save results and export to:
#   - PNGs in `plots/` folder
#   - Excel file: `data_ciudad_real.xlsx`