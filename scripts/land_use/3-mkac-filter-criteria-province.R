# ------------------------------------------------------------------------------
# FILTERING LAND USES BASED ON RELEVANCE AND TEMPORAL VARIATION CRITERIA
# ------------------------------------------------------------------------------

# Purpose:
# This script filters the results from the Mann-Kendall trend analysis on land use data
# to retain only those uses that show:
#   - Significant spatial representativeness (≥1% of the grid cell surface area),
#   - And/or relevant temporal variation (>1% difference between years).
# This filtering helps to focus subsequent analyses on the most informative cases.

# Workflow:
# 1. Load input files:
#    - A summary table with surface area per land use, grid cell, and year.
#    - The Mann-Kendall test results per land use.
# 2. Calculate the percentage of surface area that each land use occupies
#    within its grid cell and year.
# 3. For each grid cell and land use combination, calculate:
#    - Maximum and minimum percentage values observed.
#    - The range of variation over time.
# 4. Filter land uses that:
#    - Reach ≥1% in any year, *and*
#    - Show a temporal variation range >1%.
# 5. Apply this filter to the Mann-Kendall results table.
# 6. Export:
#    - The filtered result table (`mkac_usos_filter_<province>.xlsx`)
#    - A subset with only significant results (p ≤ 0.05).

# Required libraries:
library(readxl)    # For reading Excel (.xlsx) files
library(dplyr)     # For data manipulation (mutate, group_by, summarise, etc.)
library(writexl)   # For writing Excel output

# ------------------------------------------------------------------------------
# PART 1: DATA INPUT
# ------------------------------------------------------------------------------

# Load summary table with surface area by land use, grid cell, and year
land_use_table <- read_excel("SIGPAC/outputs/all/toledo/toledo_resumen.xlsx")

# Load Mann-Kendall trend analysis results
mk_results <- read_excel("SIGPAC/outputs/all/toledo/mkac_toledo_usos.xlsx")

# ------------------------------------------------------------------------------
# PART 2: PERCENTAGE CALCULATION AND TEMPORAL VARIATION
# ------------------------------------------------------------------------------

# Compute total surface area per grid cell and year
total_area_by_cell <- land_use_table %>%
  group_by(CUADRICULA, ano) %>%
  summarise(total_cell_area = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

# Join total area back to original table and calculate area percentage per land use
percentage_table <- land_use_table %>%
  left_join(total_area_by_cell, by = c("CUADRICULA", "ano")) %>%
  mutate(pct_area = 100 * area_total_m2 / total_cell_area)

# For each grid cell + land use, calculate:
# - Maximum percentage observed
# - Minimum percentage observed
# - Percentage range (temporal variation)
variation_table <- percentage_table %>%
  group_by(CUADRICULA, USO_SIGPAC) %>%
  summarise(
    pct_max = max(pct_area, na.rm = TRUE),
    pct_min = min(pct_area, na.rm = TRUE),
    pct_range = pct_max - pct_min,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# PART 3: FILTERING BY CRITERIA
# ------------------------------------------------------------------------------

# Select land uses that meet both:
# - At least 1% surface area at some point
# - A temporal variation range greater than 1%
selected_uses <- variation_table %>%
  filter(pct_max >= 1 & pct_range > 1)

# Filter Mann-Kendall results keeping only the selected land uses
filtered_results <- mk_results %>%
  semi_join(selected_uses, by = c("CUADRICULA", "USO_SIGPAC"))

# ------------------------------------------------------------------------------
# PART 4: EXPORT RESULTS
# ------------------------------------------------------------------------------

# Export the full filtered results
write_xlsx(filtered_results, "SIGPAC/outputs/all/toledo/mkac_usos_filter_toledo.xlsx")

# Export only the statistically significant results (p ≤ 0.05)
significant_results <- filtered_results %>%
  filter(significancia == "si")

write_xlsx(significant_results, "SIGPAC/outputs/all/toledo/mkac_usos_filter_significativos_toledo.xlsx")
