# ------------------------------------------------------------------------------
# MERGING SIGPAC LAND USE DATA FROM ALL PROVINCES
# ------------------------------------------------------------------------------

# Purpose:
# This script consolidates and standardizes land use data from the SIGPAC system
# across multiple Spanish provinces. It integrates and transforms summary, filtered,
# and significant trend analysis results for national-level analysis.

# Workflow:
# 1. Define a list of provinces to process.
# 2. For each province, load three types of files:
#    - The annual summary per grid cell (`*_resumen.xlsx`).
#    - The significant Mann-Kendall trend results (land use and irrigation).
#    - The filtered results with MK and relevance criteria (`*_filter.xlsx`).
# 3. In both the summary and GLM data, the irrigation type (`tipo_riego`) is duplicated
#    and treated as an additional land use category for parallel analysis.
# 4. A `provincia` column is added to all data frames for later merging.
# 5. A standardization dictionary is applied to `USO_SIGPAC` to unify different naming
#    conventions under common codes (e.g. "TIERRA ARABLE" → "TA", "PASTO CON ARBOLADO" → "PA").
# 6. All data are merged by type and exported into combined Excel files.

# Output:
# The script generates three national-level Excel files:
# - `resumen_total_todas_provincias.xlsx` → Land use area data by grid cell, year, and province.
# - `significativos_total_todas_provincias.xlsx` → Significant MK results (land use + irrigation).
# - `lm_total_filter.xlsx` → Filtered MK results (by criteria and significance) for all provinces.

# Required libraries:
library(readxl)      # To read Excel files
library(dplyr)       # For data manipulation
library(stringr)     # For file name and string handling
library(writexl)     # For writing Excel outputs

# ------------------------------------------------------------------------------
# PART 1: DEFINE INPUTS
# ------------------------------------------------------------------------------

# List of provinces to process
provinces <- c("cuenca", "albacete", "ciudad_real", "toledo", "guadalajara", "navarra")

# Base directory
base_path <- "SIGPAC/outputs/all"

# Initialize lists to accumulate results
resumen_total <- list()
significativos_total <- list()
glm_total <- list()

# ------------------------------------------------------------------------------
# PART 2: LOOP THROUGH PROVINCES
# ------------------------------------------------------------------------------

for (prov in provinces) {
  
  resumen_path <- file.path(base_path, prov, paste0(prov, "_resumen.xlsx"))
  signif_landuse_path <- file.path(base_path, prov, paste0("mkac_usos_filter_significativos_", prov, ".xlsx"))
  signif_irrig_path <- file.path(base_path, prov, paste0("mkac_", prov, "_riego_significativos.xlsx"))
  glm_path <- file.path(base_path, prov, paste0("mkac_usos_filter_", prov, ".xlsx"))
  glm_irrig_path <- file.path(base_path, prov, paste0("mkac_", prov, "_riego.xlsx"))
  
  # Load summary and duplicate `tipo_riego` as a land use category
  if (file.exists(resumen_path)) {
    df_res <- read_xlsx(resumen_path) %>%
      mutate(tipo_riego = as.character(tipo_riego))
    
    df_riego_extra <- df_res %>%
      filter(!is.na(tipo_riego)) %>%
      mutate(USO_SIGPAC = tipo_riego)
    
    df_res <- df_res %>%
      mutate(province = prov) %>%
      select(-tipo_riego)
    
    df_riego_extra <- df_riego_extra %>%
      mutate(province = prov) %>%
      select(-tipo_riego)
    
    df_res <- bind_rows(df_res, df_riego_extra)
    resumen_total[[prov]] <- df_res
  }
  
  # Load significant MK results (land use + irrigation)
  df_sig <- tibble()
  
  if (file.exists(signif_landuse_path)) {
    df_sig <- read_xlsx(signif_landuse_path)
  }
  
  if (file.exists(signif_irrig_path)) {
    df_riego <- read_xlsx(signif_irrig_path) %>%
      mutate(USO_SIGPAC = as.character(USO_SIGPAC))
    df_sig <- bind_rows(df_sig, df_riego)
  }
  
  if (nrow(df_sig) > 0) {
    df_sig <- df_sig %>%
      mutate(province = prov) %>%
      select(-any_of("tipo_riego"))
    significativos_total[[prov]] <- df_sig
  }
  
  # Load filtered MK results (GLM-compatible)
  df_glm <- tibble()
  
  if (file.exists(glm_path)) {
    df_glm <- read_xlsx(glm_path) %>%
      mutate(province = prov)
  }
  
  if (file.exists(glm_irrig_path)) {
    df_riego_glm <- read_xlsx(glm_irrig_path) %>%
      mutate(USO_SIGPAC = as.character(USO_SIGPAC),
             province = prov)
    df_glm <- bind_rows(df_glm, df_riego_glm)
  }
  
  if (nrow(df_glm) > 0) {
    glm_total[[prov]] <- df_glm
  }
}

# ------------------------------------------------------------------------------
# PART 3: MERGE AND EXPORT RESULTS
# ------------------------------------------------------------------------------

# Combine data across all provinces
resumen_df <- bind_rows(resumen_total)
significativos_df <- bind_rows(significativos_total)
glm_df <- bind_rows(glm_total)

# Export full merged datasets
write_xlsx(resumen_df, file.path(base_path, "resumen_total_todas_provincias.xlsx"))
write_xlsx(significativos_df, file.path(base_path, "significativos_total_todas_provincias.xlsx"))
write_xlsx(glm_df, file.path(base_path, "lm_total_filter.xlsx"))

# ------------------------------------------------------------------------------
# PART 4: FILTER GLM RESULTS TO INCLUDE ONLY SIGNIFICANT USES
# ------------------------------------------------------------------------------

glm_significativos_df <- glm_df %>%
  semi_join(significativos_df %>% select(CUADRICULA, USO_SIGPAC),
            by = c("CUADRICULA", "USO_SIGPAC"))

# Export only significant filtered results
write_xlsx(glm_significativos_df, file.path(base_path, "lm_total_filter_significativos.xlsx"))