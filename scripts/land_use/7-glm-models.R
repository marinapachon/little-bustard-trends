# ------------------------------------------------------------------------------
# GLM Analysis by Grid Cell and Land Use Type
# ------------------------------------------------------------------------------

# Description:
# This script performs Generalized Linear Model (GLM) analyses to evaluate the 
# relationship between land use (percentage of surface occupation per category) 
# and the abundance of the little bustard (*Tetrax tetrax*) across 10x10 km grid cells.

# Applied criteria:
# - Two filters: minimum 1% representativeness and 1% variation (without Mann-Kendall filter)
# - No minimum number of years required for bird census data
# - Land use and census years must match
# - Poisson and quasi-Poisson families are used depending on overdispersion
# - Significant autocorrelation (Durbin-Watson test) is calculated but not used as an exclusion criterion
# - Only significant positive correlations are plotted

# Poisson regression is applied because the response variable is a count.
# When overdispersion is detected, the quasi-Poisson family is used instead.

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(writexl)
library(performance)
library(lmtest)

# ------------------------------------------------------------------------------
# Province Name Cleaning
# ------------------------------------------------------------------------------
province_names <- c(
  albacete = "Albacete", alicante = "Alicante", avila = "Ávila", badajoz = "Badajoz",
  barcelona = "Barcelona", burgos = "Burgos", caceres = "Cáceres", cadiz = "Cádiz",
  cantabria = "Cantabria", ciudad_real = "Ciudad Real", cordoba = "Córdoba", cuenca = "Cuenca",
  girona = "Girona", granada = "Granada", guadalajara = "Guadalajara", huesca = "Huesca",
  jaen = "Jaén", la_rioja = "La Rioja", leon = "León", lleida = "Lleida", lugo = "Lugo",
  madrid = "Madrid", murcia = "Murcia", navarra = "Navarra", palencia = "Palencia",
  salamanca = "Salamanca", segovia = "Segovia", sevilla = "Sevilla", soria = "Soria",
  teruel = "Teruel", toledo = "Toledo", valladolid = "Valladolid", zamora = "Zamora",
  zaragoza = "Zaragoza"
)

# ------------------------------------------------------------------------------
# Land Use Dictionary
# ------------------------------------------------------------------------------
land_use_dict <- tibble::tribble(
  ~code, ~description,
  "AG", "Water bodies", "CA", "Roads", "CF", "Citrus-Fruit",
  "CI", "Citrus", "CS", "Citrus-Shell fruit", "CV", "Citrus-Vineyard",
  "ED", "Buildings", "EP", "Landscape elements", "FF", "Shell fruit-Fruit",
  "FL", "Shell fruit-Olive", "FO", "Forest", "FS", "Shell fruit",
  "FV", "Shell fruit-Vineyard", "FY", "Fruit", "IM", "Unproductive",
  "IV", "Greenhouses", "OC", "Olive-Citrus", "OF", "Olive-Fruit", "OV", "Olive",
  "PA", "Wooded pasture", "PR", "Shrubland", "PS", "Grassland", "TA", "Arable land",
  "TH", "Vegetable garden", "VF", "Fruit-Vineyard", "VI", "Vineyard",
  "VO", "Olive-Vineyard", "ZC", "Consolidated area", "ZU", "Urban area",
  "ZV", "Censored area", "OTROS", "Others", "SECANO", "Rainfed", "REGADIO", "Irrigated"
)

# ------------------------------------------------------------------------------
# Data Input: Land Use and Little Bustard Census
# ------------------------------------------------------------------------------

# Load land use summary with irrigation types included
land_use_summary <- read_excel("SIGPAC/outputs/all/resumen_total_todas_provincias.xlsx")

# Exclude general categories (e.g. 'rainfed', 'irrigated', 'others') and calculate % occupation
excluded_uses <- c("regadio", "secano", "otros")
grid_area_per_year <- land_use_summary %>%
  filter(!USO_SIGPAC %in% excluded_uses) %>%
  group_by(CUADRICULA, ano) %>%
  summarise(area_total_cuadricula = sum(area_total_m2, na.rm = TRUE), .groups = "drop")

land_use_percent <- land_use_summary %>%
  left_join(grid_area_per_year, by = c("CUADRICULA", "ano")) %>%
  mutate(pct_ocupacion = 100 * area_total_m2 / area_total_cuadricula)

# Read list of significant land uses by grid cell
significant_uses <- read_excel("SIGPAC/outputs/all/resumen_total_todas_provincias_criterios.xlsx") %>%
  select(CUADRICULA, USO_SIGPAC) %>%
  distinct()

# Filter for significant land uses and reshape to wide format
model_table <- land_use_percent %>%
  semi_join(significant_uses, by = c("CUADRICULA", "USO_SIGPAC")) %>%
  group_by(CUADRICULA, ano, USO_SIGPAC) %>%
  summarise(pct_ocupacion = sum(pct_ocupacion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = USO_SIGPAC, values_from = pct_ocupacion, values_fill = 0)

names(model_table) <- make.names(names(model_table))  # Clean column names

# Load census data and grid-province relationship
census_data <- read_excel("SIGPAC/inputs/sison/censo_tetrax_all_max_visita.xlsx")
trend_data <- read_excel("SIGPAC/inputs/sison/resultados_mkac_significativos_all_max_visita.xlsx")

# Extract list of significant grid cells
significant_grids <- unique(trend_data$cuadricula)

grid_province_match <- census_data %>%
  select(cuadricula, provincia) %>%
  distinct() %>%
  rename(CUADRICULA = cuadricula, PROVINCIA = provincia)

abundance_data <- census_data %>%
  filter(cuadricula %in% significant_grids) %>%
  group_by(cuadricula, ano) %>%
  summarise(Abundancia = sum(individuos, na.rm = TRUE), .groups = "drop") %>%
  rename(CUADRICULA = cuadricula)

# Keep only matching years for land use and census
matching_years <- abundance_data %>%
  select(CUADRICULA, ano) %>%
  distinct()

model_table_filtered <- model_table %>%
  semi_join(matching_years, by = c("CUADRICULA", "ano"))

# Final table for GLM (merged with abundance and province info)
glm_table <- model_table_filtered %>%
  filter(CUADRICULA %in% significant_grids) %>%
  left_join(abundance_data, by = c("CUADRICULA", "ano")) %>%
  left_join(grid_province_match, by = "CUADRICULA") %>%
  filter(!is.na(Abundancia))

# ------------------------------------------------------------------------------
# GLM Statistical Function
# ------------------------------------------------------------------------------

extract_glm_stats <- function(formula, data) {
  # Fit Poisson model
  model_pois <- glm(formula, data = data, family = poisson())
  
  # Test for overdispersion
  test_overdisp <- tryCatch(performance::check_overdispersion(model_pois), error = function(e) NULL)
  is_overdispersed <- if (!is.null(test_overdisp) && test_overdisp$p_value < 0.05) "yes" else "no"
  
  # Switch to quasi-Poisson if overdispersed
  final_model <- if (is_overdispersed == "yes") {
    glm(formula, data = data, family = quasipoisson())
  } else {
    model_pois
  }
  
  # Auxiliary linear model for Durbin-Watson test
  lm_aux <- tryCatch(lm(formula, data = data), error = function(e) NULL)
  dw_test <- tryCatch(dwtest(lm_aux), error = function(e) NA)
  p_dw <- if (is.list(dw_test)) dw_test$p.value else NA
  autocorrelation <- if (!is.na(p_dw) && p_dw < 0.05) "yes" else "no"
  
  r2_value <- tryCatch(performance::r2(final_model)$R2, error = function(e) NA)
  coef_table <- summary(final_model)$coefficients
  
  if (nrow(coef_table) < 2) {
    return(data.frame(uso = all.vars(formula)[2], R2 = r2_value, p_value = NA, pendiente = NA, sobredispersion = is_overdispersed))
  }
  
  data.frame(
    uso = all.vars(formula)[2],
    R2 = r2_value,
    p_value = coef_table[2, 4],
    pendiente = coef_table[2, 1],
    sobredispersion = is_overdispersed,
    p_dw = p_dw,
    autocorrelacion = autocorrelation
  )
}

# ------------------------------------------------------------------------------
# GLM Analysis by Grid Cell and Land Use Type
# ------------------------------------------------------------------------------

dir.create("graficos_usos_por_cuadricula", showWarnings = FALSE)
glm_results <- list()

for (grid in unique(glm_table$CUADRICULA)) {
  df_grid <- glm_table %>% filter(CUADRICULA == grid)
  
  land_uses <- significant_uses %>%
    filter(CUADRICULA == grid) %>%
    pull(USO_SIGPAC) %>%
    unique()
  
  province <- unique(df_grid$PROVINCIA)
  province <- if (is.na(province) || length(province) != 1) {
    "unknown"
  } else {
    province_names[[province]]
  }
  
  land_uses <- intersect(land_uses, names(df_grid))
  land_uses <- setdiff(land_uses, c("secano", "otros"))
  plots <- list()
  
  cat("\n📦 Processing grid:", grid, "\n")
  
  for (use in land_uses) {
    use_label <- land_use_dict %>%
      filter(code == use) %>%
      pull(description)
    
    if (length(use_label) == 0) {
      use_label <- ifelse(use == "regadio", "Irrigated", tools::toTitleCase(use))
    }
    
    use_data <- df_grid[[use]]
    # Filtro robusto y mínimo para evitar modelos triviales
    if (
      is.null(uso_data) || 
      all(is.na(uso_data)) || 
      sum(uso_data, na.rm = TRUE) == 0 ||
      is.na(sd(uso_data, na.rm = TRUE)) || 
      sd(uso_data, na.rm = TRUE) == 0 || 
      length(unique(uso_data)) <= 1
    ) {
      next
    }
    
    f <- as.formula(paste("Abundancia ~", use))
    stats <- extract_glm_stats(f, df_grid)
    stats$CUADRICULA <- grid
    glm_results[[length(glm_results) + 1]] <- stats
    
    if (is.na(stats$p_value) || stats$p_value > 0.05) next  # Plot only significant results
    
    # Styled p-value for subtitle
    p_formatted <- if (is.na(stats$p_value)) {
      "<span style='color:black'>p = NA</span>"
    } else if (stats$p_value < 0.001) {
      "<span style='color:black'>p < 0.001</span>"
    } else if (stats$p_value < 0.01) {
      "<span style='color:black'>p < 0.01</span>"
    } else if (stats$p_value < 0.05) {
      "<span style='color:black'>p < 0.05</span>"
    } else {
      "<span style='color:red'>p > 0.05</span>"
    }
    
    subtitle_text <- paste0(
      "D² = ", formatC(floor(stats$R2 * 100) / 100, format = "f", digits = 2, decimal.mark = ","),
      " | ", p_formatted
    )
    
    g <- ggplot(df_grid, aes(x = !!sym(use), y = Abundancia)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "#5F9EA0") +
      labs(
        title = paste0(grid, " - ", province),
        subtitle = subtitle_text,
        x = paste("%", use_label),
        y = "Abundance"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 16),
        axis.title.x = element_text(margin = margin(t = 10), size = 15),
        axis.title.y = element_text(margin = margin(t = 10), size = 15),
        axis.text.x = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_text(size = 14)
      )
    
    plots[[length(plots) + 1]] <- g
  }
  
  # Save all plots for this grid to a PDF
  if (length(plots) > 0) {
    pdf_name <- paste0(
      "graficos_usos_por_cuadricula/7-glm-percent_tetrax-poisson-criterios/sin_percentil_sison/",
      grid, "_", province, ".pdf"
    )
    pdf(file = pdf_name, width = 7, height = 5)
    for (plot in plots) print(plot)
    dev.off()
  } else {
    cat("🚫 No plots generated for:", grid, "\n")
  }
}

# ------------------------------------------------------------------------------
# Export GLM Results
# ------------------------------------------------------------------------------

glm_results_df <- bind_rows(glm_results) %>%
  left_join(grid_province_match, by = "CUADRICULA") %>%
  mutate(significant = ifelse(!is.na(p_value) & p_value < 0.05, "yes", "no"))

glm_results_significant <- glm_results_df %>%
  filter(significant == "yes")

# Save all results
write_xlsx(glm_results_df,
           "graficos_usos_por_cuadricula/7-glm-percent_tetrax-poisson-criterios/sin_percentil_sison/glm_results_by_grid_complete.xlsx")

# Save only significant results
write_xlsx(glm_results_significant,
           "graficos_usos_por_cuadricula/7-glm-percent_tetrax-poisson-criterios/sin_percentil_sison/glm_results_by_grid_significant.xlsx")