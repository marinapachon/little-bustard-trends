# --------------------------------------------
# Little Bustard Population Trend Plotting
# Author: Marina Pachón Mena
#
# Description:
# This script visualizes population trends of the Little Bustard (Tetrax tetrax)
# in UTM grid cells where significant trends were detected using the Mann-Kendall test.
# It fits linear models to annual maximum counts and generates one graph per grid-ID.
# Outputs include labeled .png plots and a summary of model coefficients in Excel format.
# --------------------------------------------

######### LOAD PACKAGES #########

library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(writexl)
library(ggtext)

######### LOAD DATA #########

# Load census data with maximum yearly counts
census_max <- st_read("inputs/censo_gis/censo_tetrax_all_max_visita.gpkg")
results_mkac_significant <- st_read("inputs/censo_gis/resultados_mkac_significativos_all_max_visita.gpkg")

######### CLEAN NAMES FOR LABELS #########

# Rename Autonomous Communities
ccaa_labels <- c(
  andalucia = "Andalucía", aragon = "Aragón", cantabria = "Cantabria",
  castilla_la_mancha = "Castilla-La Mancha", castilla_y_leon = "Castilla y León",
  catalunya = "Cataluña", comunitat_valenciana = "Comunidad Valenciana",
  extremadura = "Extremadura", galicia = "Galicia", la_rioja = "La Rioja",
  madrid = "Madrid", murcia = "Murcia", navarra = "Navarra"
)

# Rename Provinces
province_labels <- c(
  albacete = "Albacete", alicante = "Alicante", avila = "Ávila", badajoz = "Badajoz",
  barcelona = "Barcelona", burgos = "Burgos", caceres = "Cáceres", cadiz = "Cádiz",
  cantabria = "Cantabria", ciudad_real = "Ciudad Real", cordoba = "Córdoba", cuenca = "Cuenca",
  girona = "Girona", granada = "Granada", guadalajara = "Guadalajara", huesca = "Huesca",
  jaen = "Jaén", la_rioja = "La Rioja", leon = "León", lleida = "Lleida", lugo = "Lugo",
  madrid = "Madrid", murcia = "Murcia", navarra = "Navarra", palencia = "Palencia",
  salamanca = "Salamanca", segovia = "Segovia", sevilla = "Sevilla", soria = "Soria",
  teruel = "Teruel", toledo = "Toledo", valladolid = "Valladolid", zamora = "Zamora", zaragoza = "Zaragoza"
)

# Apply name cleaning
census_max <- census_max %>%
  mutate(
    ccaa = recode(tolower(ccaa), !!!ccaa_labels),
    provincia = recode(tolower(provincia), !!!province_labels)
  )

results_mkac_significant <- results_mkac_significant %>%
  mutate(
    ccaa = recode(tolower(ccaa), !!!ccaa_labels),
    provincia = recode(tolower(provincia), !!!province_labels)
  )

######### JOIN CENSUS + SIGNIFICANT TREND DATA #########

results_clean <- results_mkac_significant %>%
  st_drop_geometry() %>%
  select(cuadricula, id, tau, p_value, pendiente_sen, conteo_max, conteo_min)

census_filtered <- census_max %>%
  semi_join(results_clean, by = c("cuadricula", "id")) %>%
  left_join(results_clean, by = c("cuadricula", "id"))

######### EXAMPLE GRAPH #########

cuadricula_example <- "30TVK85"
id_example <- "120"

df_example <- census_filtered %>%
  filter(cuadricula == cuadricula_example, id == id_example)

plot_trend <- function(df) {
  model <- lm(individuos ~ ano, data = df)
  summary_model <- summary(model)
  r2 <- summary_model$r.squared
  
  cuadricula <- unique(df$cuadricula)
  id <- unique(df$id)
  province <- unique(df$provincia)
  region <- unique(df$ccaa)
  tau <- unique(df$tau)
  p_mk <- unique(df$p_value)
  sen_slope <- unique(df$pendiente_sen)
  
  title <- if (province == region) {
    paste("Little Bustard trend -", cuadricula, "-", id, "-", province)
  } else {
    paste("Little Bustard trend -", cuadricula, "-", id, "-", province, "-", region)
  }
  
  n_years <- census_max %>%
    filter(cuadricula == !!cuadricula, id == !!id) %>%
    distinct(ano) %>%
    nrow()
  
  p_text <- case_when(
    p_mk < 0.001 ~ "p < 0.001",
    p_mk < 0.01 ~ "p < 0.01",
    p_mk < 0.05 ~ "p < 0.05",
    TRUE ~ "<span style='color:red; font-weight:bold;'>p > 0.05</span>"
  )
  
  subtitle <- paste0("Mann-Kendall Test: Sen's slope = ", sprintf("%.3f", sen_slope),
                     " | ", p_text, " | n = ", n_years)
  
  ggplot(df, aes(x = ano, y = individuos)) +
    geom_point(color = "#009E73", size = 3, shape = 21, fill = "#009E73") +
    geom_smooth(method = "lm", se = FALSE, color = "#4D4D4D", linewidth = 0.8) +
    geom_line(aes(group = 3), color = "#009E73", linewidth = 1.2) +
    scale_x_continuous(limits = c(1998, 2023), breaks = seq(1998, 2023, 5)) +
    scale_y_continuous(limits = c(-5, 64)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Individuals"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "none"
    )
}

# Run example
example_plot <- plot_trend(df_example)
print(example_plot)

######### LOOP TO EXPORT PLOTS #########

model_results <- list()

census_filtered %>%
  group_by(cuadricula, id) %>%
  group_split() %>%
  lapply(function(df) {
    g <- plot_trend(df)
    
    cell <- unique(df$cuadricula)
    id <- unique(df$id)
    path_out <- paste0("outputs/cuadricula_id/all/max/lm/colaboradores/", cell, "_", id, ".png")
    ggsave(filename = path_out, plot = g, width = 8, height = 5, dpi = 300)
    
    model <- lm(individuos ~ ano, data = df)
    coefs <- coef(model)
    p_model <- summary(model)$coefficients[2, 4]
    tau <- unique(df$tau)
    sen <- unique(df$pendiente_sen)
    
    model_results <<- append(model_results, list(data.frame(
      cuadricula = cell,
      id = id,
      pendiente_modelo = coefs[2],
      p_value_modelo = p_model,
      tau = tau,
      p_value_mk = p_model,
      pendiente_sen = sen
    )))
  })

# Combine results and save to Excel
model_results_df <- bind_rows(model_results)
write_xlsx(model_results_df, "outputs/cuadricula_id/all/max/lm/colaboradores/resultados_modelo.xlsx")
