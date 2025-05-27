# Tetrax and Land-use Analysis Scripts

This repository contains R scripts developed by Marina Pachón Mena to process, analyze, and visualize trends in Little Bustard (*Tetrax tetrax*) population data and land use (SIGPAC) information in Spain.

## Scripts included

### `process_sigpac_land_use.R`
Processes yearly SIGPAC land use data by province. Clips agricultural parcels to 10x10 km grid cells and exports them for further spatial analysis.

### `prepare_tetrax_census_data.R`
Prepares and georeferences Little Bustard census data using a UTM grid. Assigns provinces, autonomous communities, and sampling effort metrics.

### `population_trend_analysis_tetrax.R`
Performs Mann-Kendall tests and Sen's slope estimation to identify significant population trends per grid cell and sampling ID.

### `plot_little_bustard_trends.R`
Generates individual trend plots using linear models for cells with significant trends. Exports `.png` figures and a table of model coefficients.

## Required R packages

```r
library(sf)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)
library(ggplot2)
library(mkac)
library(trend)
library(ggtext)
library(tidyverse)
library(purrr)
