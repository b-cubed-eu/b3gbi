# Calculate species richness from occurrence cubes

# Load required packages ----
library(tidyverse)
library(vegan)
library(minpack.lm)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tmap)
library(scico)
library(gnlm)
library(furrr)
library(tictoc)
library(mgcv)
library(devtools)
library(iNEXT)
library(ggplot2)

# Set parameters ----
region_level <- "continent" # country, continent, world
region <- "europe" # lower-case name of country or continent (or world)
grid_size <- 10000 # in meters
bias_corr_type <- "rarefaction" # type of bias correction (rarefaction, Hill)
first_year <- 1950 # start year of data
final_year <- 2022 # final year of data

# Load and prepare data ----

load_all()

cube_name <- "inst/extdata/eu_modellingtaxa_cube.csv"
tax_info <- "inst/extdata/eu_modellingtaxa_info.csv"
merged_data <- process_cube(cube_name, tax_info, first_year, final_year)

# Calculate species richness ----

richness_raw <- calc_ts(merged_data, method="raw")
richness_tr <- calc_ts(merged_data, method="total_records")
richness_rf <- calc_ts(merged_data, method="rarefaction")
evenness_ts <- calc_ts(merged_data, method="evenness")

