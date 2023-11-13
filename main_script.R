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
library(ggtext)
library(patchwork)
library(scales)
library(gganimate)
library(transformr)
library(gifski)
library(ggallin)

# Set parameters ----
region_level <- "country" # country, continent, world
region <- "Germany" # lower-case name of country or continent (or world)
grid_size <- 10000 # in meters
bias_corr_type <- "rarefaction" # type of bias correction (rarefaction, Hill)
first_year <- 1000 # start year of data
final_year <- 2023 # final year of data

# Load and prepare data ----

load_all()

cube_name <- "inst/extdata/eu_modellingtaxa_cube.csv"
tax_info <- "inst/extdata/eu_modellingtaxa_info.csv"
merged_data <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/global_amphibians_cube.csv"
tax_info <- "inst/extdata/global_amphibians_info.csv"
amphib_data <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/global_amphibians_cube2.csv"
tax_info <- "inst/extdata/global_amphibians_info2.csv"
datasets_info <- "inst/extdata/global_amphibians_datasets2.csv"
amphib_data2 <- process_cube(cube_name, tax_info, datasets_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/europe_amphibians_cube.csv"
tax_info <- "inst/extdata/europe_amphibians_info.csv"
amphib_data3 <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

amphib_data2_specimen <- amphib_data2 %>%
  dplyr::filter(dataType == "museum_specimens")

amphib_data2_citizen <- amphib_data2 %>%
  dplyr::filter(dataType == "citizen_science")

amphib_data2_other <- amphib_data2 %>%
  dplyr::filter(dataType == "mixed" | dataType == "survey")


# Calculate diversity metrics ----

richness_raw <- calc_ts(amphib_data, method="observed")
richness_tr <- calc_ts(amphib_data, method="total_records")
richness_rf <- calc_ts(merged_data, method="rarefaction")
evenness_ts <- calc_ts(amphib_data, method="evenness")

# Mapping diversity metrics ----

map_total_obs_amphib2_specimen <- calc_map(amphib_data2_specimen, level = "world", region = "Europe", type = "total_obs")
map_obs_richness_amphib2_specimen <- calc_map(amphib_data2_specimen, level = "continent", region = "Europe", type = "obs_rich")
map_newness_amphib2_specimen <- calc_map(amphib_data2_specimen, level = "continent", region = "Europe", type = "newness")
map_density_amphib2_specimen <- calc_map(amphib_data2_specimen, level = "continent", region = "Europe", type = "density")
map_evenness_amphib2_specimen <- calc_map(amphib_data2_specimen, level = "continent", region = "Europe", type = "even")
map_ab_rarity_amphib2_specimen <- calc_rarity(amphib_data2_specimen, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_amphib2_specimen <- calc_rarity(amphib_data2_specimen, level = "continent", region = "Europe", method = "grid", type = "area")

map_total_obs_amphib2_citizen <- calc_map(amphib_data2_citizen, level = "world", region = "Europe", type = "total_obs")
map_obs_richness_amphib2_citizen <- calc_map(amphib_data2_citizen, level = "continent", region = "Europe", type = "obs_rich")
map_newness_amphib2_citizen <- calc_map(amphib_data2_citizen, level = "continent", region = "Europe", type = "newness")
map_density_amphib2_citizen <- calc_map(amphib_data2_citizen, level = "continent", region = "Europe", type = "density")
map_evenness_amphib2_citizen <- calc_map(amphib_data2_citizen, level = "continent", region = "Europe", type = "even")
map_ab_rarity_amphib2_citizen <- calc_rarity(amphib_data2_citizen, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_amphib2_citizen <- calc_rarity(amphib_data2_citizen, level = "continent", region = "Europe", method = "grid", type = "area")

map_total_obs_amphib2_other <- calc_map(amphib_data2_other, level = "continent", region = "Europe", type = "total_obs")
map_obs_richness_amphib2_other <- calc_map(amphib_data2_other, level = "continent", region = "Europe", type = "obs_rich")
map_newness_amphib2_other <- calc_map(amphib_data2_other, level = "continent", region = "Europe", type = "newness")
map_density_amphib2_other <- calc_map(amphib_data2_other, level = "continent", region = "Europe", type = "density")
map_evenness_amphib2_other <- calc_map(amphib_data2_other, level = "continent", region = "Europe", type = "even")
map_ab_rarity_amphib2_other <- calc_rarity(amphib_data2_other, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_amphib2_other <- calc_rarity(amphib_data2_other, level = "continent", region = "Europe", method = "grid", type = "area")

map_total_obs_amphib2 <- calc_map(amphib_data2, level = "continent", region = "Europe", type = "total_obs")
map_obs_richness_amphib2 <- calc_map(amphib_data2, level = "continent", region = "Europe", type = "obs_rich")
map_newness_amphib2 <- calc_map(amphib_data2, level = "continent", region = "Europe", type = "newness")
map_density_amphib2 <- calc_map(amphib_data2, level = "continent", region = "Europe", type = "density")
map_evenness_amphib2 <- calc_map(amphib_data2, level = "continent", region = "Europe", type = "even")
map_ab_rarity_amphib2 <- calc_rarity(amphib_data2, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_amphib2 <- calc_rarity(amphib_data2, level = "continent", region = "Europe", method = "grid", type = "area")

map_total_obs_amphib <- calc_map(amphib_data, level = "continent", region = "Europe", type = "total_obs")
map_obs_richness_amphib <- calc_map(amphib_data, level = "continent", region = "Europe", type = "obs_rich")
map_newness_amphib <- calc_map(amphib_data, level = "continent", region = "Europe", type = "newness")
map_density_amphib <- calc_map(amphib_data, level = "continent", region = "Europe", type = "density")
map_evenness_amphib <- calc_map(amphib_data, level = "continent", region = "Europe", type = "even")
map_ab_rarity_amphib <- calc_rarity(amphib_data, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_amphib <- calc_rarity(amphib_data, level = "continent", region = "Europe", method = "grid", type = "area")

map_total_obs_modtaxa <- calc_map(merged_data, level = "continent", region = "Europe", type = "total_obs")
map_obs_richness_modtaxa <- calc_map(merged_data, level = "continent", region = "Europe", type = "obs_rich")
map_newness_modtaxa <- calc_map(merged_data, level = "continent", region = "Europe", type = "newness")
map_density_modtaxa <- calc_map(merged_data, level = "continent", region = "Europe", type = "density")
map_evenness_modtaxa <- calc_map(merged_data, level = "continent", region = "Europe", type = "even")
map_ab_rarity_merged_data <- calc_rarity(merged_data, level = "continent", region = "Europe", method = "grid", type = "abundance")
map_area_rarity_merged_data <- calc_rarity(merged_data, level = "continent", region = "Europe", method = "grid", type = "area")


# Plot animation using a function
plot_anim_map(amphib_data,
              filename="amphib_europe_test_anim.gif",
              start_year = 1970,
              end_year = 2022,
              num_years = 10,
              div_type = "total_obs",
              cust_limits = c(1, 50000),
              xlims=c(2000000, 6200000),
              ylims=c(1500000, 5500000),
              title = "Amphibians Occurrences in Europe")
