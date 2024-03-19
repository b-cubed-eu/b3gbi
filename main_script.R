# Calculate species richness from occurrence cubes

# Load required packages ----
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(furrr)
library(purrr)
library(future)
library(permute)
library(devtools)
library(iNEXT)
library(ggplot2)
library(taxize)
library(virtualspecies)
library(vegan)
library(stringr)
library(pkgdown)

# Create data frame for registering indicators
available_indicators <- data.frame(
  indicator_class = NA,
  indicator_name = NA,
  plot_title = NA,
  legend_label = NA,
  legend_transformation = NA
)

# Save it as .rda file in data directory of the package
usethis::use_data(available_indicators, overwrite = TRUE)

# Register indicators
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness", backup=FALSE, overwrite = TRUE)
register_indicator("total_occ", "Total Occurrences", "Total Occurrences", "Occurrences", backup=FALSE, overwrite = TRUE)
register_indicator("pielou_evenness", "Pielou's Evenness", "Pielou's Evenness", "Evenness", backup=FALSE, overwrite = TRUE)
register_indicator("williams_evenness", "Williams' Evenness", "Williams' Evenness", "Evenness", backup=FALSE, overwrite = FALSE)
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")

stop("This is an error message.
     \nSo is this.")

# Set parameters ----
region_level <- "country" # country, continent, world
region <- "Germany" # lower-case name of country or continent (or world)
grid_size <- 10000 # in meters
bias_corr_type <- "rarefaction" # type of bias correction (rarefaction, Hill)
first_year <- 1000 # start year of data
final_year <- 2023 # final year of data

# Load and prepare data ----

load_all()

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

cube_name <- "inst/extdata/europe_insect_cube.csv"
tax_info <- "inst/extdata/europe_insect_info.csv"
example_cube_1 <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/denmark_mammals_cube.csv"
tax_info <- "inst/extdata/denmark_mammals_info.csv"
example_cube_2 <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/eu_modellingtaxa_cube.csv"
tax_info <- "inst/extdata/eu_modellingtaxa_info.csv"
example_cube3 <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

cube_name <- "inst/extdata/europe_amphibians_cube.csv"
tax_info <- "inst/extdata/europe_amphibians_info.csv"
example_cube4 <- process_cube(cube_name, tax_info, first_year=first_year, final_year=final_year)

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
