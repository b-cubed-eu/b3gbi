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


# Calculate gridded maps of amphibian occurrences in Europe by decade

mapobs_amph3_1930s <- calc_map(amphib_data3[(amphib_data3$year > 1930 &
                                             amphib_data3$year <= 1940),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1940s <- calc_map(amphib_data3[(amphib_data3$year > 1940 &
                                             amphib_data3$year <= 1950),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1950s <- calc_map(amphib_data3[(amphib_data3$year > 1950 &
                                             amphib_data3$year <= 1960),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1960s <- calc_map(amphib_data3[(amphib_data3$year > 1960 &
                                             amphib_data3$year <= 1970),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1970s <- calc_map(amphib_data3[(amphib_data3$year > 1970 &
                                             amphib_data3$year <= 1980),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1980s <- calc_map(amphib_data3[(amphib_data3$year > 1980 &
                                             amphib_data3$year <= 1990),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_1990s <- calc_map(amphib_data3[(amphib_data3$year > 1990 &
                                             amphib_data3$year <= 2000),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_2000s <- calc_map(amphib_data3[(amphib_data3$year > 2000 &
                                             amphib_data3$year <= 2010),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_2010s <- calc_map(amphib_data3[(amphib_data3$year > 2010 &
                                             amphib_data3$year <= 2020),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

mapobs_amph3_2020s <- calc_map(amphib_data3[(amphib_data3$year > 2020 &
                                             amphib_data3$year <= 2023),],
                             level = "continent",
                             region = "Europe",
                             type = "total_obs")

# Plot maps

p1930s <- plot_map(mapobs_amph3_1930s,
                 xlims=c(2000000, 6200000),
                 ylims=c(1500000, 5500000),
                 title = "GBIF Amphibian Occurrences in Europe: 1930's",
                 cust_limits = c(1,50000))

p1940s <- plot_map(mapobs_amph3_1940s,
                 xlims=c(2000000, 6200000),
                 ylims=c(1500000, 5500000),
                 title = "GBIF Amphibian Occurrences in Europe: 1940's",
                 cust_limits = c(1,50000))

p1950s <- plot_map(mapobs_amph3_1950s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 1950's",
               cust_limits = c(1,50000))

p1960s <- plot_map(mapobs_amph3_1960s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 1960's",
               cust_limits = c(1,50000))

p1970s <- plot_map(mapobs_amph3_1970s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 1970's",
               cust_limits = c(1,50000))

p1980s <- plot_map(mapobs_amph3_1980s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 1980's",
               cust_limits = c(1,50000))

p1990s <- plot_map(mapobs_amph3_1990s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 1990's",
               cust_limits = c(1,50000))

p2000s <- plot_map(mapobs_amph3_2000s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 2000's",
               cust_limits = c(1,50000))

p2010s <- plot_map(mapobs_amph3_2010s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 2010's",
               cust_limits = c(1,50000))

p2020s <- plot_map(mapobs_amph3_2020s,
               xlims=c(2000000, 6200000),
               ylims=c(1500000, 5500000),
               title = "GBIF Amphibian Occurrences in Europe: 2020's",
               cust_limits = c(1,50000))

# Save maps as png files

ggsave("mapobs_amph3_1930s.png",
       p1930s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1940s.png",
       p1940s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1950s.png",
       p1950s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1960s.png",
       p1960s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1970s.png",
       p1970s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1980s.png",
       p1980s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_1990s.png",
       p1990s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_2000s.png",
       p2000s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_2010s.png",
       p2010s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

ggsave("mapobs_amph3_2020s.png",
       p2020s,
       device = "png",
       width = 2500,
       height = 2000,
       units = "px",
       dpi = 300)

# Join png files to create animated gif

gifski(c("mapobs_amph3_1930s.png",
         "mapobs_amph3_1940s.png",
         "mapobs_amph3_1950s.png",
         "mapobs_amph3_1960s.png",
         "mapobs_amph3_1970s.png",
         "mapobs_amph3_1980s.png",
         "mapobs_amph3_1990s.png",
         "mapobs_amph3_2000s.png",
         "mapobs_amph3_2010s.png",
         "mapobs_amph3_2020s.png"),
       gif_file = "mapobs_amph3_1950s_to_2020s.gif",
       delay = 1.5,
       width = 2500,
       height = 2000)
