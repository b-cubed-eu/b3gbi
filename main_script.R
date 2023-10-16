# Calculate species richness from occurrence cubes

# Load required packages ----
library(tidyverse)
library(vegan)
library(minpack.lm)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tmap)
library(terra)
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

richness_raw <- calc_srt(merged_data, method="raw")
richness_tr <- calc_srt(merged_data, method="total_records")
richness_rf <- calc_srt(merged_data, method="rarefaction")



# Calculate number of records for each species by year
species_records <-
  merged_data %>%
  dplyr::group_by(year) %>%
  dplyr::group_split() %>%
  purrr::map(. %>%
               dplyr::group_by(scientificName) %>%
               dplyr::summarise(spec_rec = sum(obs), .groups = "drop") %>%
               tidyr::pivot_wider(names_from = scientificName, values_from = spec_rec) %>%
               replace(is.na(.), 0)
  )

species_records_df <- species_records %>%
  list_rbind() %>%
  replace(is.na(.), 0)

raremax <- min(rowSums(species_records_df))

TES_curves <- species_records %>%
  purrr::map(. %>%
               unlist() %>%
               TES(knots=10)
  )

TES_trend <- TES_curves %>%
  purrr::map(. %>%
               pluck(1) %>%
               select(a) %>%
               slice(1)
             ) %>%
  unlist()

Chaodf <- ChaoRichness(t(species_records_df2), datatype="incidence_freq", conf = 0.95)

# Calculate number of records for each species by year
species_records_raw <-
  merged_data %>%
  dplyr::group_by(year) %>%
  dplyr::group_split() %>%
  purrr::map(. %>%
               dplyr::group_by(eea_cell_code, scientificName) %>%
               tidyr::pivot_wider(names_from = scientificName, values_from = obs) %>%
               ungroup() %>%
               dplyr::select(-eea_cell_code, -taxonKey, -kingdom, -rank, -xcoord, -ycoord, -resolution) %>%
               replace(is.na(.), 0) %>%
               mutate_if(is.numeric, as.integer)
  )

species_records_raw2 <-
  species_records_raw %>%
  purrr::map(. %>%
               select(-year) %>%
               rownames_to_column %>%
               gather(variable, value, -rowname) %>%
               spread(rowname, value))

species_records_raw3 <-
  species_records_raw2 %>%
  purrr::map(. %>%
               'row.names<-'(., NULL) %>%
               column_to_rownames(var = "variable") %>%
               as.matrix() %>%
               ifelse(. > 1, 1, .))

coverage_rare <- species_records_raw3 %>%
  iNEXT(endpoint=150, datatype="incidence_raw")

# save sample sizes for running iNEXT independently for each data year
inext_samp_sizes <- rowSums(species_records_df)

# set output directory
inext_output_dir <- "inext_output"

inext_results_list <-
  pmap(species_records_raw3, as.list(inext_samp_sizes), 1:72, ~{
    year_data <- .1
    year_sampsize <- .2
    year_index <- .3

    inext_year_output <- iNEXT(year_data,
                               endpoint = year_sampsize,
                               datatype = "incidence_raw")
  }
         )



# Calculate rarefaction curves for each year
future::plan(multisession)
rarecurves <- species_records_df2 %>%
  vegan::rarecurve(step=20, sample=raremax)


# Plot rarefaction curves
spec_rare_plots <- spec_rare %>%
  furrr::future_map(plot,
                     xlab = "Number of Occurrences",
                     ylab = "Species Richness",
                     main = "Rarefaction Curves by Year")
saveRDS(spec_rare_plots, "spec_rare_plots.RData")




# Plot observed and adjusted richness trends
ggplot(cube_species_richness, aes(x = year)) +
  geom_smooth(aes(y = species_richness), color = "green") +
  geom_smooth(aes(y = mean_rarefied_richness), color = "blue") +
  geom_smooth(aes(y = adjusted_richness), color = "red") +
  labs(x = "Year", y = "Species Richness") +
  scale_y_continuous(
    limits = c(0,
               max(cube_species_richness$mean_rarified_richness,
                   cube_species_richness$adjusted_richness))
  ) +
  theme_classic()

--------------------------------------------------------------------------------


# Create a dataframe with the richness data
richness_df <- observed_richness %>%
  data.frame(year = year,
             observed_species = x,
             rarefied_species = rarefied_richness)

# Plot observed and rarefied species richness over time
ggplot(richness_df, aes(x = year)) +
  geom_line(aes(y = observed_species), color = "blue") +
  geom_line(aes(y = rarefied_species), color = "red") +
  labs(x = "Year", y = "Species Richness") +
  scale_y_continuous(limits = c(0, max(richness_df$observed_species, richness_df$rarefied_species))) +
  theme_classic()

# --------------------------------------------------------------------------

# Calculate species richness by site
site_species_richness <-
  merged_data %>%
  group_by(eea_cell_code, year) %>%
  summarise(species_richness = n_distinct(scientificName),
            .groups = "drop")

# Calculate species richness by kingdom
kingdom_species_richness <-
  merged_data %>%
  group_by(year, kingdom, eea_cell_code) %>%
  summarise(species_richness = n_distinct(scientificName),
            .groups = "drop") %>%
  pivot_wider(names_from = kingdom, values_from = species_richness)

# Create a grid of eea_cell_code and year combinations for site-based richness
complete_grid <- expand_grid(
  eea_cell_code = unique(site_species_richness$eea_cell_code),
  year = unique(site_species_richness$year)
)

# Left join the complete grid with the site-based richness data
site_species_richness_filled <-
  complete_grid %>%
  left_join(site_species_richness,
            by = c("eea_cell_code", "year")) %>%
  arrange(eea_cell_code, year) %>%
  replace_na(list(species_richness = 0))

# --------------------------------------------------------------------------

# Calculate species richness by country
# Assuming that x_coord and y_coord can be used to determine the country
# You might need to replace this with your own logic to map coordinates to countries
country_species_richness <- merged_data %>%
  group_by(year, x_coord, y_coord) %>%
  summarise(species_richness = n_distinct(scientificName))

# Calculate species richness by kingdom
# Calculate species richness by kingdom
kingdom_species_richness <- merged_data %>%
  group_by(year, kingdom) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop") %>%
  pivot_wider(names_from = kingdom, values_from = species_richness)

# --------------------------------------------------------------------------

