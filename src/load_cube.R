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

# Set parameters ----
region_level <- "continent" # country, continent, world
region <- "europe" # lower-case name of country or continent (or world)
grid_size <- 10000 # in meters
bias_corr_type <- "rarefaction" # type of bias correction (rarefaction, Hill)
first_year <- 1950 # start year of data
final_year <- 2022 # final year of data

# Load and prepare data ----

# Read in data cube
occurrence_data <- read_csv(file = "data/raw/eu_modellingtaxa_cube.csv",
                            col_types = cols(
                              year = col_double(),
                              eea_cell_code = col_character(),
                              taxonKey = col_double(),
                              n = col_double(),
                              min_coord_uncertainty = col_double()
                            ),
                            na = ""
)

# Read in associated taxonomic info
taxonomic_info <- read_csv("data/raw/eu_modellingtaxa_info.csv",
                           col_types = cols(
                             taxonKey = col_double(),
                             scientificName = col_character(),
                             rank = col_factor(),
                             taxonomicStatus = col_factor(),
                             kingdom = col_factor(),
                             includes = col_character()
                           ),
                           na = ""
)

# Merge the two data frames together on 'taxonKey'
merged_data <- left_join(occurrence_data, taxonomic_info, by = "taxonKey")

# Separate 'eea_cell_code' into resolution, coordinates
merged_data <- merged_data %>%
  mutate(
    xcoord = as.numeric(str_extract(eea_cell_code, "(?<=E)\\d+")),
    ycoord = as.numeric(str_extract(eea_cell_code, "(?<=N)\\d+")),
    resolution = str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
  )

# Remove columns that are not needed
merged_data <-
  merged_data %>%
  dplyr::select(-min_coord_uncertainty, -taxonomicStatus, -includes)

# Rename column n to obs
merged_data <-
  merged_data %>%
  rename(obs = n)

# Check whether start and end years are within dataset
first_year <- merged_data %>%
  select(year) %>%
  min() %>%
  ifelse(first_year > ., first_year, .)
final_year <- merged_data %>%
  select(year) %>%
  max() - 1 %>%
  ifelse(final_year < ., final_year, .)

# Limit data set
merged_data <-
  merged_data %>%
  filter(year >= first_year) %>%
  filter(year <= final_year)

# Calculate species richness ----

# Calculate species richness of entire cube by year
cube_species_richness <-
  merged_data %>%
  group_by(year) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop")

# Calculate total records of cube by year
cube_species_richness <-
  cube_species_richness %>%
  left_join(merged_data %>%
              group_by(year) %>%
              summarise(total_records = sum(obs), .groups = "drop"),
            by = "year")

# Adjust species richness using total records/year as proxy for sampling effort
cube_species_richness <-
  cube_species_richness %>%
  mutate(spec_rich_adj = species_richness / total_records * 500)

# Plot unadjusted cube richness trend
cube_richness_plot <-
  ggplot(cube_species_richness, aes(x = year, y = species_richness)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_smooth(color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(first_year,
                                  final_year,
                                  by = 5)) +
  labs(x = "Year", y = "Species Richness",
       title = paste("Time Series of Species Richness (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep="")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14))

# Plot adjusted cube richness trend
cube_richness_adj_plot <-
  ggplot(cube_species_richness, aes(x = year, y = spec_rich_adj)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_smooth(color = "red", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(first_year,
                                  final_year,
                                  by = 5)) +
  labs(x = "Year", y = "Species Richness",
       title = paste("Time Series of Species Richness (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep="")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14))



# Calculate number of records for each species by year
species_records_1970 <-
  merged_data %>%
  filter(year == 1970) %>%
  group_by(eea_cell_code, scientificName) %>%
  summarise(spec_rec = sum(obs), .groups = "drop") %>%
  pivot_wider(names_from = scientificName,
              values_from = spec_rec) %>%
  select(-eea_cell_code) %>%
  replace(is.na(.), 0)

# Calculate rarefied species richness for all data
sp1_1970 <- species_records_1970 %>%
  specaccum(method = "rarefaction")
sp2 <- species_records %>%
  specaccum(method = "random")

plot(sp1, xlab = "Number of Occurrences", ylab = "Species Richness",
     main = "Rarefaction Curves by Year")

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

# Create a community matrix with years as rows and species as columns
community_matrix <- table(merged_data$year, merged_data$scientificName)

# Calculate species accumulation curve
sac <- specaccum(community_matrix, method = "random")

# Extract observed counts
observed_counts <- sac$richness

# Create a data frame with the number of samples and the observed counts
sac_df <- data.frame(samples = 1:length(observed_counts), species = observed_counts)

# Fit a non-linear model to the species accumulation curve
model <- nlsLM(species ~ SSasymp(samples, Asym, R0, lrc), data = sac_df, start = list(Asym = max(sac_df$species), R0 = sac_df$species[1], lrc = 0))


# Extract the asymptote from the model
asymptote <- coef(model)["Asym"]

# Calculate the proportion of the asymptote reached each year
proportion_asymptote <- rowSums(community_matrix) / asymptote

# Adjust species richness
adjusted_richness <- rowSums(community_matrix) * proportion_asymptote

# Convert the adjusted richness into a data frame
adjusted_richness_df <- data.frame(year = names(adjusted_richness), species_richness = adjusted_richness)

# Calculate species richness cubely
cube_species_richness <- merged_data %>%
  group_by(year) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop")

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

# Download and prepare Natural Earth map data for Europe
map_data <- ne_countries(scale = "medium",
                         returnclass = "sf",
                         continent = "Europe") %>%
  st_as_sf() %>%
  st_transform(crs = "EPSG:3035")

# Filter the map data to only include Greece
greece <- map_data %>%
  select(name = "Greece")

# Make a grid across Greece
grid_greece <- greece %>%
  st_make_grid(cellsize = c(100000, 100000)) %>%
  st_intersection(greece) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

# Calculate species richness over the grid
richness_grid <- grid_greece %>%
  st_join(occ_sf_greece) %>%
 # mutate(overlap = ifelse(obs >= 1, 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(n_distinct(obs)))

lims_greece <- st_buffer(greece, dist = 1000) %>% st_bbox()

# Plot richness
richness_plot <-  ggplot(richness_grid) +
  geom_sf(data = greece, fill = "grey") +
  geom_sf(aes(fill = log(num_species + 1)), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
  coord_sf(
    xlim = c(lims_greece["xmin"], lims_greece["xmax"]),
    ylim = c(lims_greece["ymin"], lims_greece["ymax"])
  ) +
  scale_x_continuous() +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  ) + labs(fill = "richness")
richness_plot




# Calculate intersection between occurrences and grid cells
occ_grid_int_greece <- st_intersection(occ_sf_greece, grid_greece, left = TRUE)

# Extract coordinates of intersection points and create cell_id column
occ_grid_coords_greece <- occ_grid_int_greece %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(year = occ_grid_int_greece$year,
         obs = occ_grid_int_greece$obs,
         cell_id = paste0(X, "_", Y))

# Aggregate by cell ID, year and count unique taxa
richness_year_greece2 <- occ_grid_coords_greece %>%
  group_by(cell_id, year) %>%
  summarize(taxon_count = n_distinct(obs))

# Join richness data with grid data
grid_richness_greece <- grid_greece %>%
  st_join(st_as_sf(occ_grid_coords_greece, coords = c("X", "Y"), crs = "EPSG: 3035")) %>%
  group_by(cell_id)

# Create a base map of Greece
base_map <- ggplot() +
  geom_sf(data = greece, fill = "lightgray", color = "gray")   # Plot Greece

  # Create a plot of the grid with richness values
  richness_map <- base_map +
  geom_sf(data = grid_richness_greece, aes(fill = obs)) +  # Plot the grid with richness values
  scale_fill_gradient(low = "white", high = "red") +  # Customize the color scale
  labs(title = "Richness Values in Greece by Year") +
  theme_minimal()

richness_map












# Scale the coordinates of occurrences so the number of digits matches
merged_data_scaled <-
  merged_data %>%
  mutate(xcoord = xcoord * 1000, ycoord = ycoord * 1000)

# Convert the x and y columns to the correct format for plotting with sf
occ_sf <- st_as_sf(merged_data_scaled, coords = c("xcoord", "ycoord"), crs = "EPSG:3035")

 # Define EEA coordinate system
 crs_eea <- st_crs("EPSG:3035")

 # Define grid spacing and extent
 grid_spacing <- 1000 # 1 km
 grid_extent <- st_bbox(richness_year) %>%
   st_as_sfc() %>%
   st_set_crs(crs_eea) %>%
   st_buffer(grid_spacing * 2) %>%
   st_bbox()

 # Create grid
 grid <- st_make_grid(st_as_sfc(grid_extent), cellsize = grid_spacing) %>%
   st_sf() %>%
   mutate(cell_id = paste0(X, "_", Y))

 # Transform grid to EEA coordinate system
 grid_eea <- st_transform(grid, crs = crs_eea)

 # Calculate intersection between occurrences and grid cells
 occ_grid_int <- st_intersection(occ_sf, grid, left = TRUE)

 # Extract coordinates of intersection points and create cell_id column
 occ_grid_coords <- occ_grid_int %>%
   st_coordinates() %>%
   as_tibble() %>%
   mutate(year = occ_grid_int$year,
          obs = occ_grid_int$obs,
          cell_id = paste0(X, "_", Y))

# # Aggregate by cell ID, year and count unique taxa
# richness_year <- occ_grid_coords %>%
#  group_by(cell_id, year) %>%
#  summarize(taxon_count = n_distinct(obs))
#
# # Join richness data with grid data
# grid_richness <- st_join(richness_year, grid_eea, left = TRUE)

# Aggregate by cell ID, year and count unique taxa
richness_year <- occ_sf %>%
  group_by(eea_cell_code, year) %>%
  summarise(geometry = first(geometry), taxon_count = n_distinct(obs))

saveRDS(richness_year, file="richness_year.RDS")


year_to_plot <- 2020
europe <- map_data_tr

ggplot() +
  geom_sf(data = europe, fill = "white", color = "black") +
  geom_tile(data = subset(richness_year, year == year_to_plot),
            aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = taxon_count),
            alpha = 0.7) +
  scale_fill_gradient(low = "gray90", high = "blue", na.value = "gray90") +
  labs(title = paste("Species Richness Heatmap for Year", year_to_plot)) +
  theme_minimal()





# Plot map with background layer of Greece, grid cells based on richness values, and occurrence data in 2022
ggplot() +
  geom_sf(data = greece, fill = "lightgray") +
  geom_sf(data = filter(richness_year, year == 2021), aes(fill = taxon_count)) +
  scale_fill_gradient(low = "white", high = "blue") +
  #geom_sf(data = filter(occ_sf, year == 2021), color = "red", size = 0.5) +
  theme_void() +
  labs(title = "Occurrence data and taxon richness in Greece in 2021")

# Plot the map and GBIF occurrences using tmap
tm_shape(map_data_filt) +
  tm_polygons() +
  tm_shape(occ_sf) +
  tm_dots(col = "red", size = 0.5)



# Download map of Greece
greece <- ne_countries(country = "Greece", returnclass = "sf")

# Create a data frame with coordinates
df <- data.frame(x = 21.82, y = 39.07)

# Convert data frame to sf object
sf_object <- st_as_sf(df, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")

# Plot the map of Greece
tm_shape(greece) +
  tm_polygons() +
  tm_shape(sf_object) +
  tm_dots(size = 0.5) +
  tm_layout(frame = FALSE)


extent <- st_bbox(occ_sf)
resolution <- 1000

# Create an empty raster with the specified extent and resolution
empty_raster <- rast(extent = extent, resolution = resolution, crs = st_crs(occ_sf))


# Rasterize the point data onto the empty raster
rasterized_layer <- rasterize(richness_year$, empty_raster, field = "obs", fun = "sum")
