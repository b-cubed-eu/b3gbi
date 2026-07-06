
library(sf)
library(dplyr)
library(ggplot2)
devtools::load_all(".")

# Manually replicate what plot_map does, step by step
set.seed(42)
grid_cells <- expand.grid(
  xcoord = seq(25, 55, by = 1),
  ycoord = seq(15, 55, by = 1)
) %>%
  mutate(
    cellCode = paste0("100kmE", xcoord, "N", ycoord),
    xcoord = xcoord * 100000,
    ycoord = ycoord * 100000,
    scientificName = "Species 1",
    obs = sample(1:50, n(), replace = TRUE),
    year = 2020,
    resolution = "100km"
  )

cube <- list(
  data = grid_cells,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=2500000, xmax=5500000, ymin=1500000, ymax=5500000)
)
class(cube) <- "processed_cube"

res <- compute_indicator_workflow(cube, type = "total_occ", dim_type = "map",
                                  level = "continent", region = "europe")

# Get data CRS
data_crs <- sf::st_crs(res$data)
latlong_crs <- sf::st_crs(4326)
projection <- data_crs$input  # EPSG:3035

# Compute latlong_extent
map_lims <- res$coord_range
latlong_extent <- map_lims %>%
  sf::st_bbox(crs = data_crs) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = latlong_crs) %>%
  sf::st_bbox()

expand_percent <- 0.5
lon_range <- max(latlong_extent["xmax"] - latlong_extent["xmin"], 0.1)
lat_range <- max(latlong_extent["ymax"] - latlong_extent["ymin"], 0.1)
latlong_extent <- sf::st_bbox(c(
  latlong_extent["xmin"] - (expand_percent * lon_range),
  latlong_extent["ymin"] - (expand_percent * lat_range),
  latlong_extent["xmax"] + (expand_percent * lon_range),
  latlong_extent["ymax"] + (expand_percent * lat_range)
), crs = latlong_crs)

# Get layers exactly as the pipeline does
countries_layer <- add_ne_layer("admin_0_countries", "medium", latlong_extent)
countries_proj <- sf::st_transform(countries_layer, crs = projection)

map_surround_raw <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
sf::st_agr(map_surround_raw) <- "constant"
map_surround <- map_surround_raw %>%
  sf::st_make_valid() %>%
  suppressMessages(sf::st_crop(latlong_extent)) %>%
  sf::st_transform(crs = projection)

data_proj <- sf::st_transform(res$data, crs = projection)

# Transform map_lims for coord_sf
map_lims_proj <- sf::st_bbox(map_lims, crs = data_crs) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = projection) %>%
  sf::st_bbox()

message("map_surround rows: ", nrow(map_surround))
message("countries_proj rows: ", nrow(countries_proj))
message("data_proj rows: ", nrow(data_proj))

# Plot 1: Just the country layer on its own (no data cells)
p1 <- ggplot() +
  geom_sf(data = countries_proj, fill = "transparent", colour = "red", linewidth = 0.3) +
  coord_sf(crs = projection,
           xlim = c(map_lims_proj["xmin"], map_lims_proj["xmax"]),
           ylim = c(map_lims_proj["ymin"], map_lims_proj["ymax"])) +
  theme_bw() +
  labs(title = "Plot 1: admin_0_countries layer ONLY (red)")

ggsave("scratch/diag_1_layer_only.png", p1, width = 10, height = 10, dpi = 150)
message("Saved diag_1_layer_only.png")

# Plot 2: map_surround only (should show all countries)
p2 <- ggplot() +
  geom_sf(data = map_surround, fill = "grey85", colour = "black", linewidth = 0.3) +
  coord_sf(crs = projection,
           xlim = c(map_lims_proj["xmin"], map_lims_proj["xmax"]),
           ylim = c(map_lims_proj["ymin"], map_lims_proj["ymax"])) +
  theme_bw() +
  labs(title = "Plot 2: map_surround ONLY (should show all borders)")

ggsave("scratch/diag_2_surround_only.png", p2, width = 10, height = 10, dpi = 150)
message("Saved diag_2_surround_only.png")

# Plot 3: Full pipeline reproduction
# Step 2: map_surround
# Step 3: layer (admin_0_countries) with transparent fill
# Step 4: data cells (opaque)
# Step 5: layer borders re-drawn on top
p3 <- ggplot(data_proj) +
  # Step 2
  geom_sf(data = map_surround, fill = "grey85", colour = "black",
          aes(geometry = geometry), inherit.aes = FALSE) +
  # Step 3
  geom_sf(data = countries_proj, fill = "transparent", colour = "black",
          aes(geometry = geometry), inherit.aes = FALSE) +
  # Step 4
  geom_sf(aes(fill = diversity_val, geometry = geometry),
          colour = alpha("black", 0.5), linewidth = 0.1) +
  scale_fill_gradient(low = "gold", high = "firebrick4", na.value = "transparent") +
  # Step 5: Re-draw layer borders on top
  geom_sf(data = countries_proj, fill = "transparent", colour = "black",
          aes(geometry = geometry), inherit.aes = FALSE) +
  coord_sf(crs = projection,
           xlim = c(map_lims_proj["xmin"], map_lims_proj["xmax"]),
           ylim = c(map_lims_proj["ymin"], map_lims_proj["ymax"])) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#92c5f0")) +
  labs(title = "Plot 3: Full pipeline - borders should be visible everywhere")

ggsave("scratch/diag_3_full_pipeline.png", p3, width = 10, height = 10, dpi = 150)
message("Saved diag_3_full_pipeline.png")
message("[DONE]")
