
library(sf)
library(dplyr)
library(ggplot2)
devtools::load_all(".")

# Simulate what plot_map does to get the layer data
# First, run the workflow to get an indicator_map object
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

# Now trace what prepare_map_for_plot does
data_crs <- sf::st_crs(res$data)
latlong_crs <- sf::st_crs(4326)
map_lims <- res$coord_range

message("=== DIAGNOSTIC: Layer Extents ===")
message("Data CRS: ", data_crs$input)
message("map_lims (coord_range): ", paste(map_lims, collapse=", "))

# Compute latlong_extent the same way prepare_map_data does
latlong_extent <- map_lims %>%
  sf::st_bbox(crs = data_crs) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = latlong_crs) %>%
  sf::st_bbox()

message("latlong_extent BEFORE expansion: ")
message("  xmin=", latlong_extent["xmin"], " ymin=", latlong_extent["ymin"],
        " xmax=", latlong_extent["xmax"], " ymax=", latlong_extent["ymax"])

# Expand by 50%
expand_percent <- 0.5
lon_range <- max(latlong_extent["xmax"] - latlong_extent["xmin"], 0.1)
lat_range <- max(latlong_extent["ymax"] - latlong_extent["ymin"], 0.1)
latlong_extent <- sf::st_bbox(c(
  latlong_extent["xmin"] - (expand_percent * lon_range),
  latlong_extent["ymin"] - (expand_percent * lat_range),
  latlong_extent["xmax"] + (expand_percent * lon_range),
  latlong_extent["ymax"] + (expand_percent * lat_range)
), crs = latlong_crs)

message("latlong_extent AFTER expansion: ")
message("  xmin=", latlong_extent["xmin"], " ymin=", latlong_extent["ymin"],
        " xmax=", latlong_extent["xmax"], " ymax=", latlong_extent["ymax"])

# Get map_surround (Step 2 in plot) 
map_data_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_as_sf()
sf::st_agr(map_data_sf) <- "constant"
map_surround <- map_data_sf %>%
  sf::st_make_valid() %>%
  suppressMessages(sf::st_crop(latlong_extent))

message("\nmap_surround extent (lat/long):")
surround_bbox <- sf::st_bbox(map_surround)
message("  xmin=", surround_bbox["xmin"], " ymin=", surround_bbox["ymin"],
        " xmax=", surround_bbox["xmax"], " ymax=", surround_bbox["ymax"])

# Get the layer (Step 5 in plot)
layer_data <- add_ne_layer("admin_0_countries", "medium", latlong_extent)

message("\nadmin_0_countries layer extent (lat/long):")
layer_bbox <- sf::st_bbox(layer_data)
message("  xmin=", layer_bbox["xmin"], " ymin=", layer_bbox["ymin"],
        " xmax=", layer_bbox["xmax"], " ymax=", layer_bbox["ymax"])

message("\nmap_surround nrow: ", nrow(map_surround))
message("admin_0_countries layer nrow: ", nrow(layer_data))

# Check if layer columns scalerank/featurecla exist
message("\nadmin_0_countries columns: ", paste(names(layer_data), collapse=", "))

# Now let's plot JUST the layers to see where they end
projection <- sf::st_crs(res$data)$input

map_surround_proj <- sf::st_transform(map_surround, crs = projection)
layer_data_proj <- sf::st_transform(layer_data, crs = projection)
data_proj <- sf::st_transform(res$data, crs = projection)

p <- ggplot() +
  # Step 2: map_surround (land base, BLUE to distinguish)
  geom_sf(data = map_surround_proj, fill = "lightblue", colour = "blue", linewidth = 0.3) +
  # Step 4: data cells (semi-transparent to see through)
  geom_sf(data = data_proj, aes(fill = diversity_val, geometry = geometry),
          alpha = 0.3, colour = NA) +
  scale_fill_gradient(low = "yellow", high = "red") +
  # Step 5: admin_0_countries borders (RED, thick)
  geom_sf(data = layer_data_proj, fill = "transparent", colour = "red", linewidth = 0.5) +
  theme_void() +
  labs(title = "DIAGNOSTIC: Blue=map_surround, Red=layer borders on top of data")

ggsave("scratch/layer_diagnostic.png", p, width = 12, height = 12)
message("\n[SUCCESS] Layer diagnostic saved to scratch/layer_diagnostic.png")
