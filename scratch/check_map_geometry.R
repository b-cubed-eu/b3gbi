
library(b3gbi)
library(sf)
library(ggplot2)

# Get the map data used by the workflow
message("Getting map data...")
# Simulating the internal call in compute_indicator_workflow
# which calls get_ne_data(level="continent", region="europe")
devtools::load_all(".")
map_sf <- b3gbi:::get_ne_data(level = "continent", region = "europe", projected_crs = "EPSG:3035")

# Plot it
message("Plotting map data...")
p <- ggplot() + 
  geom_sf(data = map_sf, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Natural Earth Europe Map - Geometry Check")

ggsave("scratch/check_map_geometry.png", p, width = 10, height = 10)
message("\n[SUCCESS] Map saved to scratch/check_map_geometry.png")
message("Check this image for any horizontal lines or 'missing' countries.")
