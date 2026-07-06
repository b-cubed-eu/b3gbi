
library(b3gbi)
library(sf)
library(dplyr)
library(ggplot2)
devtools::load_all(".")

# 1. Create a fake processed cube with 100km resolution for Europe
message("Creating fake cube...")
grid_cells <- expand.grid(
  x = 35:50,
  y = 20:40
) %>%
  mutate(
    cellCode = paste0("100kmE", x, "N", y),
    diversity_val = ifelse(y < 32, 0.8, 0.4) # Southern cells have higher value (for visual)
  )

cube <- list(
  data = grid_cells %>% mutate(scientificName="A", taxonKey=1, year=2020, obs=1),
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=3500000, xmax=5000000, ymin=2000000, ymax=4000000)
)
class(cube) <- "processed_cube"

# 2. Build the indicator map MANUALLY to simulate the visual effect
# We'll split the cells at y=32.
# Top fragment: transparent. Bottom fragment: opaque.
grid <- create_native_grid(grid_cells, "EPSG:3035", "eea", "100km")

# Split cells at y = 3,250,000 (roughly Germany)
split_line <- st_linestring(matrix(c(3000000, 3250000, 6000000, 3250000), ncol=2, byrow=TRUE))
# We use a small buffer to make it a polygon for intersection
split_poly_top <- st_polygon(list(matrix(c(3000000, 3250000, 6000000, 3250000, 6000000, 5000000, 3000000, 5000000, 3000000, 3250000), ncol=2, byrow=TRUE)))
split_poly_bottom <- st_polygon(list(matrix(c(3000000, 1000000, 6000000, 1000000, 6000000, 3250000, 3000000, 3250000, 3000000, 1000000), ncol=2, byrow=TRUE)))

grid_top <- st_intersection(grid, st_sfc(split_poly_top, crs="EPSG:3035"))
grid_bottom <- st_intersection(grid, st_sfc(split_poly_bottom, crs="EPSG:3035"))

# Now simulate duplication in the bottom part only
# We repeat the bottom fragments 100 times!
grid_bottom_messy <- grid_bottom[rep(1:nrow(grid_bottom), each = 100), ]

joined <- bind_rows(grid_top, grid_bottom_messy) %>%
  left_join(grid_cells %>% select(cellCode, diversity_val), by = "cellCode")

# Create indicator_map object
ind_map <- list(
  data = joined,
  div_type = "obs_richness",
  projection = "EPSG:3035",
  map_level = "cube",
  coord_range = st_bbox(joined),
  grid_type = "eea"
)
class(ind_map) <- c("obs_richness", "indicator_map", "list")

# 4. Plot it
message("Plotting map with 100 layers in the South...")
p <- plot(ind_map, title = "Simulation of EEA Grid Fragmentation and Duplication")

# Save plot
ggsave("scratch/opaque_reproduction.png", p, width = 8, height = 8)
message("\n[SUCCESS] Map saved to scratch/opaque_reproduction.png")
message("Does this look like the user's issue? The bottom half should be solid/opaque,")
message("blocking the map borders, while the top half is transparent.")
