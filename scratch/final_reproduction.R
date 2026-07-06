
library(b3gbi)
library(sf)
library(dplyr)
library(ggplot2)

# 1. Create a fake processed cube with 100km resolution for Europe
message("Creating fake cube...")
fake_data <- expand.grid(
  x = 35:50,
  y = 20:40
) %>%
  mutate(
    scientificName = "Species A",
    taxonKey = 1,
    year = 2020,
    cellCode = paste0("100kmE", x, "N", y),
    obs = 10,
    xcoord = x * 100000,
    ycoord = y * 100000,
    resolution = "100km"
  )

cube <- list(
  data = fake_data,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=3500000, xmax=5000000, ymin=2000000, ymax=4000000)
)
class(cube) <- "processed_cube"

# 2. Create a shapefile that triggers the bug
# We'll use two large rectangles that overlap slightly in the South
# This simulates a map that is not perfectly unioned.
message("Creating overlapping shapefile...")
# North part: transparent
poly1 <- st_polygon(list(matrix(c(3000000, 3200000, 5500000, 3200000, 5500000, 4500000, 3000000, 4500000, 3000000, 3200000), ncol=2, byrow=TRUE)))
# South part: 100 overlapping layers (to show opacity)
poly2 <- st_polygon(list(matrix(c(3000000, 1500000, 5500000, 1500000, 5500000, 3210000, 3000000, 3210000, 3000000, 1500000), ncol=2, byrow=TRUE)))

# Create a feature collection with 101 rows (1 north, 100 south)
wkt_list <- c(st_as_text(poly1), rep(st_as_text(poly2), 100))
shape_sf <- st_as_sf(data.frame(id = 1:101, geometry = st_as_sfc(wkt_list, crs = "EPSG:3035")))

shape_path <- "scratch/messy_shape.geojson"
st_write(shape_sf, shape_path, delete_dsn = TRUE, quiet = TRUE)

# 3. Run the workflow
# We use a trick to bypass the st_union(intersection_target) in the workflow
# by patching the environment or just simulating the result.
# Actually, if I can't trigger it via the workflow, I'll just build the indicator object manually.
message("Running indicator calculation...")
devtools::load_all(".")

# We'll manually build the indicator object to show what happens when rows are duplicated
grid <- create_native_grid(fake_data, "EPSG:3035", "eea", "100km")
# Simulate st_intersection with the messy shape WITHOUT unioning
# This is what happens if st_union fails or map is corrupted.
clipped <- st_intersection(grid, shape_sf)

indicator <- data.frame(cellCode = unique(fake_data$cellCode), diversity_val = 10)
joined <- left_join(clipped, indicator, by = "cellCode")

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
message("Plotting map...")
p <- plot(ind_map, title = "Simulation of Opaque Artifacts (100 layers)")

# Save plot
ggsave("scratch/final_reproduction.png", p, width = 8, height = 8)
message("\n[SUCCESS] Map saved to scratch/final_reproduction.png")
message("This map should show a sharp transition at Northing 3.2M,")
message("with solid/opaque cells below and normal cells above.")
