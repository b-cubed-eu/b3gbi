
library(b3gbi)
library(sf)
library(dplyr)
library(ggplot2)

# 1. Create a fake processed cube with 4 cells in a 2x2 grid (100km resolution)
# Cells: E43N32 (Germany), E44N32, E43N31, E44N31
message("Creating fake cube...")
fake_data <- expand.grid(
  x = 43:44,
  y = 31:32
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
  coord_range = list(xmin=4300000, xmax=4500000, ymin=3100000, ymax=3300000)
)
class(cube) <- "processed_cube"

# 2. Create a shapefile that triggers the bug
# Top half (y=32): One clean polygon
# Bottom half (y=31): 20 overlapping polygons (to simulate duplication)
message("Creating problematic shapefile...")
poly_top <- "POLYGON ((4300000 3200000, 4500000 3200000, 4500000 3300000, 4300000 3300000, 4300000 3200000))"
poly_bottom <- "POLYGON ((4300000 3100000, 4500000 3100000, 4500000 3200000, 4300000 3200000, 4300000 3100000))"

# Create a feature collection where the bottom part is repeated 20 times
wkt_list <- c(poly_top, rep(poly_bottom, 20))
# IMPORTANT: To avoid the 'is_empty' bug in the current package, 
# we need to keep it as separate features but the package fails if they are multiple.
# Actually, the bug is that st_is_empty returns a vector.
# Let's just create 21 rows in a data frame.
shape_sf <- st_as_sf(data.frame(id = 1:21, geometry = st_as_sfc(wkt_list, crs = "EPSG:3035")))

# Save as a shapefile (actually GeoJSON is easier for this test)
shape_path <- "scratch/problematic_shape.geojson"
st_write(shape_sf, shape_path, delete_dsn = TRUE, quiet = TRUE)

# 3. Run the workflow
# We use load_all to ensure we are testing the current (buggy) state
message("Running indicator workflow...")
devtools::load_all(".")
res <- compute_indicator_workflow(
  cube,
  type = "total_occ",
  dim_type = "map",
  shapefile_path = shape_path,
  shapefile_crs = "EPSG:3035"
)

message("Rows in resulting map data: ", nrow(res$data))
# Should be 1 (top) + 20 (bottom) = 21 rows for 4 cells? 
# Wait, 2 cells in top (2 rows) + 2 cells in bottom (40 rows) = 42 rows.
message("Cell counts:")
print(table(res$data$cellCode))

# 4. Plot the map
message("Plotting map...")
# We use a custom alpha to show the layering effect
# But even with default, 20 layers will look very different from 1 layer.
p <- plot(res, title = "Visual Reproduction of EEA Fragmentation Bug")

# Save plot
ggsave("scratch/visual_bug_reproduction.png", p, width = 8, height = 8)
message("\n[SUCCESS] Visual reproduction map saved to scratch/visual_bug_reproduction.png")
message("Look at the image: the bottom two cells should be MUCH darker/opaque than the top two,")
message("and you will see a sharp line between them at the 3.2M Northing mark.")
