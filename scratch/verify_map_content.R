
library(sf)
library(dplyr)
devtools::load_all(".")

# Get the map data for Europe
message("Fetching map for Europe...")
map_sf <- b3gbi:::get_ne_data(level = "continent", region = "europe", projected_crs = "EPSG:3035")
message("Map rows: ", nrow(map_sf$combined))
message("Map Bbox:")
print(st_bbox(map_sf$combined))

# Create a test point in Germany (EPSG:3035)
pt <- st_sfc(st_point(c(4350000, 3250000)), crs = "EPSG:3035")
intersects <- st_intersects(pt, map_sf$combined, sparse = FALSE)
message("Point in Germany intersects map: ", intersects)
