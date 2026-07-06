
library(sf)
library(dplyr)
devtools::load_all(".")

# 1. Recreate the cube (Fixed to be within the map area)
grid_cells <- expand.grid(
  xcoord = 43,
  ycoord = 15:55, # One vertical line of cells
  scientificName = "Species 1"
) %>%
  mutate(
    cellCode = paste0("100kmE", xcoord, "N", ycoord),
    xcoord = xcoord * 100000,
    ycoord = ycoord * 100000,
    resolution = "100km",
    obs = 1
  )

cube <- list(
  data = grid_cells,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=4300000, xmax=4400000, ymin=1500000, ymax=5500000)
)
class(cube) <- "processed_cube"

# 2. Run the workflow
message("Running workflow...")
res <- compute_indicator_workflow(
  cube,
  type = "total_occ",
  dim_type = "map",
  level = "continent",
  region = "europe"
)

# 3. Inspect every geometry
message("Inspecting geometries...")
geoms <- st_geometry(res$data)
for (i in seq_along(geoms)) {
  g <- geoms[[i]]
  if (!st_is_valid(g)) {
    message("CELL ", res$data$cellCode[i], " IS INVALID!")
    print(st_is_valid(g, reason = TRUE))
  }
  
  # Check for "huge" bounding box (indicates a wrap-around line)
  b <- st_bbox(g)
  if (abs(b$xmax - b$xmin) > 500000 || abs(b$ymax - b$ymin) > 500000) {
    message("CELL ", res$data$cellCode[i], " HAS EXTREME SIZE: ", abs(b$xmax - b$xmin))
    print(b)
  }
}

# 4. Check the base map again
message("Checking base map for Europe...")
map_sf <- b3gbi:::get_ne_data(level = "continent", region = "europe", projected_crs = "EPSG:3035")
message("Base map is valid: ", all(st_is_valid(map_sf$combined)))
if (!all(st_is_valid(map_sf$combined))) {
  print(st_is_valid(map_sf$combined, reason = TRUE))
}
