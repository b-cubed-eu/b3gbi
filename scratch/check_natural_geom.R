
library(sf)
library(dplyr)

# Load the result from the natural reproduction
# Since I can't load the object directly, I'll just re-run the core logic
# and check for line geometries.

devtools::load_all(".")

# 1. Recreate the cube
grid_cells <- expand.grid(
  xcoord = seq(25, 55, by = 1),
  ycoord = seq(15, 55, by = 1),
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
  coord_range = list(xmin=2500000, xmax=5500000, ymin=1500000, ymax=5500000)
)
class(cube) <- "processed_cube"

# 2. Run the workflow
res <- compute_indicator_workflow(
  cube,
  type = "total_occ",
  dim_type = "map",
  level = "continent",
  region = "europe"
)

# 3. Check for non-polygonal geometries
geom_types <- st_geometry_type(res$data)
message("Geometry types in result:")
print(table(geom_types))

# 4. Check for extreme coordinates (the "Line to Space")
bbox <- st_bbox(res$data)
message("Bounding box of result:")
print(bbox)

# 5. Check for duplicates again
dupes <- res$data$cellCode[duplicated(res$data$cellCode)]
if (length(dupes) > 0) {
  message("STILL HAVE DUPLICATES: ", length(dupes))
} else {
  message("No duplicates found.")
}
