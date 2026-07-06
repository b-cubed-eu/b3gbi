
devtools::load_all(".")
library(sf)
library(dplyr)

# 1. Create a fake processed cube with 100km resolution
# Coordinates for 100kmE43N32 in EPSG:3035 are:
# Easting: 43 * 100,000 = 4,300,000
# Northing: 32 * 100,000 = 3,200,000
# We'll put some fake data in it
fake_data <- data.frame(
  scientificName = "Species A",
  taxonKey = 1,
  year = 2020,
  cellCode = "100kmE43N32",
  obs = 1,
  xcoord = 4300000,
  ycoord = 3200000,
  resolution = "100km"
)

cube <- list(
  data = fake_data,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=4300000, xmax=4400000, ymin=3200000, ymax=3300000)
)
class(cube) <- "processed_cube"

# 2. Create a shapefile that splits this 100km cell into two pieces
# The cell is a 100km square from (4.3M, 3.2M) to (4.4M, 3.3M)
# We create two boxes: one from 3.20M to 3.24M, and one from 3.26M to 3.30M
# This creates a gap in the middle, splitting the cell into two fragments.
wkt_content <- "MULTIPOLYGON (((4300000 3200000, 4400000 3200000, 4400000 3240000, 4300000 3240000, 4300000 3200000)), ((4300000 3260000, 4400000 3260000, 4400000 3300000, 4300000 3300000, 4300000 3260000)))"
shape_path <- "scratch/split_shape.wkt"
writeLines(wkt_content, shape_path)

# 3. Run the workflow
message("Running indicator workflow with split shapefile...")
res <- compute_indicator_workflow(
  cube,
  type = "obs_richness",
  dim_type = "map",
  shapefile_path = shape_path,
  shapefile_crs = "EPSG:3035"
)

# 4. Check results
n_rows <- nrow(res$data)
message("Number of rows in resulting map data: ", n_rows)

if (n_rows > 1) {
  message("BUG REPRODUCED: The single grid cell was split into ", n_rows, " rows.")
  message("This causes data duplication and opaque layering when plotting.")
} else {
  message("SUCCESS: The grid cell was correctly handled as a single row.")
}

# Also test the 0.25km bug if we can
fake_data_decimal <- data.frame(
  scientificName = "Species B",
  taxonKey = 2,
  year = 2020,
  cellCode = "0.25kmE4321N3210",
  obs = 1,
  xcoord = 1080250, # 4321 * 250
  ycoord = 802500,  # 3210 * 250
  resolution = "0.25km"
)
cube_decimal <- cube
cube_decimal$data <- fake_data_decimal
cube_decimal$resolutions <- "0.25km"
cube_decimal$coord_range <- list(xmin=1080250, xmax=1080500, ymin=802500, ymax=802750)

message("\nTesting 0.25km resolution parsing...")
# We use try because it might fail if the grid creation fails completely
try({
  res_decimal <- compute_indicator_workflow(
    cube_decimal,
    type = "obs_richness",
    dim_type = "map"
  )
  # Check if coordinates are near 0 or near the expected values
  bbox <- sf::st_bbox(res_decimal$data)
  if (bbox$xmin < 1000) {
    message("BUG REPRODUCED: 0.25km cell was mapped to (0,0) due to parsing error.")
  } else {
    message("SUCCESS: 0.25km cell was correctly mapped.")
  }
})
