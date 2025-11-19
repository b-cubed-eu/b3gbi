# --- Mock Data Setup ---

# Define a simple mock cube class for testing the S3 methods
processed_cube <- function(grid_type, data, coord_range) {
  structure(list(
    grid_type = grid_type,
    data = data,
    coord_range = coord_range
  ), class = "processed_cube")
}

# 1. Cube: Single UTM Zone (32U) - Should return UTM CRS
cube_single_utm <- processed_cube(
  grid_type = "mgrs",
  data = data.frame(cellCode = c("32U NS 1234 5678", "32U LV 9988 7766"), value = 1),
  coord_range = st_bbox(c(xmin = 10, ymin = 50, xmax = 11, ymax = 51), crs = "EPSG:4326")
)

# 2. Cube: Multiple UTM Zones (32U and 33U) - Should return Global CRS (ESRI:54012)
cube_multi_utm <- processed_cube(
  grid_type = "mgrs",
  data = data.frame(cellCode = c("32U NS 1234 5678", "33U LV 9988 7766"), value = 1),
  coord_range = st_bbox(c(xmin = 10, ymin = 50, xmax = 18, ymax = 51), crs = "EPSG:4326")
)

# 3. Cube: Non-MGRS grid type - Should return NULL
cube_non_mgrs <- processed_cube(
  grid_type = "h3",
  data = data.frame(cellCode = "89283082cffffff", value = 1),
  coord_range = st_bbox(c(xmin = 10, ymin = 50, xmax = 11, ymax = 51), crs = "EPSG:4326")
)

# 4. Cube: Southern Hemisphere UTM Zone (18C) - Should return South UTM CRS
cube_south_utm <- processed_cube(
  grid_type = "mgrs",
  data = data.frame(cellCode = "18C FG 1234 5678", value = 1),
  coord_range = st_bbox(c(xmin = -75, ymin = -30, xmax = -74, ymax = -29), crs = "EPSG:4326")
)

# 5. BBox: Small, North Hemisphere (UTM 31N) - Should return UTM CRS
bbox_north <- st_bbox(c(xmin = 0.5, ymin = 45, xmax = 1.5, ymax = 46), crs = "EPSG:4326")

# 6. BBox: Small, South Hemisphere (UTM 18S) - Should return UTM CRS
bbox_south <- st_bbox(c(xmin = -75, ymin = -30, xmax = -74, ymax = -29), crs = "EPSG:4326")

# 7. BBox: Too wide (long_span > 6) - Should return Global CRS
bbox_wide <- st_bbox(c(xmin = 10, ymin = 50, xmax = 20, ymax = 51), crs = "EPSG:4326")

# 8. BBox: Too tall (lat_span > 15) - Should return Global CRS
bbox_tall <- st_bbox(c(xmin = 10, ymin = 40, xmax = 11, ymax = 60), crs = "EPSG:4326")

# 9. BBox: Polar North (ymax > 84) - Should return Global CRS
bbox_polar_n <- st_bbox(c(xmin = 10, ymin = 83, xmax = 11, ymax = 85), crs = "EPSG:4326")

# 10. BBox: Crosses UTM Zone boundary (!single_utm_zone) - Should return Global CRS
# xmin 5.9 (Zone 31), xmax 6.1 (Zone 32)
bbox_crosses_zone <- st_bbox(c(xmin = 5.9, ymin = 50, xmax = 6.1, ymax = 51), crs = "EPSG:4326")

# 11. Invalid Input - Should return NULL and throw warning
invalid_input <- data.frame(a = 1, b = 2)

# 12. Valid Input, but missing coord_range (triggers coord_range check)
cube_missing_coord <- processed_cube(
  grid_type = "mgrs",
  data = data.frame(cellCode = "32U LV 9988 7766", value = 1),
  coord_range = NULL # Missing
)


# --- Test Block 1: MGRS/Processed Cube Path (Activating the first IF branch) ---
test_that("MGRS/Cube logic correctly determines CRS or global default", {

  # 1. Single UTM Zone (32U) -> EPSG:32632
  expect_equal(guess_utm_epsg(cube_single_utm), "EPSG:32632")

  # 2. Single UTM Zone, South (18C) -> EPSG:32718 (Activating the South hemisphere branch)
  expect_equal(guess_utm_epsg(cube_south_utm), "EPSG:32718")

  # 3. Multiple UTM Zones -> ESRI:54012 (Activating the unique_utm_zones > 1 branch)
  expect_equal(guess_utm_epsg(cube_multi_utm), "ESRI:54012")

  # 4. Non-MGRS grid type -> NULL (Activating the grid_type != "mgrs" branch)
  expect_null(guess_utm_epsg(cube_non_mgrs))
})


# --- Test Block 2: BBox Path & Failure Modes (Activating all other IF/ELSE branches) ---
test_that("BBox logic correctly determines UTM, Global, or failure", {

  # 1. Valid BBox, fits UTM criteria (North) -> EPSG:32631 (Activating the final UTM North branch)
  expect_equal(guess_utm_epsg(bbox_north), "EPSG:32631")

  # 2. Valid BBox, fits UTM criteria (South) -> EPSG:32718 (Activating the final UTM South branch)
  expect_equal(guess_utm_epsg(bbox_south), "EPSG:32718")

  # 3. BBox too wide (> 6 deg) -> ESRI:54012 (Activating long_span > 6 branch)
  expect_equal(guess_utm_epsg(bbox_wide), "ESRI:54012")

  # 4. BBox too tall (> 15 deg) -> ESRI:54012 (Activating lat_span > 15 branch)
  expect_equal(guess_utm_epsg(bbox_tall), "ESRI:54012")

  # 5. BBox crosses 84N boundary -> ESRI:54012 (Activating coord_range["ymax"] > 84 branch)
  expect_equal(guess_utm_epsg(bbox_polar_n), "ESRI:54012")

  # 6. BBox crosses UTM Zone boundary -> ESRI:54012 (Activating !single_utm_zone branch)
  expect_equal(guess_utm_epsg(bbox_crosses_zone), "ESRI:54012")

  # 7. Invalid Input (data frame) -> NULL and Warning (Activating the final ELSE branch)
  expect_warning(
    result <- guess_utm_epsg(invalid_input),
    "Input must be a 'processed_cube' object or an 'sf::bbox' object."
  )
  expect_null(result)

  # 8. Valid Cube, but missing coord_range -> Returns valid code.
  result <- guess_utm_epsg(cube_missing_coord)
  expect_equal(result, "EPSG:32632")

})
