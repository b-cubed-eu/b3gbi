# --- Mock Data for Testing ---
# UTM Zone 32N (EPSG: 32632)
data_N <- data.frame(
  xcoord = c(450000, 451000),
  ycoord = c(5500000, 5501000),
  utmzone = c(32, 32),
  hemisphere = c("N", "N"),
  name = c("PtN1", "PtN2")
)

# UTM Zone 18S (EPSG: 32718)
data_S <- data.frame(
  xcoord = c(300000, 301000),
  ycoord = c(6000000, 6001000),
  utmzone = c(18, 18),
  hemisphere = c("S", "S"),
  name = c("PtS1", "PtS2")
)

# Combined Data (to test multi-zone handling)
data_combined <- rbind(data_N, data_S)

# Empty Data
data_empty <- data.frame(
  xcoord = numeric(), ycoord = numeric(), utmzone = numeric(), hemisphere = character()
)

# --- End of Mock Data for Testing ---

# Test Block 1: Input Validation
# Covers the three 'stop' conditions at the beginning of the function
test_that("Input validation stops on missing columns or wrong data types", {

  # 1. Missing required columns (xcoord missing)
  expect_error(
    create_sf_from_utm(data.frame(ycoord = 1, utmzone = 1, hemisphere = "N")),
    "Input data frame must contain columns:xcoord, ycoord, utmzone, hemisphere",
    fixed = TRUE
  )

  # 2. Missing required columns (hemisphere missing)
  expect_error(
    create_sf_from_utm(data.frame(xcoord = 1, ycoord = 1, utmzone = 1)),
    "Input data frame must contain columns:xcoord, ycoord, utmzone, hemisphere",
    fixed = TRUE
  )

  # 3. Non-numeric coordinates (xcoord is character)
  data_type_err <- data_N
  data_type_err$xcoord <- as.character(data_type_err$xcoord)
  expect_error(
    create_sf_from_utm(data_type_err),
    "xcoord, ycoord, and utmzone columns must be numeric."
  )

})

# Test Block 2: Single-Zone Conversions and CRS Assignment
test_that("Single zone conversion assigns correct CRS (N/S)", {

  # 1. Northern Hemisphere (UTM 32N -> EPSG: 32632)
  sf_n <- create_sf_from_utm(data_N)
  expect_true(inherits(sf_n, "sf"))
  expect_equal(sf::st_crs(sf_n)$epsg, 32632)
  expect_equal(nrow(sf_n), 2)

  # 2. Southern Hemisphere (UTM 18S -> EPSG: 32718)
  sf_s <- create_sf_from_utm(data_S)
  expect_true(inherits(sf_s, "sf"))
  expect_equal(sf::st_crs(sf_s)$epsg, 32718)
})

# Test Block 3: Multi-Zone Merging and Transformation
test_that("Multi-zone data merges and transforms correctly", {

  # 1. Merge and transform to WGS84 (EPSG: 4326)
  output_crs_wgs84 <- 4326
  combined_wgs84 <- create_sf_from_utm(data_combined, output_crs = output_crs_wgs84)

  # Check overall structure and CRS
  expect_equal(nrow(combined_wgs84), 4) # 2 N + 2 S = 4 total rows
  expect_equal(sf::st_crs(combined_wgs84)$epsg, 4326)

  # Check if the attributes were preserved
  expect_true(all(c("PtN1", "PtS2") %in% combined_wgs84$name))

  # 2. Merge and use default CRS (from the first group, UTM 32N -> 32632)
  combined_default <- create_sf_from_utm(data_combined)

  # Check CRS when output_crs is NULL (it defaults to the first zone, 32632)
  expect_equal(sf::st_crs(combined_default)$epsg, 32718)

  # 3. Test empty data frame (activates the 'else' branch for combined_sf)
  sf_empty <- create_sf_from_utm(data_empty)
  expect_true(inherits(sf_empty, "sf"))
  expect_equal(nrow(sf_empty), 0)
  expect_true(length(sf::st_geometry(sf_empty)) == 0)
})
