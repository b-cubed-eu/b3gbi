# Mock data for testing
mock_tibble <- dplyr::as_tibble(data.frame(
  year = 2000:2010,
  cellCode = rep(1:5, length.out = 11),
  taxonKey = rep(1:3, length.out = 11),
  obs = 1:11,
  scientificName = rep("Species A", 11),
  xcoord = rep(1:2, length.out = 11),
  ycoord = rep(3:4, length.out = 11),
  resolution = rep("10km", 11),
  datasetKey = rep("dataset1", 11),
  basisOfRecord = rep("Observation", 11),
  kingdom = rep("Animalia", 11),
  family = rep("Family A", 11)
))

mock_tibble_no_dataset <- dplyr::as_tibble(data.frame(
  year = 2000:2010,
  cellCode = rep(1:5, length.out = 11),
  taxonKey = rep(1:3, length.out = 11),
  obs = 1:11,
  scientificName = rep("Species A", 11),
  xcoord = rep(1:2, length.out = 11),
  ycoord = rep(3:4, length.out = 11),
  resolution = rep("10km", 11),
  kingdom = rep("Animalia", 11),
  family = rep("Family A", 11)
))

mock_sim_tibble <- dplyr::as_tibble(data.frame(
  year = 2000:2010,
  taxonKey = rep(1:3, length.out = 11),
  obs = 1:11,
  kingdom = rep("Animalia", 11),
  family = rep("Family A", 11),
  xcoord = rep(1:2, length.out = 11),
  ycoord = rep(3:4, length.out = 11),
  cellCode = rep(1:5, length.out = 11),
  resolution = rep("10km", 11)
))

mock_sim_tibble_no_coord <- dplyr::as_tibble(data.frame(
  year = 2000:2010,
  taxonKey = rep(1:3, length.out = 11),
  obs = 1:11,
  kingdom = rep("Animalia", 11),
  family = rep("Family A", 11)
))

mock_indicator_ts_tibble <- dplyr::as_tibble(data.frame(
  year = 2000:2010,
  diversity_val = 1:11
))

mock_indicator_map_sf <- sf::st_sf(
  cellid = 1:10,
  diversity_val = 1:10,
  geometry = sf::st_sfc(lapply(1:10, function(i) sf::st_point(c(i, i))))
)

# Tests for new_processed_cube
test_that("new_processed_cube creates processed_cube object correctly", {
  pc <- new_processed_cube(mock_tibble, grid_type = "UTM")
  expect_s3_class(pc, "processed_cube_dsinfo")
  expect_equal(pc$first_year, 2000)
  expect_equal(pc$last_year, 2010)
  expect_equal(pc$num_cells, 5)
  expect_equal(pc$num_species, 3)
  expect_equal(pc$num_obs, sum(1:11))
  expect_equal(pc$grid_type, "UTM")
  expect_equal(pc$resolutions, "10km")
  expect_equal(pc$num_datasets, 1)
  expect_equal(pc$record_types, "Observation")
})

test_that(
  "new_processed_cube creates processed_cube object correctly, no dataset", {
  pc <- new_processed_cube(mock_tibble_no_dataset, grid_type = "UTM")
  expect_s3_class(pc, "processed_cube")
  expect_equal(pc$first_year, 2000)
  expect_equal(pc$last_year, 2010)
  expect_equal(pc$num_cells, 5)
  expect_equal(pc$num_species, 3)
  expect_equal(pc$num_obs, sum(1:11))
  expect_equal(pc$grid_type, "UTM")
  expect_equal(pc$resolutions, "10km")
})

test_that("new_processed_cube handles missing columns", {
  expect_error(new_processed_cube(mock_tibble[, -1], grid_type = "UTM"))
})

# Tests for new_sim_cube
test_that("new_sim_cube creates sim_cube object correctly", {
  sc <- new_sim_cube(mock_sim_tibble, grid_type = "UTM")
  expect_s3_class(sc, "sim_cube")
  expect_equal(sc$first_year, 2000)
  expect_equal(sc$last_year, 2010)
  expect_equal(sc$num_cells, 5)
  expect_equal(sc$num_species, 3)
  expect_equal(sc$num_obs, sum(1:11))
  expect_equal(sc$grid_type, "UTM")
  expect_equal(sc$resolutions, "10km")
})

test_that(
  "new_sim_cube creates sim_cube object correctly with no coordinates", {
  sc <- new_sim_cube(mock_sim_tibble_no_coord, grid_type = "UTM")
  expect_s3_class(sc, "sim_cube")
  expect_equal(sc$coord_range, "Coordinates not provided")
  expect_equal(sc$num_cells, "No cell codes provided")
})

test_that("new_sim_cube handles missing columns", {
  expect_error(new_sim_cube(mock_sim_tibble[, -1], grid_type = "UTM"))
})

# Tests for new_indicator_ts
test_that("new_indicator_ts creates indicator_ts object correctly", {
  its <- new_indicator_ts(
    x = mock_indicator_ts_tibble,
    div_type = "obs_richness",
    map_level = "country",
    map_region = "Test Country",
    map_type = "countries",
    kingdoms = "Animalia",
    num_families = 1,
    num_species = 3,
    num_years = 11,
    species_names = NULL,
    coord_range = list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
  )
  expect_s3_class(its, c("indicator_ts", "obs_richness"))
  expect_equal(its$div_type, "obs_richness")
  expect_equal(its$first_year, 2000)
  expect_equal(its$last_year, 2010)
})

test_that("new_indicator_ts handles missing columns", {
  expect_error(new_indicator_ts(mock_indicator_ts_tibble[, -1], "obs_richness"))
})

# Tests for new_indicator_map
test_that("new_indicator_map creates indicator_map object correctly", {
  im <- new_indicator_map(
    x = mock_indicator_map_sf,
    div_type = "obs_richness",
    cell_size = 10,
    cell_size_units = "km",
    map_level = "country",
    map_region = "Test Country",
    map_type = "countries",
    kingdoms = "Animalia",
    num_families = 1,
    num_species = 3,
    first_year = 2000,
    last_year = 2010,
    num_years = 11,
    species_names = NULL,
    years_with_obs = 11,
    original_bbox = list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
  )
  expect_s3_class(im, c("indicator_map", "obs_richness"))
  expect_equal(im$div_type, "obs_richness")
  expect_equal(im$cell_size, "10 km^2")
  expect_equal(im$first_year, 2000)
  expect_equal(im$last_year, 2010)
})

test_that("new_indicator_map handles missing columns", {
  expect_error(
    new_indicator_map(
      mock_indicator_map_sf[, -1], "obs_richness", 10, "km"
    )
  )
})
