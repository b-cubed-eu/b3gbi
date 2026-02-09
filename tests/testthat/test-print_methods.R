# Mock Objects
mock_indicator_ts <- list(
  div_name = "Test Indicator TS",
  first_year = 2000,
  last_year = 2010,
  map_region = "Test Region",
  coord_range = c(1, 2, 3, 4),
  num_species = 10,
  num_families = c(5, 6),
  kingdoms = "Animalia",
  data = dplyr::as_tibble(data.frame(year = 2000:2010, cellCode = "CODE1", value = 1:11))
)
class(mock_indicator_ts) <- c("indicator_ts", "test_ts")

mock_indicator_map <- list(
  div_name = "Test Indicator Map",
  map_level = "country",
  map_region = "Test Country",
  projection = "EPSG:3857",
  coord_range = c(1, 2, 3, 4),
  cell_size = "10km",
  num_cells = 100,
  first_year = 2000,
  last_year = 2010,
  num_years = 11,
  num_species = 20,
  num_families = c(10, 11),
  kingdoms = c("Animalia", "Plantae"),
  map_layers = "countries",
  data = dplyr::as_tibble(data.frame(cellid = 1:11, cellCode = paste0("C", 1:11), diversity_val = 1:11, geometry = 1:11))
)
class(mock_indicator_map) <- c("indicator_map", "test_map")

mock_processed_cube <- list(
  first_year = 2000,
  last_year = 2010,
  resolutions = "10km",
  num_cells = 100,
  grid_type = "UTM",
  coord_range = c(1, 2, 3, 4),
  num_obs = 1000,
  num_species = 30,
  num_families = c(15, 16),
  kingdoms = c("Animalia", "Plantae", "Fungi"),
  data = dplyr::as_tibble(
    data.frame(cellid = 1:11, year = 2000:2010, value = 1:11)
    )
)
class(mock_processed_cube) <- "processed_cube"

mock_processed_cube_dsinfo <- list(
  first_year = 2000,
  last_year = 2010,
  resolutions = "10km",
  num_cells = 100,
  grid_type = "UTM",
  coord_range = c(1, 2, 3, 4),
  num_obs = 1000,
  num_species = 30,
  num_families = c(15, 16),
  kingdoms = c("Animalia", "Plantae", "Fungi"),
  num_datasets = 5,
  record_types = c("Occurrence", "Observation"),
  data = dplyr::as_tibble(
    data.frame(cellid = 1:11, year = 2000:2010, value = 1:11)
    )
)
class(mock_processed_cube_dsinfo) <- "processed_cube_dsinfo"

mock_sim_cube <- list(
  first_year = 2000,
  last_year = 2010,
  resolutions = "10km",
  num_cells = 100,
  grid_type = "UTM",
  coord_range = c(1, 2, 3, 4),
  num_obs = 1000,
  num_species = 30,
  num_families = c(15, 16),
  kingdoms = c("Animalia", "Plantae", "Fungi"),
  data = dplyr::as_tibble(data.frame(year = 2000:2010, value = 1:11))
)
class(mock_sim_cube) <- "sim_cube"

mock_available_indicators <- list(
  list(
    indicator_name = "Test Indicator 1",
    indicator_class = "Test Class 1",
    map_wrapper = "test_map_1",
    ts_wrapper = "test_ts_1",
    map_function_arguments = c("arg1", "arg2"),
    ts_function_arguments = c("arg3")
  ),
  list(
    indicator_name = "Test Indicator 2",
    indicator_class = "Test Class 2",
    map_wrapper = NULL,
    ts_wrapper = NULL,
    map_function_arguments = NULL,
    ts_function_arguments = NULL
  )
)
class(mock_available_indicators) <- "available_indicators"

# Tests
test_that("print.indicator_ts prints expected output", {
  output <- capture.output(print(mock_indicator_ts))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string, "Biodiversity indicator time series")
  expect_match(output_string, "Name of indicator: Test Indicator TS")
  expect_match(output_string, "Date Range: 2000 - 2010")
  expect_match(output_string, "Region(s) represented: Test Region", fixed = TRUE)
  expect_match(output_string, "Coordinate range represented:")
  expect_match(output_string, "1 2 3 4")
  expect_match(output_string, "Number of species represented: 10")
  expect_match(output_string, "Number of families represented: 5, 6")
  expect_match(output_string, "Kingdoms represented: Animalia")
  expect_match(output_string,
               "First 10 rows of data (use n = to show more):", fixed = TRUE)
  # Data check - row 1: 2000, 1
  expect_match(output_string, "1\\s+2000\\s+CODE1\\s+1")
  # Data check - row 10: 2009, 10
  expect_match(output_string, "10\\s+2009\\s+CODE1\\s+10")
})

test_that(
  "print.indicator_ts respects n parameter and displays correct final rows", {
  output_5 <- capture.output(print(mock_indicator_ts, n = 5))
  output_15 <- capture.output(print(mock_indicator_ts, n = 15))

  # Test n = 5
  expect_match(paste(output_5, collapse = "\n"), "5\\s+2004\\s+CODE1\\s+5")
  expect_match(paste(output_5, collapse = "\n"), "more rows")

  # Test n = 15
  expect_match(paste(output_15, collapse = "\n"), "11\\s+2010\\s+CODE1\\s+11")
  expect_no_match(paste(output_15, collapse = "\n"), "more rows")
})

test_that("print.indicator_map prints expected output", {
  output <- capture.output(print(mock_indicator_map))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string, "Gridded biodiversity indicator map")
  expect_match(output_string, "Name of Indicator: Test Indicator Map")
  expect_match(output_string, "Map of Test Country")
  expect_match(output_string, "Coordinate range:")
  expect_match(output_string, "1 2 3 4")
  expect_match(output_string, "Number of species represented: 20")
  expect_match(output_string, "Number of families represented: 10, 11")
  expect_match(output_string, "Kingdoms represented: Animalia, Plantae")
  expect_match(output_string,
               "First 10 rows of data (use n = to show more):", fixed = TRUE)
  # row 1: 1, 1
  expect_match(output_string, "1\\s+1\\s+C1\\s+1")
  # row 10: 10, 10
  expect_match(output_string, "10\\s+10\\s+C10\\s+10")
})

test_that(
  "print.indicator_map respects n parameter and displays correct final rows", {
  output_5 <- capture.output(print(mock_indicator_map, n = 5))
  output_15 <- capture.output(print(mock_indicator_map, n = 15))

  # Test n = 5
  expect_match(paste(output_5, collapse = "\n"), "5\\s+5\\s+C5\\s+5")
  expect_match(paste(output_5, collapse = "\n"), "more rows")

  # Test n = 15
  expect_match(paste(output_15, collapse = "\n"), "11\\s+11\\s+C11\\s+11")
  expect_no_match(paste(output_15, collapse = "\n"), "more rows")
})

test_that("print.indicator_map filters NAs by default", {
  mock_na <- mock_indicator_map
  mock_na$data$diversity_val[1:5] <- NA
  
  output <- capture.output(print(mock_na, n = 20))
  output_string <- paste(output, collapse = "\n")
  
  # Should NOT contain the rows with NA (cellid 1 to 5)
  # Regex: check for a line starting with 1 followed by NA
  expect_no_match(output_string, "\n\\s*1\\s+NA")
  expect_match(output_string, "1\\s+6\\s+C6\\s+6") # Row index 1, cellid 6, value 6
})

test_that("print.indicator_map includes NAs when include_na = TRUE", {
  mock_na <- mock_indicator_map
  mock_na$data$diversity_val[1:5] <- NA
  
  output <- capture.output(print(mock_na, n = 20, include_na = TRUE))
  output_string <- paste(output, collapse = "\n")
  
  # Should contain the rows with NA
  expect_match(output_string, "1\\s+1\\s+C1\\s+NA")
  expect_match(output_string, "6\\s+6\\s+C6\\s+6")
})

# mock_processed_cube tests
test_that("print.processed_cube prints expected output", {
  output <- capture.output(print(mock_processed_cube))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string,
               "Processed data cube for calculating biodiversity indicators")
  expect_match(output_string, "Date Range: 2000 - 2010")
  expect_match(output_string, "Single-resolution cube with cell size 10km \\^2")
  expect_match(output_string, "Number of cells: 100")
  expect_match(output_string, "Grid reference system: UTM")
  expect_match(output_string, "Coordinate range:")
  expect_match(output_string, "1 2 3 4")
  expect_match(output_string, "Total number of observations: 1000")
  expect_match(output_string, "Number of species represented: 30")
  expect_match(output_string, "Number of families represented: 15, 16")
  expect_match(output_string, "Kingdoms represented: Animalia, Plantae, Fungi")
  expect_match(output_string,
               "First 10 rows of data (use n = to show more):", fixed = TRUE)
  expect_match(output_string, "1\\s+1\\s+2000\\s+1")
  expect_match(output_string, "10\\s+10\\s+2009\\s+10")
})

test_that(
  "print.processed_cube respects n parameter and displays correct final rows", {
  output_5 <- capture.output(print(mock_processed_cube, n = 5))
  output_15 <- capture.output(print(mock_processed_cube, n = 15))

  # Test n = 5
  expect_match(paste(output_5, collapse = "\n"), "5\\s+5\\s+2004\\s+5")
  expect_match(paste(output_5, collapse = "\n"), "more rows")

  # Test n = 15
  expect_match(paste(output_15, collapse = "\n"), "11\\s+11\\s+2010\\s+11")
  expect_no_match(paste(output_15, collapse = "\n"), "more rows")
})

# mock_processed_cube_dsinfo tests
test_that("print.processed_cube_dsinfo prints expected output", {
  output <- capture.output(print(mock_processed_cube_dsinfo))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string,
               "Processed data cube for calculating biodiversity indicators.")
  expect_match(output_string, "Date Range: 2000 - 2010")
  expect_match(output_string, "Single-resolution cube with cell size 10km\\^2")
  expect_match(output_string, "Number of cells: 100")
  expect_match(output_string, "Grid reference system: UTM")
  expect_match(output_string, "Coordinate range:")
  expect_match(output_string, "1 2 3 4")
  expect_match(output_string, "Total number of observations: 1000")
  expect_match(output_string, "Number of species represented: 30")
  expect_match(output_string, "Number of families represented: 15, 16")
  expect_match(output_string, "Kingdoms represented: Animalia, Plantae, Fungi")
  expect_match(output_string, "Number of datasets represented: 5")
  expect_match(output_string,
               "Record types represented: Occurrence, Observation")
  expect_match(output_string,
               "First 10 rows of data (use n = to show more):", fixed = TRUE)
  expect_match(output_string, "1\\s+1\\s+2000\\s+1")
  expect_match(output_string, "10\\s+10\\s+2009\\s+10")
})

test_that(
  paste0(
    "print.processed_cube_dsinfo respects n parameter and displays correct ",
    "final rows"
  ), {
  output_5 <- capture.output(print(mock_processed_cube_dsinfo, n = 5))
  output_15 <- capture.output(print(mock_processed_cube_dsinfo, n = 15))

  # Test n = 5
  expect_match(paste(output_5, collapse = "\n"), "5\\s+5\\s+2004\\s+5")
  expect_match(paste(output_5, collapse = "\n"), "more rows")

  # Test n = 15
  expect_match(paste(output_15, collapse = "\n"), "11\\s+11\\s+2010\\s+11")
  expect_no_match(paste(output_15, collapse = "\n"), "more rows")
})

# mock_sim_cube tests
test_that("print.sim_cube prints expected output", {
  output <- capture.output(print(mock_sim_cube))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string,
               "Simulated data cube for calculating biodiversity indicators")
  expect_match(output_string, "Date Range: 2000 - 2010")
  expect_match(output_string, "Single-resolution cube with cell size 10km \\^2")
  expect_match(output_string, "Number of cells: 100")
  expect_match(output_string, "Grid reference system: UTM")
  expect_match(output_string, "Coordinate range:")
  expect_match(output_string, "1 2 3 4")
  expect_match(output_string, "Total number of observations: 1000")
  expect_match(output_string, "Number of species represented: 30")
  expect_match(output_string, "Number of families represented: 15, 16")
  expect_match(output_string, "Kingdoms represented: Animalia, Plantae, Fungi")
  expect_match(output_string,
               "First 10 rows of data (use n = to show more):", fixed = TRUE)
  expect_match(output_string, "1\\s+2000\\s+1")
  expect_match(output_string, "10\\s+2009\\s+10")
})

test_that(
  "print.sim_cube respects n parameter and displays correct final rows", {
  output_5 <- capture.output(print(mock_sim_cube, n = 5))
  output_15 <- capture.output(print(mock_sim_cube, n = 15))

  # Test n = 5
  expect_match(paste(output_5, collapse = "\n"), "5\\s+2004\\s+5")
  expect_match(paste(output_5, collapse = "\n"), "more rows")

  # Test n = 15
  expect_match(paste(output_15, collapse = "\n"), "11\\s+2010\\s+11")
  expect_no_match(paste(output_15, collapse = "\n"), "more rows")
})

test_that("print.available_indicators prints expected values", {
  output <- capture.output(print(mock_available_indicators))
  output_string <- paste(output, collapse = "\n")

  expect_match(output_string, "Test Indicator 1")
  expect_match(output_string, "Class: Test Class 1")
  expect_match(output_string,
               "Calculate map: yes, e.g. test_map_1(my_data_cube)", fixed = TRUE)
  expect_match(output_string,
               "Calculate time series: yes, e.g. test_ts_1(my_data_cube)", fixed = TRUE)
  expect_match(output_string, "Additional map function arguments: arg1, arg2")
  expect_match(output_string, "Additional time series function arguments: arg3")

  expect_match(output_string, "Test Indicator 2")
  expect_match(output_string, "Class: Test Class 2")
  expect_match(output_string, "Calculate map: no")
  expect_match(output_string, "Calculate time series: no")
  expect_match(output_string, "Additional map function arguments: none")
  expect_match(output_string, "Additional time series function arguments: none")
})
