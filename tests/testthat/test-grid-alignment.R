test_that("grid alignment for MGRS across multiple UTM zones works", {
  # Create a mock cube spanning two UTM zones (e.g., 31U and 32U)
  mock_data <- data.frame(
    year = c(2020, 2021),
    cellCode = c("31UDS65", "32ULB65"), # One in 31U, one in 32U
    occurrences = c(1, 1),
    scientificName = c("Spec A", "Spec B"),
    speciesKey = c(1, 2)
  )

  # Process cube
  cube <- process_cube(mock_data, grid_type = "mgrs")

  # Calculate richness map (should trigger Mollweide and native grid)
  # We force a large area to ensure Mollweide is chosen if multi-zone
  res_map <- obs_richness_map(cube)

  # Check results
  expect_s3_class(res_map$data, "sf")
  expect_equal(nrow(res_map$data), 2)
  expect_true(all(!is.na(res_map$data$diversity_val)))
})

test_that("grid alignment for EEA works", {
  # EEA code example
  mock_data <- data.frame(
    year = c(2020, 2021),
    cellCode = c("1kmE4321N3210"),
    occurrences = 1,
    scientificName = "Spec A",
    speciesKey = 1
  )
  cube <- process_cube(mock_data, grid_type = "eea")
  res_map <- obs_richness_map(cube)
  expect_equal(nrow(res_map$data), 1)
  expect_false(is.na(res_map$data$diversity_val[1]))
})

test_that("grid alignment for EQDGC works", {
  # EQDGC code example
  mock_data <- data.frame(
    year = c(2020, 2021),
    cellCode = c("E144S36"),
    occurrences = 1,
    scientificName = "Spec A",
    speciesKey = 1
  )
  cube <- process_cube(mock_data, grid_type = "eqdgc")
  res_map <- obs_richness_map(cube)
  expect_equal(nrow(res_map$data), 1)
  expect_false(is.na(res_map$data$diversity_val[1]))
})

test_that("spec_richness_density_map handles native grids and area", {
  mock_data <- data.frame(
    year = c(2020, 2021),
    cellCode = c("31UDS65", "31UDS65"),
    speciesKey = c(1, 2),
    occurrences = c(1, 1),
    scientificName = c("A", "B")
  )
  cube <- suppressMessages(process_cube(mock_data, grid_type = "mgrs"))
  res_map <- suppressMessages(spec_richness_density_map(cube))
  expect_s3_class(res_map$data, "sf")
  expect_true("diversity_val" %in% names(res_map$data))
  expect_true(all(!is.na(res_map$data$diversity_val)))
})

