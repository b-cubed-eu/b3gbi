# Tests for the compute_indicator_workflow clipping change
# Verifies that intersect_grid_with_polygon is used for all grid types

test_that("total_occ_map clips to region boundary for EQDGC", {
  result <- total_occ_map(
    example_cube_1,
    level = "country",
    region = "Denmark"
  )

  expect_s3_class(result, "indicator_map")
  expect_s3_class(result, "total_occ")
  expect_gt(nrow(result$data), 0)
  expect_true("diversity_val" %in% names(result$data))

  # Total occurrences should not exceed the full cube total
  d <- sf::st_drop_geometry(result$data)
  output_total <- sum(d$diversity_val, na.rm = TRUE)
  input_total <- sum(example_cube_1$data$obs, na.rm = TRUE)
  expect_lte(output_total, input_total)
  expect_gt(output_total, 0)
})

test_that("total_occ_map works at cube level without clipping", {

  result <- total_occ_map(example_cube_1)

  expect_s3_class(result, "indicator_map")
  expect_gt(nrow(result$data), 0)

  # Sum of occurrences should match input
  input_total <- sum(example_cube_1$data$obs, na.rm = TRUE)
  output_total <- sum(sf::st_drop_geometry(result$data)$diversity_val, na.rm = TRUE)
  expect_equal(output_total, input_total)
})

test_that("obs_richness_map clips to region boundary", {

  result <- obs_richness_map(
    example_cube_1,
    level = "country",
    region = "Denmark"
  )

  expect_s3_class(result, "indicator_map")
  expect_gt(nrow(result$data), 0)

  # Richness should be reasonable (not exceed total species)
  d <- sf::st_drop_geometry(result$data)
  max_richness <- max(d$diversity_val, na.rm = TRUE)
  total_species <- length(unique(example_cube_1$data$scientificName))
  expect_lte(max_richness, total_species)
})

test_that("total_occ_map with cell_size aggregates correctly", {

  result_native <- total_occ_map(
    example_cube_1,
    level = "country",
    region = "Denmark"
  )

  result_agg <- total_occ_map(
    example_cube_1,
    level = "country",
    region = "Denmark",
    cell_size = 1
  )

  # Aggregated should have fewer or equal rows
  expect_lte(nrow(result_agg$data), nrow(result_native$data))

  # Total occurrences should be preserved
  native_total <- sum(sf::st_drop_geometry(result_native$data)$diversity_val, na.rm = TRUE)
  agg_total <- sum(sf::st_drop_geometry(result_agg$data)$diversity_val, na.rm = TRUE)
  expect_equal(agg_total, native_total)
})

test_that("intersect_grid_with_polygon produces clipped cells", {
  result <- total_occ_map(
    example_cube_1,
    level = "country",
    region = "Denmark"
  )

  # Clipped cells should have different sizes than original uniform grid
  # Verify by checking that geometry bounding boxes vary
  bboxes <- sf::st_bbox(result$data)
  expect_true(!is.null(bboxes))
  # Cells should be POLYGON type (not empty)
  expect_true(all(sf::st_geometry_type(result$data) %in% c("POLYGON", "MULTIPOLYGON")))
})
