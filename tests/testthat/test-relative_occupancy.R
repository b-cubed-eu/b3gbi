# Tests for relative_occupancy functions

library(b3gbi)
library(testthat)

# ---------------------------------------------------------------------------
# Map tests
# ---------------------------------------------------------------------------

test_that("relative_occupancy_map occ_type 0 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_map(example_cube_1, ci_type = "none",
                                   occ_type = 0)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1))
})

test_that("relative_occupancy_map occ_type 1 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_map(example_cube_1, ci_type = "none",
                                   occ_type = 1)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1))
})

test_that("relative_occupancy_map occ_type 2 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_map(example_cube_1, ci_type = "none",
                                   occ_type = 2)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1,
                  na.rm = TRUE))
})

test_that("map occ_type 1 values >= occ_type 0 values (smaller denominator)", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  r0 <- relative_occupancy_map(example_cube_1, ci_type = "none", occ_type = 0)
  r1 <- relative_occupancy_map(example_cube_1, ci_type = "none", occ_type = 1)

  # Type 1 denominator (ever-occupied cells) <= Type 0 denominator (all cells),
  # so Type 1 values should be >= Type 0 values.
  # Merge on species to compare properly (drop sf geometry first)
  d0 <- sf::st_drop_geometry(r0$data)[, c("scientificName", "diversity_val")]
  d1 <- sf::st_drop_geometry(r1$data)[, c("scientificName", "diversity_val")]
  merged <- merge(d0, d1, by = "scientificName", suffixes = c("_t0", "_t1"))
  merged <- merged[!duplicated(merged$scientificName), ]
  expect_true(all(merged$diversity_val_t1 >= merged$diversity_val_t0 - 1e-9))
})

test_that("relative_occupancy_map rejects invalid occ_type", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  expect_error(
    relative_occupancy_map(example_cube_1, ci_type = "none", occ_type = 3),
    "`occ_type` must be 0, 1, or 2"
  )
})

# ---------------------------------------------------------------------------
# Time series tests
# ---------------------------------------------------------------------------

test_that("relative_occupancy_ts occ_type 0 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_ts(example_cube_1, ci_type = "none",
                                  occ_type = 0)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("year" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1,
                  na.rm = TRUE))
})

test_that("relative_occupancy_ts occ_type 1 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_ts(example_cube_1, ci_type = "none",
                                  occ_type = 1)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("year" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1,
                  na.rm = TRUE))
})

test_that("relative_occupancy_ts occ_type 2 calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- relative_occupancy_ts(example_cube_1, ci_type = "none",
                                  occ_type = 2)

  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("year" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 &
                    result$data$diversity_val <= 1,
                  na.rm = TRUE))
})

test_that("ts occ_type 1 values >= occ_type 0 for same year/species", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  r0 <- relative_occupancy_ts(example_cube_1, ci_type = "none", occ_type = 0)
  r1 <- relative_occupancy_ts(example_cube_1, ci_type = "none", occ_type = 1)

  merged <- merge(
    r0$data[, c("year", "scientificName", "diversity_val")],
    r1$data[, c("year", "scientificName", "diversity_val")],
    by = c("year", "scientificName"), suffixes = c("_t0", "_t1")
  )
  expect_true(all(merged$diversity_val_t1 >= merged$diversity_val_t0 - 1e-9))
})

test_that("ts occ_type 0 matches previous default behaviour", {
  # Type 0 should give identical results to the old un-typed default since
  # it uses the same total_num_cells denominator.
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  r_new  <- relative_occupancy_ts(example_cube_1, ci_type = "none", occ_type = 0)
  # Default should also be occ_type 0
  r_default <- relative_occupancy_ts(example_cube_1, ci_type = "none")

  expect_equal(r_new$data$diversity_val, r_default$data$diversity_val)
})

test_that("relative_occupancy_ts rejects invalid occ_type", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  expect_error(
    relative_occupancy_ts(example_cube_1, ci_type = "none", occ_type = 5),
    "`occ_type` must be 0, 1, or 2"
  )
})
