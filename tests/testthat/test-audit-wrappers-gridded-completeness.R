test_that("completeness_ts(gridded_average = TRUE) averages over coarser cells", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  res_whole <- completeness_ts(example_cube_1, first_year = 2000)
  expect_message(
    res_grid <- completeness_ts(example_cube_1,
                                first_year = 2000,
                                gridded_average = TRUE,
                                cutoff_length = 0),
    "Forcing coarser grid resolution"
  )

  expect_s3_class(res_grid, "indicator_ts")
  expect_equal(res_whole$data$year, res_grid$data$year)
  expect_true(any(!is.na(res_grid$data$diversity_val)))
  expect_true(all(res_grid$data$diversity_val >= 0 &
                    res_grid$data$diversity_val <= 1, na.rm = TRUE))
  # Averaging completeness over grid cells is not the same as computing it
  # once for the whole area
  expect_false(isTRUE(all.equal(res_whole$data$diversity_val,
                                res_grid$data$diversity_val)))
})

test_that("gridded_average reaches the completeness core", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  seen <- NULL
  mock_core <- function(x, ...) {
    seen <<- list(...)$gridded_average
    tibble::tibble(year = sort(unique(x$year)), diversity_val = 0.5)
  }
  local_mocked_bindings(calc_ts_completeness_core = mock_core, .package = "b3gbi")

  suppressMessages(completeness_ts(example_cube_1, first_year = 2000,
                                   gridded_average = TRUE))
  expect_true(isTRUE(seen))

  suppressMessages(completeness_ts(example_cube_1, first_year = 2000))
  expect_false(isTRUE(seen))
})
