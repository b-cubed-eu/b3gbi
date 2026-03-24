library(b3gbi)
library(testthat)

test_that("completeness_ts with gridded_average works", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  mock_DataInfo <- function(x, ...) {
    reps <- length(x)
    tibble::tibble(
      Assemblage = if(!is.null(names(x))) names(x) else seq_len(reps),
      SC = rep(0.8, reps),
      n = rep(10, reps),
      S.obs = rep(5, reps)
    )
  }

  # This replaces the binding for the duration of this test_that block
  local_mocked_bindings(my_DataInfo = mock_DataInfo, .package = "b3gbi")

  # 1. Regular completeness
  res_regular <- completeness_ts(example_cube_1, cutoff_length = 0)

  # 2. Gridded average completeness
  res_gridded <- completeness_ts(example_cube_1,
                                 gridded_average = TRUE,
                                 cell_size = 5,
                                 cutoff_length = 0)

  expect_s3_class(res_regular, "indicator_ts")
  expect_s3_class(res_gridded, "indicator_ts")

  # Both should have diversity_val
  expect_true("diversity_val" %in% names(res_regular$data))
  expect_true("diversity_val" %in% names(res_gridded$data))

  # Years should match
  expect_equal(res_regular$data$year, res_gridded$data$year)

  # Average should be around 0.8
  expect_equal(mean(res_gridded$data$diversity_val, na.rm = TRUE), 0.8, tolerance = 0.01)
})
