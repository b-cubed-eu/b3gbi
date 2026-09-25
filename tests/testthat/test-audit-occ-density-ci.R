test_that("add_ci works for occurrence density (total area kept for bootstrapping)", {
  skip_on_cran()
  x <- occ_density_ts(example_cube_1, first_year = 2015)
  expect_false(is.null(attr(x$raw_data, "total_area_sqkm")))
  res <- suppressWarnings(
    add_ci(x, num_bootstrap = 20, bootstrap_level = "indicator", seed = 1)
  )
  expect_true(all(c("ll", "ul") %in% names(res$data)))
  expect_true(any(is.finite(res$data$ll)))
})
