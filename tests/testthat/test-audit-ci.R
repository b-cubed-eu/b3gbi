# add_ci(): ci_type = "none", confidence_level, seed and overwrite message
# at the indicator level

make_occ_ts <- function() {
  total_occ_ts(example_cube_1, first_year = 2015)
}

test_that("ci_type = 'none' returns the indicator unchanged", {
  ind <- make_occ_ts()
  expect_message(
    res <- add_ci(ind, ci_type = "none", bootstrap_level = "indicator"),
    "without confidence intervals"
  )
  expect_identical(res, ind)
  expect_false(any(c("ll", "ul") %in% names(res$data)))
})

test_that("ci_type = 'none' also returns early at cube level", {
  ind <- make_occ_ts()
  expect_message(
    res <- add_ci(ind, ci_type = "none", bootstrap_level = "cube"),
    "without confidence intervals"
  )
  expect_identical(res, ind)
})

test_that("indicator level uses confidence_level", {
  skip_on_cran()
  ind <- make_occ_ts()
  # Few replicates: silence boot's "extreme order statistics" warnings
  ci95 <- suppressWarnings(
    add_ci(ind, num_bootstrap = 50, bootstrap_level = "indicator",
           confidence_level = 0.95)
  )
  ci80 <- suppressWarnings(
    add_ci(ind, num_bootstrap = 50, bootstrap_level = "indicator",
           confidence_level = 0.8)
  )
  expect_true(all(ci95$data$conf == 0.95))
  expect_true(all(ci80$data$conf == 0.8))
  width95 <- ci95$data$ul - ci95$data$ll
  width80 <- ci80$data$ul - ci80$data$ll
  expect_true(all(width80 <= width95, na.rm = TRUE))
  expect_true(any(width80 < width95, na.rm = TRUE))
  expect_equal(unique(ci95$data$int_type), "perc")
})

test_that("indicator level is reproducible with seed and keeps RNG state", {
  skip_on_cran()
  ind <- make_occ_ts()
  set.seed(1)
  expected_next <- stats::runif(1)
  set.seed(1)
  a <- suppressWarnings(
    add_ci(ind, num_bootstrap = 30, bootstrap_level = "indicator", seed = 42)
  )
  # The user's random number stream is not affected
  expect_equal(stats::runif(1), expected_next)
  b <- suppressWarnings(
    add_ci(ind, num_bootstrap = 30, bootstrap_level = "indicator", seed = 42)
  )
  expect_identical(a$data$ll, b$data$ll)
  expect_identical(a$data$ul, b$data$ul)
  c <- suppressWarnings(
    add_ci(ind, num_bootstrap = 30, bootstrap_level = "indicator", seed = 7)
  )
  expect_false(identical(a$data$ll, c$data$ll))
})

test_that("overwrite = FALSE warning refers to the overwrite argument", {
  skip_on_cran()
  ind <- make_occ_ts()
  ind_ci <- suppressWarnings(
    add_ci(ind, num_bootstrap = 20, bootstrap_level = "indicator")
  )
  expect_warning(
    res <- add_ci(ind_ci, num_bootstrap = 20, bootstrap_level = "indicator",
                  overwrite = FALSE),
    "overwrite = TRUE"
  )
  expect_identical(res, ind_ci)
})
