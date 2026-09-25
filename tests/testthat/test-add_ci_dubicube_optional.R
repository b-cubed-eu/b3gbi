# add_ci(): bootstrap_level = "auto" and dubicube as an optional dependency

make_mock_ts <- function(div_type = "total_occ") {
  x <- list(
    div_name = "Mock indicator",
    div_type = div_type,
    first_year = 2000, last_year = 2001,
    map_region = "unknown", coord_range = c(xmin = 0, xmax = 1),
    num_species = 2,
    data = tibble::tibble(year = c(2000, 2001), diversity_val = c(2, 3)),
    raw_data = data.frame(year = c(2000, 2000, 2001, 2001),
                          taxonKey = c(1, 2, 1, 2),
                          scientificName = c("Sp A", "Sp B", "Sp A", "Sp B"),
                          obs = c(1, 1, 2, 1),
                          cellCode = c("C1", "C2", "C1", "C2"))
  )
  class(x) <- c("indicator_ts", div_type)
  attr(x, "type") <- "ts"
  x
}

fake_calc_ci <- function(x, indicator, ...) {
  indicator$ll <- indicator$diversity_val - 1
  indicator$ul <- indicator$diversity_val + 1
  indicator
}

test_that("auto falls back to indicator level when dubicube is missing", {
  withr::local_options(rlib_message_verbosity = "verbose")
  local_mocked_bindings(
    is_package_installed = function(package) package != "dubicube",
    calc_ci = fake_calc_ci
  )
  expect_message(res <- add_ci(make_mock_ts()), "dubicube")
  expect_equal(res$ci_method, "indicator")
  expect_equal(res$data$ll, c(1, 2))
})

test_that("explicit cube level without dubicube gives install instructions", {
  local_mocked_bindings(
    is_package_installed = function(package) package != "dubicube"
  )
  expect_error(add_ci(make_mock_ts(), bootstrap_level = "cube"),
               "requires the 'dubicube' package")
  expect_error(add_ci(make_mock_ts(), bootstrap_level = "cube"),
               "b-cubed-eu.r-universe.dev")
})

test_that("explicit indicator level never needs dubicube", {
  local_mocked_bindings(
    is_package_installed = function(package) package != "dubicube",
    calc_ci = fake_calc_ci
  )
  expect_no_message(res <- add_ci(make_mock_ts(), bootstrap_level = "indicator"))
  expect_equal(res$ci_method, "indicator")
})

test_that("auto uses cube level when dubicube is installed", {
  skip_if_not_installed("dubicube")
  local_mocked_bindings(
    is_package_installed = function(package) TRUE
  )
  testthat::with_mocked_bindings(
    bootstrap_cube = function(...) data.frame(year = 2000, sample = 1),
    calculate_bootstrap_ci = function(...) {
      data.frame(year = c(2000, 2001), ll = c(1, 2), ul = c(3, 4),
                 int_type = "perc", conf = 0.95, est_original = c(2, 3))
    },
    .package = "dubicube",
    {
      res <- add_ci(make_mock_ts())
      expect_equal(res$ci_method, "cube")
      expect_equal(res$data$ul, c(3, 4))
    }
  )
})

test_that("Hill numbers use the indicator level silently under auto", {
  local_mocked_bindings(calc_ci = fake_calc_ci)
  expect_no_warning(res <- add_ci(make_mock_ts("hill0")))
  expect_equal(res$ci_method, "indicator")
})

test_that("print shows how confidence intervals were calculated", {
  x <- make_mock_ts()
  x$ci_method <- "cube"
  expect_output(print(x), "cube-level bootstrapping \\(dubicube\\)")
  x$ci_method <- "indicator"
  expect_output(print(x), "indicator-level bootstrapping")
})
