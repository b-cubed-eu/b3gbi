test_that("check_cell_size handles valid inputs", {
  expect_equal(check_cell_size(10, "1km", "country"), 10000)
  expect_equal(check_cell_size(20, "10km", "world"), 20000)
  expect_equal(check_cell_size(0.5, "0.25degrees", "country"), 0.5)
  expect_equal(check_cell_size(1, "0.5degrees", "world"), 1)
})

test_that("check_cell_size handles invalid cell sizes", {
  expect_error(check_cell_size(11, "10km", "country"),
               "cell_size must be a whole number multiple of the resolution.")
  expect_error(check_cell_size(0.3, "0.25degrees", "country"),
               "cell_size must be a whole number multiple of the resolution.")
})

test_that("check_cell_size handles NULL cell size (km)", {
  expect_equal(check_cell_size(NULL, "1km", "world"), 100000)
  expect_equal(check_cell_size(NULL, "1km", "continent"), 100000)
  expect_equal(check_cell_size(NULL, "1km", "country"), 10000)
})

test_that("check_cell_size handles NULL cell size (degrees)", {
  expect_equal(check_cell_size(NULL, "1degrees", "world"), 1)
  expect_equal(check_cell_size(NULL, "0.25degrees", "country"), 0.25)
  expect_equal(check_cell_size(NULL, "0.5degrees", "continent"), 1)
})

test_that("check_cell_size handles invalid resolution units", {
  expect_error(check_cell_size(10, "1miles", "country"),
               "Resolution units not recognized.")
})

test_that("check_cell_size handles edge cases", {
  expect_equal(check_cell_size(0.5, "0.25degrees", "country"), 0.5)
  expect_equal(check_cell_size(NULL, "0.1degrees", "continent"), 1)
  expect_equal(check_cell_size(NULL, "1km", "region"), 10000)
})
