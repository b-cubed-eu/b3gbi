test_that("detect_grid_column detects valid EEA grid column", {
  df <- data.frame(
    id = 1:3,
    grid_cell = c("10kmE1234567N1234567",
                  "50kmW9876543S9876543",
                  "100kmE1111111N2222222"),
    value = c(10, 20, 30)
  )
  expect_equal(detect_grid_column(df, "eea"), "grid_cell")
})

test_that("detect_grid_column detects valid MGRS grid column", {
  df <- data.frame(
    id = 1:3,
    mgrs_cell = c("32TMT1234567890",
                  "33TUN9876543210",
                  "34TVP1111111111"),
    value = c(10, 20, 30)
  )
  expect_equal(detect_grid_column(df, "mgrs"), "mgrs_cell")
})

test_that("detect_grid_column detects valid EQDGC grid column", {
  df <- data.frame(
    id = 1:3,
    eqdgc_cell = c("E123N45A",
                   "W987S65B",
                   "E111N22C"),
    value = c(10, 20, 30)
  )
  expect_equal(detect_grid_column(df, "eqdgc"), "eqdgc_cell")
})

test_that("detect_grid_column throws error for no matching column", {
  df <- data.frame(
    id = 1:3,
    value = c(10, 20, 30)
  )
  expect_error(detect_grid_column(df, "eea"),
               "Could not detect specified grid type.")
})

test_that("detect_grid_column handles mixed data types", {
  df <- data.frame(
    id = 1:3,
    grid_cell = c("10kmE1234567N1234567", 2, 3),
    value = c(10, 20, 30)
  )
  expect_error(
    detect_grid_column(df, "eea"),
    "contains a mix of valid and invalid grid cell codes.")
})

test_that("detect_grid_column handles NA values", {
  df <- data.frame(
    id = 1:3,
    grid_cell = c(NA,
                  "10kmE1234567N1234567",
                  "50kmW9876543S9876543"),
    value = c(10, 20, 30)
  )
  expect_equal(detect_grid_column(df, "eea"), "grid_cell")
})

test_that("detect_grid_column throws error for invalid grid type", {
  df <- data.frame(
    id = 1:3,
    grid_cell = c("10kmE1234567N1234567",
                  "50kmW9876543S9876543",
                  "100kmE1111111N2222222"),
    value = c(10, 20, 30)
  )
  expect_error(detect_grid_column(df, "invalid"))
})
