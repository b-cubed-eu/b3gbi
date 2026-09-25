test_that("create_sf_from_utm uses a two-digit zone in the EPSG code", {
  # Zone 5 (Hawaii): EPSG:32605, not EPSG:3265
  df <- data.frame(
    xcoord = 500000,
    ycoord = 2200000,
    utmzone = 5,
    hemisphere = "N"
  )
  res <- create_sf_from_utm(df, output_crs = 4326)
  xy <- sf::st_coordinates(res)
  expect_equal(unname(xy[1, "X"]), -153, tolerance = 0.01)
  expect_equal(unname(xy[1, "Y"]), 19.9, tolerance = 0.01)

  # Without output_crs the CRS is the zone's own
  res_utm <- create_sf_from_utm(df)
  expect_equal(sf::st_crs(res_utm)$epsg, 32605L)

  # Southern hemisphere, single-digit zone
  df_s <- data.frame(xcoord = 500000, ycoord = 7800000, utmzone = 5,
                     hemisphere = "S")
  expect_equal(sf::st_crs(create_sf_from_utm(df_s))$epsg, 32705L)
})

test_that("create_sf_from_utm keeps behaviour for two-digit zones", {
  df <- data.frame(
    xcoord = c(500000, 501000),
    ycoord = c(5600000, 5601000),
    utmzone = c(32, 32),
    hemisphere = c("North", "North")
  )
  expect_equal(sf::st_crs(create_sf_from_utm(df))$epsg, 32632L)
  res <- create_sf_from_utm(df, output_crs = "EPSG:4326")
  xy <- sf::st_coordinates(res)
  # Central meridian of zone 32 is 9 degrees E
  expect_equal(unname(xy[1, "X"]), 9, tolerance = 1e-6)
  expect_true(all(xy[, "Y"] > 50 & xy[, "Y"] < 51))
})

test_that("process_cube reports the number of removed rows as positive", {
  cube_file <- system.file("extdata", "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  skip_if(cube_file == "")
  cube_df <- readr::read_delim(cube_file, show_col_types = FALSE,
                               col_types = readr::cols(.default = "c"))
  cube_df <- cube_df[1:50, ]
  cube_df$eqdgccellcode[2:4] <- NA
  expect_message(
    process_cube(cube_df, grid_type = "eqdgc",
                 cols_cellCode = "eqdgccellcode"),
    "Removed 3 rows with missing cell codes"
  )
})

test_that("process_cube error message names force_gridcode correctly", {
  df <- data.frame(year = c(2000, 2001), cellCode = c("abc", "def"),
                   occurrences = c(1, 2), scientificName = c("a", "b"),
                   speciesKey = c("1", "2"))
  expect_error(process_cube(df, grid_type = "eea", cols_cellCode = "cellCode"),
               "force_gridcode = TRUE")
})
