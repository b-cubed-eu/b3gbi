# The shapefile defines the study area: data are filtered to it, and density
# time series divide by its area (within the cube's extent).

make_test_polygon <- function(crs = 4326) {
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(9.5, 56.9), c(10.5, 56.9), c(10.5, 57.5), c(9.5, 57.5), c(9.5, 56.9)
  ))), crs = 4326))
  sf::st_transform(poly, crs)
}

poly_area_km2 <- function(poly) {
  as.numeric(units::set_units(
    sf::st_area(sf::st_transform(poly, "ESRI:54012")), "km^2"
  ))
}

run_density_ts <- function(...) {
  suppressWarnings(suppressMessages(
    spec_richness_density_ts(example_cube_1, first_year = 2015, ...)
  ))
}

test_that("shapefile restricts data and area of density time series", {
  skip_on_cran()
  poly <- make_test_polygon()
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(poly, f, quiet = TRUE)

  full <- run_density_ts()
  inside <- run_density_ts(shapefile_path = f)
  outside <- run_density_ts(shapefile_path = f, invert = TRUE)

  # Area is the polygon's area, not the cube's extent
  expect_equal(attr(inside$raw_data, "total_area_sqkm"), poly_area_km2(poly),
               tolerance = 0.01)
  expect_lt(attr(inside$raw_data, "total_area_sqkm"),
            attr(full$raw_data, "total_area_sqkm") / 10)

  # Data are filtered to the polygon, and inside + outside = everything
  expect_lt(nrow(inside$raw_data), nrow(full$raw_data))
  expect_equal(nrow(inside$raw_data) + nrow(outside$raw_data),
               nrow(full$raw_data))
})

test_that("shapefile already in the working projection is accepted", {
  skip_on_cran()
  poly <- make_test_polygon(crs = "ESRI:54012")
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(poly, f, quiet = TRUE)

  res <- run_density_ts(shapefile_path = f)
  expect_s3_class(res, "indicator_ts")
  expect_equal(attr(res$raw_data, "total_area_sqkm"), poly_area_km2(poly),
               tolerance = 0.01)
})

test_that("density time series warn when no study area is given", {
  skip_on_cran()
  expect_warning(
    suppressMessages(spec_richness_density_ts(example_cube_1, first_year = 2015)),
    "No study area was given"
  )
  expect_warning(
    suppressMessages(occ_density_ts(example_cube_1, first_year = 2015)),
    "No study area was given"
  )

  poly <- make_test_polygon()
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(poly, f, quiet = TRUE)
  expect_no_warning(
    suppressMessages(spec_richness_density_ts(example_cube_1, first_year = 2015,
                                              shapefile_path = f)),
    message = "No study area was given"
  )
})
