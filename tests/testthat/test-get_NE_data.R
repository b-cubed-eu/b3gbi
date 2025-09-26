test_that("get_ne_data retrieves map data correctly", {
  # Test country level
  france_map <- get_ne_data(latlong_bbox = NULL,
                            projected_crs = "EPSG:3857",
                            region = "France",
                            level = "country",
                            ne_type = "countries",
                            ne_scale = "medium")
  expect_s3_class(france_map[[1]], "sf")
  expect_equal(sf::st_crs(france_map[[1]]), sf::st_crs("EPSG:3857"))

  # Test continent level
  africa_map <- get_ne_data(latlong_bbox = NULL,
                            projected_crs = "EPSG:3857",
                            region = "Africa",
                            level = "continent",
                            ne_type = "countries",
                            ne_scale = "medium")
  expect_s3_class(africa_map[[2]], "sf")
  expect_equal(sf::st_crs(africa_map[[2]]), sf::st_crs("EPSG:3857"))

  # Test world level
  world_map <- get_ne_data(latlong_bbox = NULL,
                           projected_crs = "EPSG:3857",
                           region = NULL,
                           level = "world",
                           ne_type = "countries",
                           ne_scale = "medium")
  expect_s3_class(world_map[[1]], "sf")
  expect_equal(sf::st_crs(world_map[[1]]), sf::st_crs("EPSG:3857"))

  # Test sovereignty level
  sovereignty_map <- get_ne_data(latlong_bbox = NULL,
                                 projected_crs = "EPSG:3857",
                                 region = "United States of America",
                                 level = "sovereignty",
                                 ne_type = "sovereignty",
                                 ne_scale = "medium")
  expect_s3_class(sovereignty_map[[2]], "sf")

  # Test geounit level
  geounit_map <- get_ne_data(latlong_bbox = NULL,
                             projected_crs = "EPSG:3857",
                             region = "Germany",
                             level = "geounit",
                             ne_type = "map_units",
                             ne_scale = "medium")
  expect_s3_class(geounit_map[[1]], "sf")

  # Test tiny_countries
  tiny_countries_map <- get_ne_data(latlong_bbox = NULL,
                                    projected_crs = "EPSG:3857",
                                    region = "France",
                                    level = "country",
                                    ne_type = "tiny_countries",
                                    ne_scale = "medium")
  expect_s3_class(tiny_countries_map[[2]], "sf")

  # test multiple level ne_type combinations.
  sovereignty_tiny_map <- get_ne_data(latlong_bbox = NULL,
                                      projected_crs = "EPSG:3857",
                                      region = "United States of America",
                                      level = "sovereignty",
                                      ne_type = "tiny_countries",
                                      ne_scale = "medium")
  expect_s3_class(sovereignty_tiny_map[[1]], "sf")
})

test_that("get_ne_data handles edge cases", {
  # Test different ne_scale
  france_map_small <- get_ne_data(latlong_bbox = NULL,
                                  projected_crs = "EPSG:3857",
                                  region = "France",
                                  level = "country",
                                  ne_type = "countries",
                                  ne_scale = "small")
  expect_s3_class(france_map_small[[2]], "sf")

  # Test different region
  germany_map <- get_ne_data(latlong_bbox = NULL,
                             projected_crs = "EPSG:3857",
                             region = "Germany",
                             level = "country",
                             ne_type = "countries",
                             ne_scale = "medium")
  expect_s3_class(germany_map[[1]], "sf")
})

test_that("get_ne_data handles errors", {
  # Test invalid level
  expect_error(get_ne_data(latlong_bbox = NULL,
                           output_crs = "EPSG:3857",
                           region = "France",
                           level = "invalid",
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test that when region and latlong_bbox are both null we get an error
  expect_error(get_ne_data(latlong_bbox = NULL,
                              projected_crs = "EPSG:3857",
                              region = NULL,
                              level = "country",
                              ne_type = "countries",
                              ne_scale = "medium"))
  expect_error(get_ne_data(latlong_bbox = NULL,
                              projected_crs = "EPSG:3857",
                              region = NULL,
                              level = "continent",
                              ne_type = "countries",
                              ne_scale = "medium"))
  expect_error(get_ne_data(latlong_bbox = NULL,
                              projected_crs = "EPSG:3857",
                              region = NULL,
                              level = "sovereignty",
                              ne_type = "sovereignty",
                              ne_scale = "medium"))
  expect_error(get_ne_data(latlong_bbox = NULL,
                              projected_crs = "EPSG:3857",
                              region = NULL,
                              level = "geounit",
                              ne_type = "map_units",
                              ne_scale = "medium"))
  expect_error(get_ne_data(latlong_bbox = NULL,
                              projected_crs = "EPSG:3857",
                              region = NULL,
                              level = "country",
                              ne_type = "tiny_countries",
                              ne_scale = "medium"))

  # Test for a valid result when ne_type is NULL
  no_ne_type <- get_ne_data(latlong_bbox = NULL,
                            projected_crs = "EPSG:3857",
                            region = "France",
                            level = "country",
                            ne_type = NULL,
                            ne_scale = "medium")
  expect_s3_class(no_ne_type[[1]], "sf")

  # Test for error when level is null
  expect_error(get_ne_data(latlong_bbox = NULL,
                           projected_crs = "EPSG:3857",
                           region = "Germany",
                           level = NULL,
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test for error when output_crs is null
  expect_error(get_ne_data(latlong_bbox = NULL,
                           projected_crs = NULL,
                           region = "Germany",
                           level = "country",
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test for error when ne_scale is null
  expect_error(get_ne_data(latlong_bbox = NULL,
                           projected_crs = "EPSG:3857",
                           region = "Germany",
                           level = "country",
                           ne_type = "countries",
                           ne_scale = NULL))

})
