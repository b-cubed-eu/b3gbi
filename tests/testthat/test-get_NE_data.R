test_that("get_NE_data retrieves map data correctly", {
  # Test country level
  france_map <- get_NE_data(region = "France",
                            output_crs = "EPSG:3857",
                            level = "country",
                            ne_type = "countries",
                            ne_scale = "medium")
  expect_s3_class(france_map, "sf")
  expect_equal(sf::st_crs(france_map), sf::st_crs("EPSG:3857"))

  # Test continent level
  africa_map <- get_NE_data(region = "Africa",
                            output_crs = "EPSG:3857",
                            level = "continent",
                            ne_type = "countries",
                            ne_scale = "medium")
  expect_s3_class(africa_map, "sf")
  expect_equal(sf::st_crs(africa_map), sf::st_crs("EPSG:3857"))

  # Test world level
  world_map <- get_NE_data(region = NULL,
                           output_crs = "EPSG:3857",
                           level = "world",
                           ne_type = "countries",
                           ne_scale = "medium")
  expect_s3_class(world_map, "sf")
  expect_equal(sf::st_crs(world_map), sf::st_crs("EPSG:3857"))

  # Test cube level (default)
  mock_data <- data.frame(resolution = c("10km", "10km"),
                          xcoord = c("5", "3"),
                          ycoord = c("5", "6"))
  cube_map <- suppressWarnings(get_NE_data(region = NULL,
                          output_crs = "EPSG:3857",
                          data = mock_data,
                          include_water = FALSE,
                          layers = NULL))
  expect_s3_class(cube_map, "sf")
  expect_equal(sf::st_crs(cube_map), sf::st_crs("EPSG:3857"))

  # Test sovereignty level
  sovereignty_map <- get_NE_data(region = "United States of America",
                                 output_crs = "EPSG:3857",
                                 level = "sovereignty",
                                 ne_type = "sovereignty",
                                 ne_scale = "medium")
  expect_s3_class(sovereignty_map, "sf")

  # Test geounit level
  geounit_map <- get_NE_data(region = "Germany",
                             output_crs = "EPSG:3857",
                             level = "geounit",
                             ne_type = "map_units",
                             ne_scale = "medium")
  expect_s3_class(geounit_map, "sf")

  # Test tiny_countries
  tiny_countries_map <- get_NE_data(region = "France",
                                    output_crs = "EPSG:3857",
                                    level = "country",
                                    ne_type = "tiny_countries",
                                    ne_scale = "medium")
  expect_s3_class(tiny_countries_map, "sf")

  # test multiple level ne_type combinations.
  sovereignty_tiny_map <- get_NE_data(region = 'United States of America',
                                      output_crs = "EPSG:3857",
                                      level = "sovereignty",
                                      ne_type = "tiny_countries",
                                      ne_scale = "medium")
  expect_s3_class(sovereignty_tiny_map, "sf")
})

test_that("get_NE_data handles edge cases", {
  # Test different ne_scale
  france_map_small <- get_NE_data(region = "France",
                                  output_crs = "EPSG:3857",
                                  level = "country",
                                  ne_type = "countries",
                                  ne_scale = "small")
  expect_s3_class(france_map_small, "sf")

  # Test different region
  germany_map <- get_NE_data(region = "Germany",
                             output_crs = "EPSG:3857",
                             level = "country",
                             ne_type = "countries",
                             ne_scale = "medium")
  expect_s3_class(germany_map, "sf")
})

test_that("get_NE_data handles errors", {
  # Test invalid level
  expect_error(get_NE_data(region = "France",
                           output_crs = "EPSG:3857",
                           level = "invalid",
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test that when region is null we still get a valid output
  expect_s3_class(get_NE_data(region = NULL,
                              output_crs = "EPSG:3857",
                              level = "country",
                              ne_type = "countries",
                              ne_scale = "medium"),
                  "sf")
  expect_s3_class(get_NE_data(region = NULL,
                              output_crs = "EPSG:3857",
                              level = "continent",
                              ne_type = "countries",
                              ne_scale = "medium"),
                  "sf")
  expect_s3_class(get_NE_data(region = NULL,
                              output_crs = "EPSG:3857",
                              level = "sovereignty",
                              ne_type = "sovereignty",
                              ne_scale = "medium"),
                  "sf")
  expect_s3_class(get_NE_data(region = NULL,
                              output_crs = "EPSG:3857",
                              level = "geounit",
                              ne_type = "map_units",
                              ne_scale = "medium"),
                  "sf")
  expect_s3_class(get_NE_data(region = NULL,
                              output_crs = "EPSG:3857",
                              level = "country",
                              ne_type = "tiny_countries",
                              ne_scale = "medium"),
                  "sf")

  # Test null ne_type when ne_type is needed
  expect_error(get_NE_data(region = "France",
                           output_crs = "EPSG:3857",
                           level = "country",
                           ne_type = NULL,
                           ne_scale = "medium"))
  expect_error(get_NE_data(region = "Africa",
                           output_crs = "EPSG:3857",
                           level = "continent",
                           ne_type = NULL,
                           ne_scale = "medium"))
  expect_error(get_NE_data(region = "United States of America",
                           output_crs = "EPSG:3857",
                           level = "sovereignty",
                           ne_type = NULL,
                           ne_scale = "medium"))
  expect_error(get_NE_data(region = "Germany",
                           output_crs = "EPSG:3857",
                           level = "geounit",
                           ne_type = NULL,
                           ne_scale = "medium"))

  # Test for error when level is null
  expect_error(get_NE_data(region = "Germany",
                           output_crs = "EPSG:3857",
                           level = NULL,
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test for error when output_crs is null
  expect_error(get_NE_data(region = "Germany",
                           output_crs = NULL,
                           level = "countries",
                           ne_type = "countries",
                           ne_scale = "medium"))

  # Test for error when ne_scale is null
  expect_error(get_NE_data(region = "Germany",
                           output_crs = "EPSG:3857",
                           level = "countries",
                           ne_type = "countries",
                           ne_scale = NULL))

})
