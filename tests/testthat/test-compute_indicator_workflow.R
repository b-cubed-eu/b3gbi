test_that("compute_indicator_workflow handles input validation", {
  # Create a mock processed_cube object
  mock_cube <- list(
    data = data.frame(
      xcoord = 1:10,
      ycoord = 1:10,
      year = 2000:2009,
      scientificName = "A",
      obs = 1:10
    ),
    first_year = 2000,
    last_year = 2009,
    num_species = 1,
    resolution = "10km",
    grid_type = "eqdgc",
    coord_range = c(1, 10, 1, 10)
  )
  names(mock_cube$coord_range) <- c("xmin", "xmax", "ymin", "ymax")
  class(mock_cube) <- c("processed_cube", "list")

  # Invalid data class
  expect_error(
    compute_indicator_workflow(
      data = "invalid",
      type = "obs_richness",
      dim_type = "map"
    ),
    "Object class not recognized."
  )

  # Invalid type
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "invalid_type",
      dim_type = "map"
    ),
    "'arg' should be one of"
  )

  # Invalid dim_type
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "invalid"
    ),
    "'arg' should be one of"
  )

  # Invalid ci_type
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      ci_type = "invalid"
    ),
    "'arg' should be one of"
  )

  # Invalid level
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      level = "invalid"
    ),
    "'arg' should be one of"
  )

  # Invalid ne_type
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      ne_type = "invalid"
    ),
    "'arg' should be one of"
  )

  # Invalid ne_scale
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      ne_scale = "invalid"
    ),
    "'arg' should be one of"
  )

  # Invalid shapefile_path
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      shapefile_path = "invalid_path.shp"
    ),
    "Shapefile not found at the specified path."
  )

  # Invalid first_year greater than last_year
  expect_error(
    compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      first_year = 2010,
      last_year = 2005
    ),
    "First year must be less than or equal to last year."
  )

  # Test sim_cube errors
  mock_sim_cube <- list(
    data = data.frame(
      xcoord = 1,
      ycoord = 1,
      year = 2000,
      scientificName = "A",
      obs = 1
    )
  )
  class(mock_sim_cube) <- c("sim_cube", "list")
  expect_error(
    compute_indicator_workflow(
      data = mock_sim_cube,
      type = "obs_richness",
      dim_type = "map"
    ),
    "You have provided an object of class 'sim_cube' as input."
  )

  # Test empty data
  empty_cube <- list(
    data = data.frame(),
    first_year = 2000,
    last_year = 2009,
    num_species = 0,
    resolution = "10km",
    grid_type = "eea"
  )
  class(empty_cube) <- c("processed_cube", "list")
  expect_error(
    compute_indicator_workflow(
      data = empty_cube,
      type = "obs_richness",
      dim_type = "map"
    ),
    "No data found in the cube."
  )
})


# Mock get_NE_data function
mock_get_NE_data <- function(latlong_bbox,
                             projected_crs,
                             region,
                             level,
                             ne_type,
                             ne_scale,
                             include_water = TRUE,
                             buffer_dist_km = NULL) {
  sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 0, 0, 10, 10, 10, 10, 0, 0, 0),
            # c(unname(latlong_bbox["xmin"])-1,
            #   unname(latlong_bbox["ymin"])-1,
            #   unname(latlong_bbox["xmin"])-1,
            #   unname(latlong_bbox["ymax"])+1,
            #   unname(latlong_bbox["xmax"])+1,
            #   unname(latlong_bbox["ymax"])+1,
            #   unname(latlong_bbox["xmax"])+1,
            #   unname(latlong_bbox["ymin"])-1,
            #   unname(latlong_bbox["xmin"])-1,
            #   unname(latlong_bbox["ymin"])-1
            #   ),
            ncol = 2,
            byrow = TRUE
          )
        )
      )
    ),
    crs = projected_crs
  )

}

test_that(
  "compute_indicator_workflow creates grids and performs spatial operations", {
  mock_cube <- list(
    data = data.frame(
      xcoord = c(9,1,5),
      ycoord = c(1,9,5),
      cellCode = c(1,2,3),
      year = c(2000,2000,2000),
      scientificName = c("A","A","A"),
      obs = c(1,1,1)
    ),
    first_year = 2000,
    last_year = 2000,
    coord_range = list("xmin" = 1,
                       "xmax" = 9,
                       "ymin" = 1,
                       "ymax" = 9),
    num_species = 1,
    resolutions = "10km",
    grid_type = "eea"
  )
  class(mock_cube) <- c("processed_cube", "list")

  # Mock get_NE_data
  with_mocked_bindings(
    `get_NE_data` = mock_get_NE_data,
    {
      # Test grid creation and intersection with EEA grid.
      result <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        level = "country",
        region = "Test",
        cell_size = 10
      )
      expect_s3_class(result$data, "sf")
      expect_true("cellid" %in% names(result$data))
    }
  )

  # Test shapefile filtering (requires a sample shapefile)
  # Create a temporary shapefile for testing.
  temp_shapefile <- tempfile(fileext = ".shp")
  sf::st_write(
    sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_polygon(
          list(
            matrix(
              c(2, 2, 2, 8, 8, 8, 8, 2, 2, 2),
              ncol = 2,
              byrow = TRUE
            )
          )
        )
      ),
      crs = "EPSG:3035"
    ),
    temp_shapefile
  )

  with_mocked_bindings(
    `get_NE_data` = mock_get_NE_data,
    {
      result_shapefile <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        level = "country",
        region = "Test",
        cell_size = 10,
        shapefile_path = temp_shapefile,
        output_crs = "EPSG:3035"
      )
      expect_s3_class(result_shapefile$data, "sf")
      # check that some data has been filtered.
      expect_true(nrow(result_shapefile$data) < 10)
    }
  )

  # cleanup
  file.remove(temp_shapefile);

  # test invert.
  temp_shapefile <- tempfile(fileext = ".shp")
  sf::st_write(
    sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_polygon(
          list(
            matrix(
              c(2, 2, 2, 8, 8, 8, 8, 2, 2, 2),
              ncol = 2,
              byrow = TRUE
            )
          )
        )
      ),
      crs = "EPSG:3035"
    ),
    temp_shapefile
  )

  with_mocked_bindings(
    `get_NE_data` = mock_get_NE_data,
    {
      result_invert <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        level = "country",
        region = "Test",
        cell_size = 10,
        shapefile_path = temp_shapefile,
        invert = TRUE,
        output_crs = "EPSG:3035"
      )
      expect_s3_class(result_invert$data, "sf")
      # check that some data has been filtered.
      expect_true(nrow(result_invert$data) < 10)
    }
  )

  # cleanup
  file.remove(temp_shapefile);

  # test spherical geometry.
  with_mocked_bindings(
    `get_NE_data` = mock_get_NE_data,
    {
      result_spherical <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        level = "country",
        region = "Test",
        cell_size = 10,
        spherical_geometry = FALSE
      )
      expect_s3_class(result_spherical$data, "sf")
    }
  )
})


test_that(
  "compute_indicator_workflow integrates indicator calculations correctly", {
  # Create a mock processed_cube object
    mock_cube <- list(
      data = data.frame(
        cellCode <- rep(seq(1000, 1100, length.out = 10), 10),
        xcoord = rep(seq(4000000, 4100000, length.out = 10), 10),
        ycoord = rep(seq(3000000, 3100000, length.out = 10), 10),
        year = rep(2000:2009, 10),
        scientificName = rep(
          c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
          10),
        obs = rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), each = 10),
        resolution = "10km"
      ),
      first_year = 2000,
      last_year = 2009,
      coord_range = list("xmin" = 4000000,
                         "xmax" = 4100000,
                         "ymin" = 3000000,
                         "ymax" = 3100000),
      num_species = 10,
      resolution = "10km",
      grid_type = "eea"
    )
    class(mock_cube) <- c("processed_cube", "list")

    # Test map calculation
    result_map <- compute_indicator_workflow(
      data = mock_cube,
      type = "obs_richness",
      dim_type = "map",
      include_water = TRUE
    )
    expect_equal(names(result_map$data), c("cellid",
                                           "area",
                                           "diversity_val",
                                           "geometry")
    )

    # Test time series calculations without confidence intervals
    result_ts <- compute_indicator_workflow(
      data = mock_cube,
      type = "total_occ",
      dim_type = "ts",
      ci_type = "none"
    )
    expect_equal(names(result_ts$data), c("year", "diversity_val"))

    # Test time series calculations with confidence intervals
    result_ci <- compute_indicator_workflow(
      data = mock_cube,
      type = "total_occ",
      dim_type = "ts",
      ci_type = "norm"
    )
    expect_true(
      all(
        c(
          "year",
          "diversity_val",
          "int_type",
          "ll",
          "ul",
          "conf_level"
        ) %in% names(result_ci$data)
      )
    )
})


test_that("compute_indicator_workflow creates output objects correctly", {
  # Set min and max coordinates
  xmin <- 4000000
  ymin <- 3000000
  xmax <- 4100000
  ymax <- 3100000
  # Create a mock processed_cube object
  mock_cube <- list(
    data = data.frame(
      cellCode = seq(1000, 1100, length.out = 10),
      xcoord = seq(xmin, xmax, length.out = 10),
      ycoord = seq(ymin, ymax, length.out = 10),
      obs = 1:10,
      year = 2000:2009,
      scientificName = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
      resolution = "10km"
    ),
    first_year = 2000,
    last_year = 2009,
    coord_range = list("xmin" = 4000000,
                       "xmax" = 4100000,
                       "ymin" = 3000000,
                       "ymax" = 3100000),
    num_species = 10,
    resolution = "10km",
    grid_type = "eea"
  )
  class(mock_cube) <- c("processed_cube", "list")

  # Test indicator_map object creation
  result_map <- suppressWarnings(compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map"
  ))
  expect_s3_class(result_map, "indicator_map")
  expect_s3_class(result_map$data, "sf")
  expect_equal(result_map$div_type, "obs_richness")
  expect_equal(result_map$first_year, 2000)
  expect_equal(result_map$last_year, 2009)
  expect_equal(result_map$num_species, 10)
  expect_equal(result_map$num_years, 10)
  expect_equal(result_map$species_names,
               c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))
  expect_equal(result_map$div_name, "Observed Species Richness")
  expect_equal(as.numeric(result_map$coord_range),
               c(xmin, ymin, xmax, ymax))
  expect_equal(names(result_map$coord_range),
               c("xmin", "ymin", "xmax", "ymax"))

  # Test indicator_ts object creation
  result_ts <- suppressWarnings(compute_indicator_workflow(
    data = mock_cube,
    type = "total_occ",
    dim_type = "ts",
    ci_type = "none"
  ))
  expect_s3_class(result_ts, "indicator_ts")
  expect_s3_class(result_ts$data, "data.frame")
  expect_equal(result_ts$div_type, "total_occ")
  expect_equal(result_ts$div_name, "Total Occurrences")
  expect_equal(as.numeric(result_ts$coord_range),
               c(xmin, ymin, xmax, ymax))
  expect_equal(names(result_ts$coord_range),
               c("xmin", "ymin", "xmax", "ymax"))
})


test_that("compute_indicator_workflow handles sim_cube objects", {
  # Create a valid sim_cube object
  mock_sim_cube <- list(
    data = data.frame(
      xcoord = rep(seq(4000000, 4900000, length.out = 10), 10),
      ycoord = rep(seq(3000000, 3900000, length.out = 10), 10),
      year = rep(2000:2009, 10),
      scientificName = rep(
        c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
        10),
      obs = rep(1:10, each = 100)
    ),
    first_year = 2000,
    last_year = 2009,
    num_species = 10
  )
  class(mock_sim_cube) <- c("sim_cube", "list")

  # Test invalid sim_cube operation (map)
  expect_error(
    compute_indicator_workflow(
      data = mock_sim_cube,
      type = "obs_richness",
      dim_type = "map"
    )
  )

  # Test valid sim_cube operation (ts) without confidence intervals
  result_ts <- compute_indicator_workflow(
    data = mock_sim_cube,
    type = "total_occ",
    dim_type = "ts",
    ci_type = "non"
  )
  expect_s3_class(result_ts, "indicator_ts")

  # Test valid sim_cube operation (ts) with confidence intervals
  result_ci <- compute_indicator_workflow(
    data = mock_sim_cube,
    type = "total_occ",
    dim_type = "ts",
    ci_type = "norm")
  expect_true(
    all(
      c(
        "year",
        "diversity_val",
        "int_type",
        "ll",
        "ul",
        "conf_level"
      ) %in% names(result_ci$data)
    )
  )

  # Test invalid sim_cube (missing 'obs' column)
  invalid_sim_cube <- list(
    data = data.frame(
      xcoord = 1,
      ycoord = 1,
      year = 2000,
      scientificName = "A"
    )
  )
  class(invalid_sim_cube) <- c("sim_cube", "list")
  expect_error(
    compute_indicator_workflow(
      data = invalid_sim_cube,
      type = "obs_richness",
      dim_type = "ts"
    ),
    "No occurrences found in the data."
  )
})

# Mock get_NE_data function
mock_get_NE_data <- function(region,
                             output_crs,
                             level,
                             ne_type,
                             ne_scale,
                             cube_cell_codes = NULL,
                             include_water = FALSE,
                             buffer_dist_km = NULL,
                             data = NULL,
                             layers = NULL) {
  sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 0, 0, 10, 10, 10, 10, 0, 0, 0),
            ncol = 2,
            byrow = TRUE
          )
        )
      )
    ),
    crs = output_crs
  )
}

test_that(
  "compute_indicator_workflow leaves the sf::sf_use_s2() setting as it was", {
  # Create a mock processed_cube object
    mock_cube <- list(
      data = data.frame(
        xcoord = c(1,5),
        ycoord = c(5,1),
        cellCode = c(1,2),
        year = c(2000,2000),
        scientificName = c("A","A"),
        obs = c(1,1)
      ),
      first_year = 2000,
      last_year = 2000,
      coord_range = list("xmin" = 1,
                          "xmax" = 5,
                          "ymin" = 1,
                          "ymax" = 5),
      num_species = 1,
      resolutions = "10km",
      grid_type = "eea"
    )
  class(mock_cube) <- c("processed_cube", "list")

  sf::sf_use_s2(TRUE)

  with_mocked_bindings(
    `get_NE_data` = mock_get_NE_data,
    {
      # Test spherical geometry setting
      result <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        spherical_geometry = TRUE
      )
      expect_true(sf::sf_use_s2())

      # Test spherical geometry setting
      result <- compute_indicator_workflow(
        data = mock_cube,
        type = "obs_richness",
        dim_type = "map",
        spherical_geometry = FALSE
      )
      expect_true(sf::sf_use_s2())
    })
})
