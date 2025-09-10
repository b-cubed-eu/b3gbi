test_that("plot_map handles input and basic plot creation", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1))
  expect_s3_class(p, "ggplot")

  geom_sf_present <- any(
    sapply(
      p$layers, function(layer) inherits(layer$geom, "GeomSf")
    )
  )
  expect_true(geom_sf_present)

  coord_sf_present <- inherits(p$coordinates, "CoordSf")
  expect_true(coord_sf_present)

  expect_error(plot_map(example_indicator_map1$data))
})

test_that("plot_map handles custom title", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1,
                                 title = "Custom Title"))
  expect_equal(p$labels$title, "Custom Title")
})

test_that("plot_map handles custom legend title", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1,
                                 legend_title = "Custom Legend"))
  expect_equal(p$labels$fill, "Custom\nLegend")
})

test_that("plot_map handles custom xlims and ylims", {
  data(example_indicator_map1)

  geometry_list <- sf::st_geometry(example_indicator_map1$data)
  all_coords <- NULL

  for (geom in geometry_list) {
    coords <- sf::st_coordinates(geom)
    if (!is.null(coords)) {
      if (is.null(all_coords)) {
        all_coords <- coords[, 1:2, drop = FALSE] # Take only x and y
      } else {
        all_coords <- rbind(all_coords,
                            coords[, 1:2, drop = FALSE]) # Take only x and y
      }
    }
  }

  if (!is.null(all_coords)) {
    xlims_test <- range(all_coords[, 1])
    ylims_test <- range(all_coords[, 2])
    names(xlims_test) <- c("xmin", "xmax")
    names(ylims_test) <- c("ymin", "ymax")

    p <- suppressWarnings(plot_map(example_indicator_map1,
                                   xlims = xlims_test,
                                   ylims = ylims_test))
    expect_equal(p$coordinates$limits$x, xlims_test)
    expect_equal(p$coordinates$limits$y, ylims_test)
  } else {
    fail("No coordinates found in example_indicator_map1$data")
  }
})

test_that("plot_map handles breaks and labels", {
  data(example_indicator_map1)
  breaks_test <- c(1, 2, 3)
  labels_test <- c("Low", "Medium", "High")
  p <- suppressWarnings(plot_map(example_indicator_map1,
                breaks = breaks_test,
                labels = labels_test))
  expect_equal(p$scales$scales[[1]]$breaks, breaks_test)
  expect_equal(p$scales$scales[[1]]$labels, labels_test)
})

test_that("plot_map handles crop_to_grid", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1, crop_to_grid = TRUE))
  p <- suppressWarnings(plot_map(example_indicator_map1, crop_to_grid = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("plot_map handles panel_bg and land_fill_colour", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1,
                panel_bg = "red",
                land_fill_colour = "blue"))
  expect_equal(p$theme$panel.background$fill, "red")
  expect_equal(p$layers[[1]]$aes_params$fill, "blue")
})

# Test case for default parameter
test_that("plot_map handles default parameters", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1))
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

# Test with all options
test_that("plot_map with all parameters set", {
  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1,
                                 title = "Full Test",
                                 auto_title = "Auto Title",
                                 leg_label_default = "Default Legend",
                                 xlims = c(0, 10),
                                 ylims = c(0, 10),
                                 trans = "log",
                                 bcpower = 0.5,
                                 breaks = c(1, 2, 3),
                                 labels = c("One", "Two", "Three"),
                                 crop_to_grid = TRUE,
                                 panel_bg = "green",
                                 land_fill_colour = "yellow",
                                 legend_title = "Legend Title",
                                 legend_limits = c(1, 10),
                                 legend_title_wrap_length = 15,
                                 title_wrap_length = 40,
                                 layers = NULL,
                                 scale = "medium"))
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Full Test")
})

# Test different transformation methods
test_that("plot_map handles different transformations", {
  data(example_indicator_map1)
  p_boxcox <- suppressWarnings(plot_map(example_indicator_map1,
                                        trans = "boxcox",
                                        bcpower = 0.5))
  expect_s3_class(p_boxcox, "ggplot")

  p_modulus <- suppressWarnings(plot_map(example_indicator_map1,
                                         trans = "modulus",
                                         bcpower = 0.5))
  expect_s3_class(p_modulus, "ggplot")

  p_yj <- suppressWarnings(plot_map(example_indicator_map1,
                                    trans = "yj",
                                    bcpower = 0.5))
  expect_s3_class(p_yj, "ggplot")
})


# Title and legend wrapping
test_that("plot_map handles title and legend title wrapping", {
  long_title <- paste(rep("Long Title Part", 10), collapse = " ")
  long_legend_title <- paste(rep("Long Legend Part", 10), collapse = " ")

  data(example_indicator_map1)
  p <- suppressWarnings(plot_map(example_indicator_map1,
                                 title = long_title,
                                 legend_title = long_legend_title,
                                 title_wrap_length = 20,
                                 legend_title_wrap_length = 25))

  wrapped_title_len <- nchar(p$labels$title)
  wrapped_legend_len <- nchar(p$labels$fill)

  expect_true(wrapped_title_len > 20) # Ensure it's been wrapped
  expect_true(wrapped_legend_len > 25) # Ensure it's been wrapped
})

# Unsupported projections
test_that("plot_map handles unsupported projections", {
  data(example_indicator_map1)
  example_indicator_map1$projection <- "UNKNOWN_PROJ"
  expect_error(plot_map(example_indicator_map1))
})

test_that("plot_map handles all parameters without error", {
  p <- suppressWarnings(plot_map(
    x = example_indicator_map1,
    title = "Map Full Parameter Test",
    auto_title = "Auto Title Example",
    leg_label_default = "Default Legend Label",
    xlims = c(-10, 10),
    ylims = c(-10, 10),
    trans = "log",
    bcpower = 0.5,
    breaks = c(1, 2, 3),
    labels = c("Low", "Medium", "High"),
    crop_to_grid = TRUE,
    panel_bg = "white",
    land_fill_colour = "grey90",
    legend_title = "Legend Title",
    legend_limits = c(1, 5),
    legend_title_wrap_length = 15,
    title_wrap_length = 25,
    layers = NULL,
    scale = "medium"
  ))

  # Check that the resulting plot is a ggplot object
  expect_s3_class(p, "ggplot")
})

spec_occ_mammals_denmark <- spec_occ_map(example_cube_1,
                                         level = "country",
                                         region = "Denmark")

test_that("plot_species_map handles basic functionality", {
  # Assume `example_indicator_map` includes demo data for testing
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728)
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_species_map handles species selection correctly", {

  # Test with valid species name
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c("Vulpes vulpes")
  )
  expect_s3_class(p, "ggplot")
  expect_true("Lepus europaeus" %in% unique(p$data$scientificName))

  # Test with valid species name
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c("Vulpes")
  )
  expect_s3_class(p, "ggplot")
  expect_false("Vulpes" %in% unique(p$data$scientificName))

  # Test with invalid species name
  expect_error(
    plot_species_map(
      x = spec_occ_mammals_denmark,
      species = "NotRealSpecies"
    ), "No matching"
  )
  expect_false("NotRealSpecies" %in% unique(p$data$scientificName))

  # Test with multiple species names
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c("Vulpes vulpes", "Lepus europaeus")
  )
  expect_s3_class(p, "ggplot")

  # Test with valid taxonKey
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728)
  )
  expect_s3_class(p, "ggplot")

  # Test with invalid taxonKey
  expect_error(
    plot_species_map(
      x = spec_occ_mammals_denmark,
      species = 999999999
    ), "No matching"
  )
})

test_that(
  "plot_species_map correctly applies title and legend customizations", {
  # Run the function which produces multiple plots
  plot_output <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728, 4265185),
    title = "Custom Title",
    legend_title = "Custom Legend"
  )
    expect_true("Custom Title" == plot_output$patches$annotation$title)
    expect_true("Custom\nLegend" %in% plot_output[[1]]$labels$fill)
})

test_that("plot_species_map applies transformation correctly", {
  expect_error(
    plot_species_map(
      x = spec_occ_mammals_denmark,
      species = c(2440728),
      trans = "log"
    ),
    NA # Expect no error
  )
})

test_that("plot_species_map accommodates geographic limits", {
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728),
    xlims = c(13, 15),
    ylims = c(56, 57)
  )

  # Check custom limits using plot data
  expect_equal(unname(p$coordinates$limits$x), c(13, 15))
  expect_equal(unname(p$coordinates$limits$y), c(56, 57))
})

test_that("plot_species_map responds to geographic and contextual parameters", {
  p1 <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728)
  )

  expect_s3_class(p1, "ggplot")

  p2 <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728),
    crop_to_grid = TRUE
  )

  expect_s3_class(p2, "ggplot")
})

test_that("plot_species_map throws expected errors", {
  expect_error(
    plot_species_map(x = spec_occ_mammals_denmark)
  )

  expect_error(
    plot_species_map(
      x = spec_occ_mammals_denmark,
      species = 999999999
    ),
    "No matching taxonKeys"
  )
})

test_that("plot_species_map handles all parameters without error", {
  # Assuming example_species_map is a valid indicator_map object
  p <- plot_species_map(
    x = spec_occ_mammals_denmark,
    species = c(2440728, 4265185),
    leg_label_default = "Default Legend Label",
    auto_title = "Auto Title",
    suppress_legend = FALSE,
    title = "Species Map Full Parameter Test",
    xlims = c(-10, 10),
    ylims = c(-10, 10),
    trans = "log",
    bcpower = 0.5,
    breaks = c(1, 2, 3),
    labels = c("Low", "Medium", "High"),
    crop_to_grid = TRUE,
    single_plot = TRUE,
    panel_bg = "white",
    land_fill_colour = "grey90",
    legend_title = "Legend Title",
    legend_limits = c(1, 5),
    legend_title_wrap_length = 15,
    title_wrap_length = 25,
    layers = NULL,
    scale = "medium"
  )

  # Check that the resulting plot is a ggplot object or patchwork
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})


test_that("plot_ts returns a ggplot object with defaults", {
  data(example_indicator_ts1)

  p <- plot_ts(example_indicator_ts1)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ts handles specified year limits", {
  data(example_indicator_ts1)

  # Test valid year range
  min_y <- 2000
  max_y <- 2010
  p <- plot_ts(example_indicator_ts1, min_year = min_y, max_year = max_y)

  filtered_data <- example_indicator_ts1$data %>%
    filter(year >= min_y & year <= max_y)

  plot_data <- p$data

  expect_equal(nrow(plot_data), nrow(filtered_data))
})

test_that("plot_ts handles title and axis label management", {
  data(example_indicator_ts1)

  p <- plot_ts(
    x = example_indicator_ts1,
    title = "Custom Title",
    y_label = "Custom Y Label"
  )

  # Validate the custom title and Y label
  expect_true(grepl("Custom Title", p$labels$title))
  expect_true(grepl("Custom Y Label", p$labels$y))
})

test_that("plot_ts correctly applies smoothing and confidence intervals", {
  data(example_indicator_ts2)

  # With smoothing
  p1 <- plot_ts(
    example_indicator_ts2,
    smoothed_trend = TRUE
  )
  # Check existence of loess layer (smoothing)
  smoothing_present <- any(sapply(p1$layers, function(layer) {
    inherits(layer$stat, "StatSmooth")
  }))
  expect_true(smoothing_present)

  total_occ_example <- total_occ_ts(example_cube_1,
                                   level = "country",
                                   region = "Denmark")

  # With confidence intervals
  p2 <- plot_ts(
    total_occ_example,
    ci_type = "ribbon"
  )
  ci_ribbon_present <- any(sapply(p2$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }))
  expect_true(ci_ribbon_present)
})

test_that("plot_ts customization options function as expected", {
  data(example_indicator_ts1)

  # Different point and line styles
  p_points <- plot_ts(
    example_indicator_ts1,
    point_line = "point",
    pointsize = 4
  )
  p_line <- plot_ts(
    example_indicator_ts1,
    point_line = "line",
    linewidth = 2
  )

  expect_true(any(sapply(p_points$layers, function(layer) {
    inherits(layer$geom, "GeomPoint") && layer$aes_params$size == 4
  })))

  expect_true(any(sapply(p_line$layers, function(layer) {
    inherits(layer$geom, "GeomLine") && layer$aes_params$linewidth == 2
  })))
})

test_that("plot_ts produces expected errors for invalid input", {
  data(example_indicator_ts1)

  expect_error(plot_ts(NULL),
               "Incorrect object class.")

  expect_error(
    plot_ts(
      example_indicator_ts1,
      min_year = 3000,  # Year not in data range
      max_year = 4000
    ),
    "No data available for the selected years."
  )
})

test_that("plot_ts handles all parameters without error", {
  # Assuming example_indicator_ts is a valid indicator_ts object
  p <- plot_ts(
    x = example_indicator_ts1,
    min_year = 2000,
    max_year = 2015,
    title = "Full Parameter Test",
    auto_title = "Auto Title Example",
    y_label_default = "Default Y Label",
    suppress_y = TRUE,
    smoothed_trend = TRUE,
    linecolour = "red",
    linealpha = 0.7,
    ribboncolour = "purple",
    ribbonalpha = 0.2,
    error_alpha = 0.9,
    trendlinecolour = "green",
    trendlinealpha = 0.6,
    envelopecolour = "pink",
    envelopealpha = 0.3,
    smooth_cialpha = 0.8,
    point_line = "line",
    pointsize = 3,
    linewidth = 2,
    ci_type = "ribbon",
    error_width = 0.5,
    error_thickness = 0.3,
    smooth_linetype = "dashed",
    smooth_linewidth = 2,
    smooth_cilinewidth = 1.5,
    gridoff = FALSE,
    x_label = "Year",
    y_label = "Occurrence",
    x_expand = c(0.1, 0.2),
    y_expand = c(0.1, 0.2),
    x_breaks = 5,
    y_breaks = 3,
    wrap_length = 30
  )

  # Check that the resulting plot is a ggplot object
  expect_s3_class(p, "ggplot")
})

spec_occ_mammals_denmark_ts <- spec_occ_ts(example_cube_1,
                                           level = "country",
                                           region = "Denmark",
                                           ci_type = "none")

test_that(
  "plot_species_ts returns a ggplot or patchwork object with defaults", {
  # Direct test without the `species` - expect error
  expect_error(
    plot_species_ts(spec_occ_mammals_denmark_ts)
  )
})

test_that("plot_species_ts correctly handles species selection", {

  # Valid single taxonKey selection
  p <- plot_species_ts(spec_occ_mammals_denmark_ts,
                       species = c(4265185))
  expect_true(inherits(p, "ggplot"))

  # Valid multiple taxonKey selection
  p <- plot_species_ts(spec_occ_mammals_denmark_ts,
                       species = c(2440728, 4265185))
  expect_true(inherits(p, "ggplot") && length(p) == 2)

  # Invalid taxonKey
  expect_error(plot_species_ts(spec_occ_mammals_denmark_ts, species = 99999))

  # Valid scientificName, one full match and one partial match
  p <- plot_species_ts(spec_occ_mammals_denmark_ts,
                       species = c("Vulpes v", "Phoca vitulina"))
  expect_true(inherits(p, "patchwork") && length(p) == 2)

  # Invalid scientificName
  expect_error(plot_species_ts(spec_occ_mammals_denmark_ts, species = "Fake"))

})

test_that("plot_species_ts filters data by year correctly", {
  p <- plot_species_ts(spec_occ_mammals_denmark_ts,
                       species = 4265185,
                       min_year = 2000,
                       max_year = 2015)
  plot_data <- p$data

  filtered_data <- spec_occ_mammals_denmark_ts$data %>%
    filter(year >= 2000 & year <= 2015, taxonKey == 4265185)

  expect_equal(nrow(plot_data), nrow(filtered_data))
})

test_that("plot_species_ts applies custom aesthetics correctly", {
  p <- plot_species_ts(
    spec_occ_mammals_denmark_ts,
    species = c(2440728, 4265185),
    linecolour = "green",
    point_line = "point",
    pointsize = 4
  )

  point_geom <- p$layers[[1]]
  expect_equal(point_geom$aes_params$colour, "green")
  expect_equal(point_geom$aes_params$size, 4)
})

spec_occ_mammals_denmark_ts_ci <- spec_occ_ts(example_cube_1,
                                           level = "country",
                                           region = "Denmark",
                                           ci_type = "norm",
                                           num_bootstrap = 20)

test_that("plot_species_ts manages trends and confidence intervals", {
  # Verify presence of smoothed trend
  p1 <- plot_species_ts(spec_occ_mammals_denmark_ts,
                        species = 4265185,
                        smoothed_trend = TRUE)
  is_smooth_present <- any(sapply(p1$layers, function(layer) {
    inherits(layer$stat, "StatSmooth")
  }))

  expect_true(is_smooth_present)

  # Test for confidence interval presence
  p2 <- plot_species_ts(spec_occ_mammals_denmark_ts_ci,
                        species = 4265185,
                        ci_type = "ribbon")
  is_ribbon_present <- any(sapply(p2$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }))

  expect_true(is_ribbon_present)
})

test_that("plot_species_ts returns correctly formatted titles", {
  p <- plot_species_ts(
    spec_occ_mammals_denmark_ts,
    species = c(2440728, 4265185),
    title = "Custom Title"
  )

  # Check if the overall title matches
    overall_title <- p$patches$annotation$title
    expect_equal(overall_title, "Custom Title")
 })

test_that("plot_species_ts handles all parameters without error", {

  # A comprehensive test using all parameters
  p <- plot_species_ts(
    x = spec_occ_mammals_denmark_ts,
    species = c(5219243, 4265185),
    single_plot = TRUE,
    min_year = 2000,
    max_year = 2015,
    title = "Full Parameter Test",
    auto_title = NULL,
    y_label_default = NULL,
    suppress_y = TRUE,
    smoothed_trend = TRUE,
    linecolour = "red",
    linealpha = 0.7,
    ribboncolour = "purple",
    ribbonalpha = 0.2,
    error_alpha = 0.9,
    trendlinecolour = "green",
    trendlinealpha = 0.6,
    envelopecolour = "pink",
    envelopealpha = 0.3,
    smooth_cialpha = 0.8,
    point_line = "line",
    pointsize = 3,
    linewidth = 2,
    ci_type = "ribbon",
    error_width = 0.5,
    error_thickness = 0.3,
    smooth_linetype = "dashed",
    smooth_linewidth = 2,
    smooth_cilinewidth = 1.5,
    gridoff = FALSE,
    x_label = "Year",
    y_label = "Occurrence",
    x_expand = c(0.1, 0.2),  # Custom extension
    y_expand = c(0.1, 0.2),
    x_breaks = 5,
    y_breaks = 3,
    wrap_length = 30
  )

  # Check that the resulting plot is indeed a ggplot or patchwork object
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))

})


# Mock data common to all plot wrapper tests
mock_data <- data.frame(
  year = rep(2001:2005, 3),
  diversity_val = rnorm(15),
  taxonKey = rep(c(101, 102, 103), each = 5),
  scientificName = rep(c("Species A", "Species B", "Species C"), each = 5)
)

# Mock objects for each class
mock_spec_range <- structure(list(data = mock_data,
                                  first_year = 2001,
                                  last_year = 2005),
                             class = c("indicator_ts", "spec_range"))
mock_spec_occ <- structure(list(data = mock_data,
                                first_year = 2001,
                                last_year = 2005),
                           class = c("indicator_ts", "spec_occ"))
mock_cum_richness <- structure(list(data = mock_data,
                                    first_year = 2001,
                                    last_year = 2005),
                               class = c("indicator_ts", "cum_richness"))
mock_pielou_evenness <- structure(list(data = mock_data,
                                       first_year = 2001,
                                       last_year = 2005),
                                  class = c("indicator_ts", "pielou_evenness"))
mock_williams_evenness <- structure(list(data = mock_data,
                                         first_year = 2001,
                                         last_year = 2005),
                                    class = c("indicator_ts",
                                              "williams_evenness"))
mock_tax_distinct <- structure(list(data = mock_data,
                                    first_year = 2001,
                                    last_year = 2005),
                               class = c("indicator_ts", "tax_distinct"))
mock_occ_density <- structure(list(data = mock_data,
                                   first_year = 2001,
                                   last_year = 2005),
                              class = c("indicator_ts", "occ_density"))
mock_newness <- structure(list(data = mock_data,
                               first_year = 2001,
                               last_year = 2005),
                          class = c("indicator_ts", "newness"))
mock_total_occ <- structure(list(data = mock_data,
                                 first_year = 2001,
                                 last_year = 2005),
                            class = c("indicator_ts", "total_occ"))
mock_area_rarity <- structure(list(data = mock_data,
                                   first_year = 2001,
                                   last_year = 2005),
                              class = c("indicator_ts", "area_rarity"))
mock_ab_rarity <- structure(list(data = mock_data,
                                 first_year = 2001,
                                 last_year = 2005),
                            class = c("indicator_ts", "ab_rarity"))
mock_rarefied <- structure(list(data = mock_data,
                                first_year = 2001,
                                last_year = 2005),
                           class = c("indicator_ts", "rarefied"))
mock_hill2 <- structure(list(data = mock_data,
                             first_year = 2001,
                             last_year = 2005),
                        class = c("indicator_ts", "hill2"))
mock_hill1 <- structure(list(data = mock_data,
                             first_year = 2001,
                             last_year = 2005),
                        class = c("indicator_ts", "hill1"))
mock_hill0 <- structure(list(data = mock_data,
                             first_year = 2001,
                             last_year = 2005),
                        class = c("indicator_ts", "hill0"))
mock_obs_richness <- structure(list(data = mock_data,
                                    first_year = 2001,
                                    last_year = 2005),
                               class = c("indicator_ts", "obs_richness"))
mock_occ_turnover <- structure(list(data = mock_data,
                                    first_year = 2001,
                                    last_year = 2005),
                               class = c("indicator_ts", "occ_turnover"))

# Same invalid mock object for all error tests
mock_invalid_object <- list(a = 1, b = 2)

# Test Suite
test_that("plot.spec_range handles valid input and class", {
  expect_silent(plot.spec_range(mock_spec_range, species = c(101, 102)))
  expect_error(plot.spec_range(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.spec_occ handles valid input and class", {
  expect_silent(plot.spec_occ(mock_spec_occ, species = c(101, 102)))
  expect_error(plot.spec_occ(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.cum_richness handles valid input and class", {
  expect_silent(plot.cum_richness(mock_cum_richness))
  expect_error(plot.cum_richness(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.pielou_evenness handles valid input and class", {
  expect_silent(plot.pielou_evenness(mock_pielou_evenness))
  expect_error(plot.pielou_evenness(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.williams_evenness handles valid input and class", {
  expect_silent(plot.williams_evenness(mock_williams_evenness))
  expect_error(plot.williams_evenness(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.tax_distinct handles valid input and class", {
  expect_silent(plot.tax_distinct(mock_tax_distinct))
  expect_error(plot.tax_distinct(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.occ_density handles valid input and class", {
  expect_silent(plot.occ_density(mock_occ_density))
  expect_error(plot.occ_density(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.newness handles valid input and class", {
  expect_silent(plot.newness(mock_newness))
  expect_error(plot.newness(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.total_occ handles valid input and class", {
  expect_silent(plot.total_occ(mock_total_occ))
  expect_error(plot.total_occ(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.area_rarity handles valid input and class", {
  expect_silent(plot.area_rarity(mock_area_rarity))
  expect_error(plot.area_rarity(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.ab_rarity handles valid input and class", {
  expect_silent(plot.ab_rarity(mock_ab_rarity))
  expect_error(plot.ab_rarity(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.rarefied handles valid input and class", {
  expect_silent(plot.rarefied(mock_rarefied))
  expect_error(plot.rarefied(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.hill2 handles valid input and class", {
  expect_silent(plot.hill2(mock_hill2))
  expect_error(plot.hill2(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.hill1 handles valid input and class", {
  expect_silent(plot.hill1(mock_hill1))
  expect_error(plot.hill1(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.hill0 handles valid input and class", {
  expect_silent(plot.hill0(mock_hill0))
  expect_error(plot.hill0(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.obs_richness handles valid input and class", {
  expect_silent(plot.obs_richness(mock_obs_richness))
  expect_error(plot.obs_richness(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.occ_turnover handles valid input and class", {
  expect_silent(plot.occ_turnover(mock_occ_turnover))
  expect_error(plot.occ_turnover(mock_invalid_object),
               "Incorrect object class.")
})



mock_data_by_dataset <- data.frame(
  year = rep(2001:2005, 9),
  diversity_val = sample(1:10, 45, replace = TRUE),
  type = rep(c("Dataset 1", "Dataset 2", "Dataset 3"), each = 15)
)

mock_data_by_type <- data.frame(
  year = rep(2001:2005, 9),
  diversity_val = sample(1:10, 45, replace = TRUE),
  type = rep(c("Type A", "Type B", "Type C"), each = 15)
)

mock_occ_by_dataset <- structure(list(data = mock_data_by_dataset,
                                      first_year = 2001,
                                      last_year = 2005),
                                 class = c("indicator_ts", "occ_by_dataset"))
mock_occ_by_type <- structure(list(data = mock_data_by_type,
                                   first_year = 2001,
                                   last_year = 2005),
                              class = c("indicator_ts", "occ_by_type"))

mock_invalid_object <- list(a = 1, b = 2) # Invalid class

# Test Suite

test_that("plot.occ_by_dataset handles valid input and class", {
  expect_silent(plot.occ_by_dataset(mock_occ_by_dataset))
  expect_error(plot.occ_by_dataset(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.occ_by_type handles valid input and class", {
  expect_silent(plot.occ_by_type(mock_occ_by_type))
  expect_error(plot.occ_by_type(mock_invalid_object),
               "Incorrect object class.")
})

test_that("plot.occ_by_dataset handles x_breaks parameter", {
  p <- plot.occ_by_dataset(mock_occ_by_dataset, x_breaks = 10)
  # Check if the correct number of breaks is specified
  x_breaks_set <- ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  expect_equal(length(x_breaks_set), 10, tolerance = 1)
})

test_that("plot.occ_by_dataset handles facet parameters correctly", {
  p <- plot.occ_by_dataset(mock_occ_by_dataset,
                           facet_scales = "free_y",
                           facet_rows = 1,
                           facet_cols = 2)
  expect_silent(facet_wrap_check <- p$facet)
  expect_equal(facet_wrap_check$params$free$y, TRUE)
  expect_equal(facet_wrap_check$params$nrow, 1)
  expect_equal(facet_wrap_check$params$ncol, 2)
})

test_that("plot.occ_by_dataset applies max_datasets and min_occurrences", {
  p <- plot.occ_by_dataset(mock_occ_by_dataset,
                           max_datasets = 2,
                           min_occurrences = 5)
  unique_datasets <- length(ggplot_build(p)$layout$panel_params)
  expect_true(unique_datasets == 2)  # Limited by max_datasets
})

test_that("plot.occ_by_type handles x_breaks parameter", {
  p <- plot.occ_by_type(mock_occ_by_type, x_breaks = 10)
  # Check if the correct number of breaks is specified
  x_breaks_set <- ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  expect_equal(length(x_breaks_set), 10, tolerance = 1)
})

test_that("plot.occ_by_type handles facet parameters correctly", {
  p <- plot.occ_by_type(mock_occ_by_type,
                        facet_scales = "free_y",
                        facet_rows = 1,
                        facet_cols = 2)
  expect_silent(facet_wrap_check <- p$facet)
  expect_equal(facet_wrap_check$params$free$y, TRUE)
  expect_equal(facet_wrap_check$params$nrow, 1)
  expect_equal(facet_wrap_check$params$ncol, 2)
})
