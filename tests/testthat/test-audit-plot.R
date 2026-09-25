# Tests for fixes made during the plotting audit

# Helper: a fake Natural Earth layer so that extra layers can be tested
# without downloading data
fake_ne_layer <- function(layer_name, scale, extent_projected) {
  layer_raw <- rnaturalearth::ne_countries(scale = "medium",
                                           returnclass = "sf")
  sf::st_agr(layer_raw) <- "constant"
  suppressWarnings(suppressMessages(
    sf::st_crop(sf::st_make_valid(layer_raw), extent_projected)
  ))
}

# Get the colour (or fill) used by each geom_sf layer that draws a given
# Natural Earth layer, identified by the number of rows it plots
layer_values <- function(p, column) {
  b <- ggplot2::ggplot_build(p)
  vapply(b$data, function(d) {
    vals <- unique(d[[column]])
    if (length(vals) == 1) as.character(vals) else NA_character_
  }, character(1))
}

test_that("plot_map applies one layer colour per extra layer", {
  skip_if_not_installed("rnaturalearthdata")
  local_mocked_bindings(add_ne_layer = fake_ne_layer)

  p <- suppressWarnings(suppressMessages(
    plot_map(example_indicator_map1,
             layers = c("layer_a", "layer_b"),
             layer_colours = c("blue", "green"),
             layer_fill_colours = c("pink", "yellow"))
  ))
  expect_s3_class(p, "ggplot")

  cols <- layer_values(p, "colour")
  fills <- layer_values(p, "fill")

  # Layers: 1 = surrounding land, 2 = admin_0, 3 = layer_a, 4 = layer_b,
  # 5 = indicator grid, 6-8 = re-drawn outlines of admin_0, layer_a, layer_b
  expect_equal(cols[[2]], "black")
  expect_equal(cols[[3]], "blue")
  expect_equal(cols[[4]], "green")
  expect_equal(fills[[3]], "pink")
  expect_equal(fills[[4]], "yellow")
  # Outlines re-drawn on top keep the custom colours (not black)
  expect_equal(unname(cols[6:8]), c("black", "blue", "green"))
})

test_that("plot_map errors if layer_colours length does not match layers", {
  expect_error(
    plot_map(example_indicator_map1,
             layers = c("layer_a", "layer_b"),
             layer_colours = "blue"),
    "same length as layers"
  )
})

test_that("plot_map uses grid_fill_colour for empty grid cells", {
  skip_if_not_installed("rnaturalearthdata")
  n_na <- sum(is.na(example_indicator_map1$data$diversity_val))
  skip_if(n_na == 0)

  # Default: empty cells stay transparent
  p_default <- suppressWarnings(plot_map(example_indicator_map1))
  b <- ggplot2::ggplot_build(p_default)
  grid_fill <- b$data[[3]]$fill
  expect_equal(sum(grid_fill == "transparent" |
                     grepl("^#[0-9A-Fa-f]{6}00$", grid_fill)), n_na)

  # Custom colour with full opacity
  p_red <- suppressWarnings(plot_map(example_indicator_map1,
                                     grid_fill_colour = "red",
                                     grid_fill_transparency = 1))
  b <- ggplot2::ggplot_build(p_red)
  expect_equal(sum(b$data[[3]]$fill == "#FF0000FF"), n_na)

  # Default transparency (0.2) is applied to a custom colour
  p_red2 <- suppressWarnings(plot_map(example_indicator_map1,
                                      grid_fill_colour = "red"))
  b <- ggplot2::ggplot_build(p_red2)
  expect_equal(sum(b$data[[3]]$fill == "#FF000033"), n_na)
})

test_that("plot() on an indicator_map uses legend_title if supplied", {
  skip_if_not_installed("rnaturalearthdata")
  p <- suppressWarnings(plot(example_indicator_map1, legend_title = "XX"))
  expect_equal(p$labels$fill, "XX")
  p <- suppressWarnings(plot(example_indicator_map1))
  expect_equal(p$labels$fill, "Richness")
})

test_that("plot_species_map suppresses the legend for range maps", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("rnaturalearthdata")

  range_map <- suppressWarnings(suppressMessages(
    spec_range_map(example_cube_1, level = "cube")
  ))
  expect_true(inherits(range_map, "spec_range"))

  keys <- unique(range_map$data$taxonKey)[1:2]
  plots <- suppressWarnings(suppressMessages(
    plot_species_map(range_map, species = keys, single_plot = FALSE)
  ))
  expect_type(plots, "list")
  expect_length(plots, 2)
  expect_equal(plots[[1]]$theme$legend.position, "none")

  combined <- suppressWarnings(suppressMessages(
    plot_species_map(range_map, species = keys)
  ))
  expect_s3_class(combined, "patchwork")
})

test_that("plot.cum_richness passes envelopecolour through to plot_ts", {
  captured <- NULL
  local_mocked_bindings(plot_ts = function(...) {
    captured <<- list(...)
    invisible(NULL)
  })
  cr <- structure(
    list(data = data.frame(year = 2000:2002, diversity_val = 1:3)),
    class = c("indicator_ts", "cum_richness")
  )
  plot(cr, envelopecolour = "pink")
  expect_equal(captured$envelopecolour, "pink")
  expect_false(captured$smoothed_trend)

  # smoothed_trend can be overridden without a duplicate-argument error
  plot(cr, smoothed_trend = TRUE)
  expect_true(captured$smoothed_trend)
})

test_that("plot.occ_turnover warns that auccolour is unsupported", {
  captured <- NULL
  local_mocked_bindings(plot_ts = function(...) {
    captured <<- list(...)
    invisible(NULL)
  })
  ot <- structure(
    list(data = data.frame(year = 2000:2002, diversity_val = c(0.1, 0.2, 0.3))),
    class = c("indicator_ts", "occ_turnover")
  )
  expect_warning(plot(ot, auccolour = "red"), "auccolour")
  expect_null(captured$auccolour)
  expect_no_warning(plot(ot))
})

test_that("plot_mv passes ... to mapview::mapview()", {
  skip_if_not_installed("mapview")
  skip_if_not_installed("RColorBrewer")
  captured <- NULL
  local_mocked_bindings(
    call_mapview = function(...) {
      captured <<- list(...)
      invisible(NULL)
    }
  )
  plot_mv(example_indicator_map1, legend = FALSE)
  expect_false(captured$legend)
})
