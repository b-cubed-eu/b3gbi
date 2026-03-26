#' @title Plot Interactive Biodiversity Indicator Map
#'
#' @description Creates an interactive map of a calculated multi-species
#'  biodiversity indicator. Requires an indicator_map object as input.
#'
#' @param x An 'indicator_map' object containing multi-species indicator values
#'  associated with map grid cells. This is a required parameter with no
#'  default.
#' @param legend_title (Optional) Legend title. Add your own custom legend title
#'  if you don't like the automatically generated one. If set to the default of
#'  NULL, the function will provide a legend title based on the indicator being
#'  plotted.
#' @param transparency (Optional) Change the transparency of the grid fill.
#'  Default is 0.8. A value of 0 is full transparency, 1 is solid fill.
#' @param filter_NA (Optional) Filter out NA values so NA grid cells do not
#'  appear. Default is TRUE.
#' @param basemap_list (Optional) A list of basemaps you want mapview to display
#'  as options to select from. The first one in the list will be shown when the
#'  map loads. Default is c("OpenStreetMap", "OpenTopoMap", "CartoDB.Positron",
#'  "Esri.WorldImagery").
#' @param ... (Optional) Pass additional parameters to the mapview package.
#'
#' @return An interactive mapview plot.
#'
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
plot_mv <- function(x,
                    legend_title = NULL,
                    transparency = 0.8,
                    filter_NA = TRUE,
                    basemap_list = c("OpenStreetMap",
                                     "OpenTopoMap",
                                     "CartoDB.Positron",
                                     "Esri.WorldImagery"),
                    ...
                    ){

  # Check if the mapview package is installed
  if (!requireNamespace("mapview", quietly = TRUE)) {
    # If not installed, stop and tell the user what to do
    stop("The 'mapview' package is required for this function.
         Please install it using: install.packages('mapview')",
         call. = FALSE)
  }

  # Check if the RColorBrewer package is installed
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    # If not installed, stop and tell the user what to do
    stop("The 'RColorBrewer' package is required for this function.
         Please install it using: install.packages('mapview')",
         call. = FALSE)
  }

  indicator_value <- NULL

  # Check that the object is the correct class
  wrong_class(x, "indicator_map", reason = "incorrect")

  # Set basemaps to display as options in mapview
  mapview::mapviewOptions(basemaps = c(basemap_list))

  # Get geometry and indicator values
  mv_geometry <- x$data$geometry
  mv_values <- x$data$diversity_val

  # Create a new sf object
  indicator_sf <- sf::st_sf(
    # 1. Provide indicator data as a column
    indicator_value = mv_values,

    # 2. Provide the geometry list as a column
    geometry = mv_geometry
  )

  # Filter out NAs
  if (filter_NA == TRUE) {
    indicator_sf <-indicator_sf %>%
      dplyr::filter(!is.na(indicator_value))
  }

  # Custom legend title if provided
  legend_title <- legend_title %||%
    available_indicators[[x$div_type]]$legend_label

  # Define the Yellow-to-Red color ramp function
  yellow_to_red_pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))

  # Launch mapview
  mapview::mapview(
    indicator_sf,
    zcol = "indicator_value",
    layer.name = legend_title,
    alpha.regions = transparency, # Ensure the polygons are visible
    color = "transparent",
    col.regions = yellow_to_red_pal
  )

}

