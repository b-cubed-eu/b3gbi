#' @title Calculate a Biodiversity Indicator Time Series
#'
#' @description A generic function for calculating biodiversity indicator
#'   time series data. Specific implementations for different indicator types
#'   or calculation methods should be provided using S3 methods.
#'
#' @param x An object containing the necessary data and information for the
#'   time series calculation.
#' @param ... Additional arguments (potentially used by specific methods).
#'
#' @return An object representing the calculated biodiversity indicator time
#'   series. The format of the output will depend on the specific S3 method used.
#'
#' @method calc_ts default
#' @usage calc_ts(x)
#'
#' @export
calc_ts <- function(x, ...) {
  UseMethod("calc_ts")
}

#' @title Calculate a Biodiversity Indicator Map
#'
#' @description A generic function for calculating spatial biodiversity
#'   indicator maps. Specific implementations for different indicator types
#'   or mapping approaches should be provided using S3 methods.
#'
#' @param x An object containing the geospatial data and other information
#'   required for the map calculation.
#' @param ... Additional arguments (potentially used by specific methods).
#'
#' @return An object representing the calculated biodiversity indicator map.
#'   The format of the output will depend on the specific S3 method used.
#'
#' @method calc_map default
#' @usage calc_map(x)
#'
#' @export
calc_map <- function(x, ...) {
  UseMethod("calc_map")
}

#' @title Extract Information from a Data Cube Object
#'
#' @description A generic function to extract essential information from data
#'   cube objects. Specific implementations for different data cube classes
#'   should be provided using S3 methods.
#'
#' @param x An object representing a data cube.
#' @param ... Additional arguments (potentially used by specific methods).
#'
#' @return The type of information returned will depend on the specific S3
#'    method that is dispatched based on the class of 'x'.
#'
# #' @method getcubeinfo default
#' @usage getcubeinfo(x)
#'
#' @export
getcubeinfo <- function(x) {
  UseMethod("getcubeinfo")
}

#' @title Print an Indicator Time Series Object
#'
#' @description Provides a summary representation of an indicator_ts object,
#'   designed for user-friendly display in the console.
#'
#' @method print indicator_ts
#' @param x An indicator_ts object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.indicator_ts <- function(x, n = 10, ...) {
  cat("Biodiversity indicator time series\n\n")
  cat("Name of indicator:", x$div_name, "\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n\n")
  if (x$map_region!="unknown") cat("Region(s) represented:", x$map_region, "\n\n")
  cat("Coordinate range represented:\n")
  print(x$coord_range)
  cat("\nNumber of species represented:", x$num_species, "\n")
  cat("Kingdoms represented:", x$kingdoms, "\n")
  cat("\nFirst", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print an Indicator Map Object
#'
#' @description Provides a summary representation of an indicator_map object,
#'   designed for user-friendly display in the console.
#'
#' @method print indicator_map
#' @param x An indicator_map object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.indicator_map <- function(x, n = 10, ...) {
  cat("Gridded biodiversity indicator map\n\n")
  cat("Name of Indicator:", x$div_name, "\n\n")
  if (x$map_level == "continent" | x$map_level == "country") {
    cat("Map of", x$map_region, "\n\n")
  } else if (x$map_level == "world") {
    cat("World map\n\n")
  }
    cat("Projection:", x$projection, "\n\n")
  cat("Coordinate range:\n")
  print(x$coord_range)
  cat("\nGrid cell size:", x$cell_size, "\n")
  cat("Number of cells:", x$num_cells, "\n\n")
  cat("Observation years:", x$first_year, "-", x$last_year, "\n")
  cat("Total years with observations:", x$num_years, "\n\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  as_tibble(x$data) %>%
    select(-geometry) %>%
    print(n = n, ...)
}

#' @title Print a Processed Data Cube Object
#'
#' @description Provides a summary representation of a processed_cube object,
#'   designed for user-friendly display in the console.
#'
#' @method print processed_cube
#' @param x A processed_cube object.
#' @param n Integer specifying the number of rows of cube data to display.
#' @param ... Additional arguments.
#'
#' @export
print.processed_cube <- function(x, n = 10, ...) {
  cat("\nProcessed data cube for calculating biodiversity indicators\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n")
  if (x$multi_res==TRUE) {cat("Multi-resolution cube with cell sizes:", paste(x$resolutions, collapse = "^2, "), "^2\n")}
  else {cat("Single-resolution cube with cell size", x$resolutions, "^2\n")}
  cat("Number of cells:", x$num_cells, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print a Processed Data Cube Object
#'
#' @description Provides a summary representation of a processed_cube_dsinfo object,
#'   designed for user-friendly display in the console.
#'
#' @method print processed_cube_dsinfo
#' @param x A processed_cube_dsinfo object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.processed_cube_dsinfo <- function(x, n = 10, ...) {
  cat("\nProcessed data cube for calculating biodiversity indicators.\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n")
  if (x$multi_res==TRUE) {cat("Multi-resolution cube with cell sizes:", paste(x$resolutions, collapse = "^2, ", sep = ""), "^2\n")}
  else {cat("Single-resolution cube with cell size", paste(x$resolutions, "^2\n", sep = ""))}
  cat("Number of cells:", x$num_cells, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("Number of datasets represented:", x$num_datasets, "\n")
  cat("Record types represented:", paste(x$record_types, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print a Virtual Cube Object
#'
#' @description Provides a summary representation of a virtual_cube object,
#'   designed for user-friendly display in the console.
#'
#' @method print virtual_cube
#' @param x A virtual_cube object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.virtual_cube <- function(x, n = 5, ...) {
  cat("\nVirtual data cube for calculating biodiversity indicators\n\n")
  if (x$last_year - x$first_year == 0) {cat("*Note that this cube cannot be used to calculate indicator trends as there is only one year of data.\n\n")}
  else if (x$last_year - x$first_year > 0) {cat("Date Range:", x$first_year, "-", x$last_year, "\n")}
  cat("Number of cells:", x$num_cells, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("\nFirst and last", n, "rows of data (use n = to show more):\n\n")
  print(x$data, topn = n, ...)
}

