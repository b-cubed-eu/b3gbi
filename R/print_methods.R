#' @title Print an Indicator Time Series Object
#'
#' @description Provides a summary representation of an indicator_ts object,
#'   designed for user-friendly display in the console.
#'
#' @method print indicator_ts
#'
#' @param x An indicator_ts object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @examples
#' print(example_indicator_ts1)
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
  if (!is.null(x$num_families)) {cat("Number of families represented:",
                                     paste(x$num_families, collapse = ", "), "\n\n")}
  if (!is.null(x$kingdoms)) {cat("Kingdoms represented:", x$kingdoms, "\n")}
 # if (!is.null(x$families)) {cat("Families represented:", x$families, "\n")}
  cat("\nFirst", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print an Indicator Map Object
#'
#' @description Provides a summary representation of an indicator_map object,
#'   designed for user-friendly display in the console.
#'
#' @method print indicator_map
#'
#' @param x An indicator_map object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @examples
#' library(sf)
#' obs_rich_map <- obs_richness_map(example_cube_1, spherical_geometry = FALSE)
#' print(obs_rich_map)
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
  cat("Projected CRS:", x$projection, "\n\n")
  cat("Coordinate range:\n")
  print(x$coord_range)
  cat("\nGrid cell size:", x$cell_size, "\n")
  cat("Number of cells:", x$num_cells, "\n\n")
  cat("Observation years:", x$first_year, "-", x$last_year, "\n")
  cat("Total years with observations:", x$num_years, "\n\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Number of families represented:", paste(x$num_families, collapse = ", "), "\n\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  tibble::as_tibble(x$data) %>%
    dplyr::select(-geometry) %>%
    print(n = n, ...)
}

#' @title Print a Processed Data Cube Object
#'
#' @description Provides a summary representation of a processed_cube object,
#'   designed for user-friendly display in the console.
#'
#' @method print processed_cube
#'
#' @param x A processed_cube object.
#' @param n Integer specifying the number of rows of cube data to display.
#' @param ... Additional arguments.
#'
#' @examples
#' print(example_cube_1)
#'
#' @export
print.processed_cube <- function(x, n = 10, ...) {
  cat("\nProcessed data cube for calculating biodiversity indicators\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n")
  cat("Single-resolution cube with cell size", x$resolutions, "^2\n")
  cat("Number of cells:", x$num_cells, "\n")
  cat("Grid reference system:", x$grid_type, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Number of families represented:", paste(x$num_families, collapse = ", "), "\n\n")
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
#'
#' @param x A processed_cube_dsinfo object.
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.processed_cube_dsinfo <- function(x, n = 10, ...) {
  cat("\nProcessed data cube for calculating biodiversity indicators.\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n")
  cat("Single-resolution cube with cell size", paste(x$resolutions, "^2\n", sep = ""))
  cat("Number of cells:", x$num_cells, "\n")
  cat("Grid reference system:", x$grid_type, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Number of families represented:", paste(x$num_families, collapse = ", "), "\n\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("Number of datasets represented:", x$num_datasets, "\n")
  cat("Record types represented:", paste(x$record_types, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print a Simulated Data Cube Object
#'
#' @description Provides a summary representation of a sim_cube object,
#'   designed for user-friendly display in the console.
#'
#' @method print sim_cube
#'
#' @param x A sim_cube object.
#' @param n Integer specifying the number of rows of cube data to display.
#' @param ... Additional arguments.
#'
#'
#' @export
print.sim_cube <- function(x, n = 10, ...) {
  cat("\nSimulated data cube for calculating biodiversity indicators\n\n")
  cat("Date Range:", x$first_year, "-", x$last_year, "\n")
  if (!is.null(x$resolutions)) {
    cat("Single-resolution cube with cell size", x$resolutions, "^2\n")
  }
  cat("Number of cells:", x$num_cells, "\n")
  cat("Grid reference system:", x$grid_type, "\n")
  cat("Coordinate range:\n")
  print(unlist(x$coord_range))
  cat("\nTotal number of observations:", x$num_obs, "\n")
  cat("Number of species represented:", x$num_species, "\n")
  cat("Number of families represented:", paste(x$num_families, collapse = ", "), "\n\n")
  cat("Kingdoms represented:", paste(x$kingdoms, collapse = ", "), "\n\n")
  cat("First", n, "rows of data (use n = to show more):\n\n")
  print(x$data, n = n, ...)
}

#' @title Print Available Indicators
#' @description Provide a summary of indicators registered in the package.
#'
#' @method print available_indicators
#'
#' @param x Object of class available_indicators
#' @param n Integer specifying the number of rows of data to display.
#' @param ... Additional arguments.
#'
#' @export
print.available_indicators <- function(x, n = 30, ...) {
  if (n > length(names(x))) n <- length(names(x))
  cat("\n\nAvailable Indicators\n")
  for (i in 1:n) {
    cat(paste0("\n\n", i, ". \033[0;31m", x[[i]]$indicator_name, "\033[0m"), sep="")
    cat("\n     Class: ", x[[i]]$indicator_class, sep="")
    if (!is.null(x[[i]]$map_wrapper)) {
      cat("\n     Calculate map: yes, e.g. ", x[[i]]$map_wrapper, "(my_data_cube)", sep="")
    } else {
      cat("\n     Calculate map: no", sep="")
    }
    if (!is.null(x[[i]]$ts_wrapper)) {
      cat("\n     Calculate time series: yes, e.g. ", x[[i]]$ts_wrapper, "(my_data_cube)", sep="")
    } else {
      cat("\n     Calculate time series: no", sep="")
    }
    if (!is.null(x[[i]]$map_function_arguments)){
      cat("\n     Additional map function arguments: ", paste(x[[i]]$map_function_arguments, collapse= ", "), sep="")
    } else {
      cat("\n     Additional map function arguments: none", sep="")
    }
    if (!is.null(x[[i]]$ts_function_arguments)){
      cat("\n     Additional time series function arguments: ", paste(x[[i]]$ts_function_arguments, collapse = ", "), sep="")
    } else {
      cat("\n     Additional time series function arguments: none", sep="")
    }
  }
  if (n < length(names(x))) {
    cat("\n*Note: There are ", length(names(x)) - n,
        " additional indicator(s). To see more, increase n (e.g. n = ",
        length(names(x)), ").", sep = "")
  } else {
    cat("\n")
  }
}
