#' @export
calc_ts <- function(x, ...) {
  UseMethod("calc_ts")
}

#' @export
calc_map <- function(x, ...) {
  UseMethod("calc_map")
}

#' @export
getcubeinfo <- function(x) {
  UseMethod("getcubeinfo")
}

#' @export
print.indicator_ts <- function(x, n = 10, ...) {
  cat("Biodiversity indicator trend\n\n")
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

