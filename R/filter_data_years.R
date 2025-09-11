#' @noRd
filter_data_years <- function(x, min_year, max_year) {

  # Check input
  if (!is.null(min_year) && !is.numeric(min_year)) {
    stop("min_year must be numeric.")
  }
  if (!is.null(max_year) && !is.numeric(max_year)) {
    stop("max_year must be numeric.")
  }
  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    stop("min_year cannot be greater than max_year.")
  }

  # Set min and max year using the 'rlang::%||%' operator
  min_year <- min_year %||% x$first_year
  max_year <- max_year %||% x$last_year

  # Filter data
  x$data <- dplyr::filter(x$data, year >= min_year & year <= max_year)

  if (length(x$data$year) < 1) {
    stop("No data available for the selected years. Please check your input.")
  }

  # Return list with updated data and years
  return(list(data = x$data, min_year = min_year, max_year = max_year))

}
