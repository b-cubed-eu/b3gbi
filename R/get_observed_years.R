#' Extract Years With Observations from an Indicator Map
#'
#' Takes an "indicator_map" object and determines the years for which
#' observation data exists.
#'
#' @param x An "indicator_map" object containing calculated indicator values
#'   associated with grid cells.
#' @return A data frame with two columns:
#'   * `years`: A sequence of years covering the range of observations.
#'   * `occurrences`: A logical vector indicating if observations exist
#'      for each year (`TRUE` if present, `FALSE` if absent).
#'
#' @examples
#' # Assuming you have an 'indicator_map' object named 'my_indicator'
#' observed_data <- get_observed_years(my_indicator)
#' @export
get_observed_years <- function(x) {
  stopifnot(inherits(x, "indicator_map"))
  stopifnot("years_with_obs" %in% names(x))
  obs <- x$years_with_obs
  years <- min(x$years_with_obs):max(x$years_with_obs)
  observed <- ifelse(years %in% obs, TRUE, FALSE)
  data.frame("years" = years,
             "occurrences" = observed)

}
