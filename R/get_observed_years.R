#' Extract Years With Observations from an Indicator Map
#'
#' Takes an "indicator_map",  "indicator_ts", or "processed_cube" object and
#' determines the years for which observation data exists.
#'
#' @param x An "indicator_map" object containing calculated indicator values
#'  associated with grid cells.
#' @return A data frame with two columns:
#'  * `years`: A sequence of years covering the range of observations.
#'  * `occurrences`: A logical vector indicating if observations exist
#'    for each year (`TRUE` if present, `FALSE` if absent).
#'
#' @examples
#' total_occ_mammals_denmark <- total_occ_map(example_cube_1, level = "country",
#'                                            region = "Denmark")
#' get_observed_years(total_occ_mammals_denmark)
#' @export
get_observed_years <- function(x) {

  if (inherits(x, "indicator_map")) {
    obs <- x$years_with_obs
    years <- min(x$years_with_obs):max(x$years_with_obs)
  } else if (inherits(x, "indicator_ts") || inherits(x, "processed_cube")) {
    obs <- unique(x$data$year)
    years <- min(obs):max(obs)
  }

  observed <- ifelse(years %in% obs, TRUE, FALSE)
  df <- data.frame("years" = years,
                   "occurrences" = observed)

  return(df)

}
