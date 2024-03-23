#' Retrieve Biodiversity Indicator Name
#'
#' Returns the full name (human-readable) of a biodiversity indicator, provided
#' its corresponding code.
#'
#' @param x A character string representing the biodiversity indicator code
#'   (e.g., "obs_richness", "hill1", "tax_distinct").
#' @return A character string containing the full descriptive name of the
#'   biodiversity indicator. If no match is found, returns an error.
#'
#' @examples
#' # Get the name for the code "obs_richness"
#' indicator_name <- get_divname("obs_richness")
#' indicator_name  # Output: "Observed Species Richness"
#' @noRd
get_indicator_name <- function(x) {

  iname <- as.character(available_indicators[[x]]$indicator_name)

  if (!length(iname) > 0) {
    stop("Indicator class is not registered. Check that you typed it correctly")
  } else {
    iname
  }

}

#' Retrieve Legend Title for Biodiversity Indicator
#'
#' Provides an appropriate legend title for a biodiversity indicator,
#' suitable for use with time series plots ("ts") or maps.
#'
#' @param x A character string representing the biodiversity indicator code
#'   (e.g., "obs_richness", "hill1", "tax_distinct").
#' @param type Specifies the visualization type: "ts" (time series) or "map".
#' @return A character string containing the legend title. Throws an error if
#'   the specified `div_type` is not found.
#'
#' @examples
#' # Get legend title for "tax_distinct" on a map
#' map_legend <- get_legend_title("tax_distinct", type = "map")
#'
#' # Get legend title for "hill2" on a time series plot
#' ts_legend <- get_legend_title("hill2", type = "ts")
#' @noRd
get_legend_title <- function(x) {

  ltitle <- as.character(available_indicators[[x]]$legend_label)

  if (!length(ltitle) > 0) {
    stop("Indicator class is not registered. Check that you typed it correctly.")
  } else {
    ltitle
  }

}

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


#' Extract Species Names.
#'
#' Retrieves a list of species names from a biodiversity data object
#'   ('processed_cube', 'indicator_map' or 'indicator_ts').
#'
#' @param object A biodiversity data object containing species names, either as
#'   a separate vector called species_names or as a column called scientificName.
#' @return  Either a character vector of species names (if directly available)
#'   or a data frame with columns:
#'   * `taxonKey`: A unique identifier for each species.
#'   * `scientificName`:  The scientific name of each species.
#'
#' @examples
#' # Assuming you have a biodiversity data cube named 'mammals_cube'
#' species_list <- list_species(mammals_cube)
#' @export
list_species <- function(object) {

  if(length(object$species_names) > 0) {

    return(object$species_names)

  } else {

    species_df <-
      object$data %>%
      dplyr::select(taxonKey, scientificName) %>%
      dplyr::distinct(taxonKey, scientificName)

    return(species_df)

  }
}

