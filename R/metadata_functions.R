#' Retrieve Biodiversity Indicator Name
#'
#' Returns the full name (human-readable) of a biodiversity indicator, provided
#' its corresponding code.
#'
#' @param x A character string representing the biodiversity indicator class
#'  (e.g., "obs_richness", "hill1", "cum_richness").
#' @return A character string containing the full descriptive name of the
#'  biodiversity indicator. Returns an error if the specified indicator class
#'  is not found.
#' @noRd
get_indicator_name <- function(x) {

 # available_indicators <- NULL; rm(available_indicators)

  iname <- as.character(available_indicators[[x]]$indicator_name)

  if (!length(iname) > 0) {

    stop("Indicator class is not registered. Check that you typed it correctly")

  } else {

    iname

  }

  return(iname)

}

#' Retrieve Legend Title for Biodiversity Indicator
#'
#' Provides an appropriate legend title for a biodiversity indicator,
#' suitable for use with time series plots ("ts") or maps.
#'
#' @param x A character string representing the biodiversity indicator class
#'  (e.g., "obs_richness", "hill1", "cum_richness").
#' @return A character string containing the legend title. Throws an error if
#'  the specified indicator class is not found.
#' @noRd
get_legend_title <- function(x) {

 # available_indicators <- NULL; rm(available_indicators)

  ltitle <- as.character(available_indicators[[x]]$legend_label)

  if (!length(ltitle) > 0) {

    stop(
      "Indicator class is not registered. Check that you typed it correctly."
    )

  } else {

    ltitle

  }

  return(ltitle)

}

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


#' Extract Species Names.
#'
#' Retrieves a list of species names from a biodiversity data object
#'   ('processed_cube', 'indicator_map' or 'indicator_ts').
#'
#' @param object A biodiversity data object containing species names, either as
#'  a separate vector called species_names or as a column called scientificName.
#' @return  Either a character vector of species names (if directly available)
#'  or a data frame with columns:
#'  * `taxonKey`: A unique identifier for each species.
#'  * `scientificName`:  The scientific name of each species.
#'
#' @examples
#' list_species(example_cube_1)
#' @export
list_species <- function(object) {

  taxonKey <- scientificName <- NULL

  if (length(object$species_names) > 0) {

    return(object$species_names)

  } else {

    species_df <-
      object$data %>%
      dplyr::select(taxonKey, scientificName) %>%
      dplyr::distinct(taxonKey, scientificName)

    return(species_df)

  }
}
