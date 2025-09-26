#' @title 'indicator_ts' S3 Constructor
#'
#' @description This function creates an 'indicator_ts' S3 object, a
#' specialized structure for storing biodiversity indicator results calculated
#' over time. These objects include essential metadata and the calculated
#' indicator time series.
#'
#' @param x A tibble data frame containing at least two columns:
#'   * 'year'
#'   * 'diversity_val' (calculated indicator value)
#' @param div_type The type of biodiversity indicator in short form (e.g.,
#'  "obs_richness").
#' @param kingdoms A character vector of kingdoms included in the analysis.
#' @param num_species The total number of species in the dataset.
#' @param num_years The number of years in the time series.
#' @param species_names A character vector of species names, if a per-species
#'  indicator was calculated.
#' @param map_level The spatial level of the data (e.g., "country", "continent",
#'  "world").
#' @param map_region The name of the spatial region under analysis.
#' @param coord_range A named list specifying the coordinate range of the
#'  analysis (elements: xmin, xmax, ymin, ymax).
#'
#' @return An 'indicator_ts' S3 object containing:
#'   * **Indicator name and type:**  Descriptive name of indicator (e.g.,
#'     "Observed Species Richness") and short-form (e.g., "obs_richness").
#'   * **Data timeframe:** First year, last year, number of years.
#'   * **Spatial details:** Level (country, continent, world), region name, and
#'     coordinate range.
#'   * **Diversity information:**  Kingdoms, names of species, number of
#'     species.
#'   * **Time Series:** The input tibble containing year and indicator values.
#'
#' @noRd
new_indicator_ts <- function(x,
                             div_type,
                             map_level,
                             map_region,
                             map_type,
                             kingdoms,
                             num_families,
                             num_species,
                             num_years,
                             species_names,
                             coord_range) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "diversity_val") %in% names(x)))
  id <- div_type
  class(x) <- c("indicator_data", class(x))
  structure(list(
    div_name = get_indicator_name(id),
    div_type = div_type,
    first_year = min(x$year),
    last_year = max(x$year),
    num_years = num_years,
    num_species = num_species,
    map_level = map_level,
    map_region = map_region,
    map_type = map_type,
    kingdoms = kingdoms,
    num_families = num_families,
    coord_range = coord_range,
    species_names = species_names,
    data = x
  ),
  class = c("indicator_ts", div_type),
  indicator_id = id,
  type = "ts")
}
