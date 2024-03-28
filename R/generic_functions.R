#' @title Calculate Biodiversity Indicators Over Time
#'
#' @description This function provides a flexible framework for calculating various biodiversity
#' indicators over time. It prepares the data, creates a grid, calculates indicators,
#' and formats the output into an appropriate S3 object ('indicator_ts').
#' Specific implementations for different indicator types are provided using the
#' appropriate wrappers.
#'
#' @param x A data cube object ('processed_cube').
#' @param type The indicator to calculate. Supported options include:
#'   * 'hill0', 'hill1', 'hill2': Hill numbers (order 0, 1, and 2).
#'   * 'obs_richness': Observed species richness.
#'   * 'cum_richness': Cumulative species richness.
#'   * 'total_occ': Total number of occurrences.
#'   * 'newness': Mean year of occurrence.
#'   * 'occ_density': Density of occurrences.
#'   * 'williams_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity scores.
#'   * 'spec_occ': Species occurrences.
#'   * 'tax_distinct': Taxonomic distinctness.
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param ... Additional arguments passed to specific indicator calculation functions.
#'
#' @return An S3 object of the class 'indicator_ts' containing the calculated indicator values and metadata.
#'
#' @examples
#' occurrence_density_trend <- occ_density_ts(example_cube_1)
#' plot(occurrence_density_trend, min_year=1980)
#'
#' @export
calc_ts <- function(x, ...) {
  UseMethod("calc_ts")
}


#' @title Calculate Biodiversity Indicators Over Space
#'
#' @description This function provides a flexible framework for calculating various biodiversity
#' indicators on a spatial grid or as a time series. It prepares the data, creates a grid, calculates indicators,
#' and formats the output into an appropriate S3 object ('indicator_map'). Specific implementations
#' for different indicator types are provided using the appropriate wrappers.
#'
#' @param x A data cube object ('processed_cube').
#' @param type The indicator to calculate. Supported options include:
#'   * 'hill0', 'hill1', 'hill2': Hill numbers (order 0, 1, and 2).
#'   * 'obs_richness': Observed species richness.
#'   * 'total_occ': Total number of occurrences.
#'   * 'newness': Mean year of occurrence.
#'   * 'occ_density': Density of occurrences.
#'   * 'williams_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity scores.
#'   * 'spec_occ': Species occurrences.
#'   * 'tax_distinct': Taxonomic distinctness.
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param ... Additional arguments passed to specific indicator calculation functions.
#'
#' @return An S3 object of the class 'indicator_map' containing the calculated indicator values and metadata.
#'
#' @examples
#' observed_richness_map <- obs_richness_map(example_cube_2, level = "continent", region = "Europe")
#' plot(observed_richness_map)
#'
#' @export
calc_map <- function(x, ...) {
  UseMethod("calc_map")
}
