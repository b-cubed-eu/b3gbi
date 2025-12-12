#' Cube of GBIF Mammal Occurrences in Denmark
#'
#' Small example cube containing the mammal occurrences in Denmark available on
#' GBIF as of 16.03.2024.
#'
#' @format A 'processed_cube' object containing a tibble with 31,632 rows and
#'  15 variables, as well as metadata
#' \describe{
#'   \item{year}{year occurrence was recorded}
#'   \item{cellCode}{code containing the cell resolution and coordinates
#'    on the Extended Quarter Degree (eqdgc) grid}
#'   \item{kingdomKey}{kingdom key associated with the species on GBIF}
#'   \item{kingdom}{kingdom name}
#'   \item{familyKey}{family key associated with the species on GBIF}
#'   \item{family}{family name}
#'   \item{taxonKey}{taxonomic key associated with the species on GBIF}
#'   \item{scientificName}{scientific species name}
#'   \item{obs}{number of individuals observed}
#'   \item{minCoordinateUncertaintyInMeters}{minimum coordinate uncertainty in
#'    meters}
#'   \item{minTemporalUncertainty}{minimum temporal uncertainty in seconds}
#'   \item{xcoord}{East-West coordinate on the eqdgc grid}
#'   \item{ycoord}{North-South coordinate on the eqdgc grid}
#'   \item{resolution}{grid cell size}
#' }
#' @examples{
#'   \dontrun{
#'     # Basic example of how to use the dataset
#'     denmark_mammals_or_map <- obs_richness_map(example_cube_1,
#'                                                level = "country",
#'                                                region = "Denmark")
#'     plot(denmark_mammals_or_map,
#'          title = "Mammals in Denmark (1862-2024): Observed Species Richness")
#'   }
#' }
#' @source \url{https://doi.org/10.15468/dl.5mb887}
"example_cube_1"


#' Time Series of Observed Species Richness for Mammals in Denmark
#'
#' Example indicator containing a time series of observed species richness for
#' mammal occurrences in Denmark, 1970-2024 (occurrences from GBIF: 16.03.2024).
#'
#' @format An 'indicator_ts' object containing a tibble with 55 rows and 2
#'  variables, as well as metadata
#' \describe{
#'   \item{year}{a year the indicator was calculated for}
#'   \item{diversity_val}{calculated richness value for the year}
#' }ed
#' @source \url{https://doi.org/10.15468/dl.5mb887}
"example_indicator_ts1"

#' Time Series of Cumulative Species Richness for Bryophytes in South Africa
#'
#' Example indicator containing a time series of cumulative species richness for
#' bryophyte occurrences in South Africa, 1875-2024 (occurrences from GBIF:
#' 16.03.2024).
#'
#' @format An 'indicator_ts' object containing a tibble with 110 rows and 2
#'  variables, as well as metadata
#' \describe{
#'   \item{year}{a year the indicator was calculated for}
#'   \item{diversity_val}{calculated richness value for the year}
#' }
#' @source \url{https://doi.org/10.15468/dl.yfzgja}
"example_indicator_ts2"

#' Map of Observed Species Richness for Mammals in Denmark
#'
#' Example indicator containing a map of observed species richness for mammal
#' occurrences in Denmark (occurrences from GBIF: 16.03.2024).
#'
#' @format An 'indicator_map' object containing a tibble with 800 rows and 5
#'  variables, as well as metadata
#' \describe{
#'  \item{cellid}{id of a map cell the indicator was calculated for}
#'  \item{area}{area of the map cell in square kilometers}
#'  \item{cellCode}{code containing the cell coordinates in Extended Quarter
#'   Degree (eqdgc) grid format}
#'  \item{diversity_val}{calculated richness value for the cell}
#'  \item{geometry}{geometry of the map cell}
#' }
#' @source \url{https://doi.org/10.15468/dl.5mb887}
"example_indicator_map1"

#' Indicators Available for Use in the Package
#'
#' A list of all biodiversity indicators available within the package, along
#' with the dimensions they can be calculated across, the functions to access
#' them, and any special arguments
#'
#' @format A special object of class 'available_indicators' containing a list
#'  of indicators and six fields with information about them
#' \describe{
#'   \item{indicator_class}{class of the indicator}
#'   \item{indicator_name}{name of the indicator}
#'   \item{plot_title}{title to be used when plotting with automated title
#'    generation}
#'   \item{legend_label}{title to be used when plotting with automated legend
#'    title generation}
#'   \item{legend_transformation}{any transformation to perform on the legend
#'    when plotting, to improve visualization of maps}
#'   \item{map_wrapper}{wrapper function to use when calculating indicator as a
#'    map}
#'   \item{ts_wrapper}{wrapper function to use when calculating indicator as a
#'    time series}
#'   \item{map_function_arguments}{any special arguments to consider when using
#'    the function to calculate an indicator map}
#'   \item{ts_function_arguments}{any special arguments to consider when using
#'    the function to calculate an indicator time series}
#' }
#' @source N/A
"available_indicators"
