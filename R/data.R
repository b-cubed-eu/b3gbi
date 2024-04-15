#' Cube of GBIF Mammal Occurrences in Denmark
#'
#' Small example cube containing the mammal occurrences in Denmark available on GBIF as of 16.03.2024.
#'
#' @format A 'processed_cube' object containing a tibble with 276,950 rows and 10 variables, as well as metadata
#' \describe{
#'   \item{year}{year occurrence was recorded}
#'   \item{eea_cell_code}{code containing the cell resolution and coordinates on the EEA grid}
#'   \item{taxonKey}{taxonomic key associated with the species on GBIF}
#'   \item{obs}{number of individuals observed}
#'   \item{scientificName}{scientific species name}
#'   \item{rank}{taxonomic rank}
#'   \item{kingdom}{kingdom}
#'   \item{xcoord}{East-West coordinate on the EEA grid}
#'   \item{ycoord}{North-South coordinate on the EEA grid}
#'   \item{resolution}{grid cell size}
#' }
#' @examples{
#'   \dontrun{
#'     # Basic example of how to use the dataset
#'     denmark_mammals_or_map <- obs_richness_map(example_cube_1,
#'                                                level = "country",
#'                                                region = "Denmark")
#'     plot(denmark_mammals_or_map,
#'          title = "Mammals in Denmark (1751-2023): Observed Species Richness")
#'   }
#' }
#' @source \url{https://doi.org/10.15468/dl.5mb887}
"example_cube_1"

#' Cube of GBIF Insect Occurrences in Europe
#'
#' Small example cube containing the insect occurrences in Europe available on GBIF as of 16.03.2024.
#'
#' @format A 'processed_cube' object containing a tibble with 23,025 rows and 10 variables, as well as metadata
#' \describe{
#'   \item{year}{year occurrence was recorded}
#'   \item{eea_cell_code}{code containing the cell resolution and coordinates on the EEA grid}
#'   \item{taxonKey}{taxonomic key associated with the species on GBIF}
#'   \item{obs}{number of individuals observed}
#'   \item{scientificName}{scientific species name}
#'   \item{rank}{taxonomic rank}
#'   \item{kingdom}{kingdom}
#'   \item{xcoord}{East-West coordinate on the EEA grid}
#'   \item{ycoord}{North-South coordinate on the EEA grid}
#'   \item{resolution}{grid cell size}
#' }
#' @examples{
#'     \dontrun{
#'       # Basic example of how to use the dataset
#'       europe_insects_or_map <- obs_richness_map(example_cube_2,
#'                                                 level = "continent",
#'                                                 region = "Europe")
#'       plot(europe_insects_or_map,
#'            title = "Insects in Europe (1751-2023): Observed Species Richness")
#'     }
#'   }
#' @source \url{https://doi.org/10.15468/dl.qk4f2z}
"example_cube_2"




#' Time Series of Observed Species Richness for Mammals in Denmark
#'
#' Example indicator containing a time series of observed species richness for mammal occurrences in Denmark (occurrences from GBIF: 16.03.2024).
#'
#' @format An 'indicator_ts' object containing a tibble with 54 rows and 2 variables, as well as metadata
#' \describe{
#'   \item{year}{a year the indicator was calculated for}
#'   \item{diversity_val}{calculated richness value for the year}
#' }
#' @source \url{https://doi.org/10.15468/dl.qk4f2z}
"example_indicator_ts1"

#' Time Series of Cumulative Species Richness for Insects in Europe
#'
#' Example indicator containing a time series of cumulative species richness for insect occurrences in Europe (occurrences from GBIF: 16.03.2024).
#'
#' @format An 'indicator_ts' object containing a tibble with 89 rows and 2 variables, as well as metadata
#' \describe{
#'   \item{year}{a year the indicator was calculated for}
#'   \item{diversity_val}{calculated richness value for the year}
#' }
#' @source \url{https://doi.org/10.15468/dl.qk4f2z}
"example_indicator_ts2"

#' Indicators Available for Use in the Package
#'
#' A list of all biodiversity indicators available within the package, along with the dimensions they can be calculated across, the functions to access them, and any special arguments
#'
#' @format A special object of class 'available_indicators' containing a list of indicators and six fields with information about them
#' \describe{
#'   \item{indicator_class}{class of the indicator}
#'   \item{indicator_name}{name of the indicator}
#'   \item{plot_title}{title to be used when plotting with automated title generation}
#'   \item{legend_label}{title to be used when plotting with automated legend title generation}
#'   \item{legend_transformation}{any transformation to perform on the legend when plotting, to improve visualization of maps}
#'   \item{map_wrapper}{wrapper function to use when calculating indicator as a map}
#'   \item{ts_wrapper}{wrapper function to use when calculating indicator as a time series}
#'   \item{map_function_arguments}{any special arguments to consider when using the function to calculate an indicator map}
#'   \item{ts_function_arguments}{any special arguments to consider when using the function to calculate an indicator time series}
#' }
#' @source N/A
"available_indicators"
