#' Cube of GBIF Mammal Occurrences in Denmark
#'
#' Small example cube containing the mammal occurrences in Denmark favailable on GBIF as of 16.03.2024.
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
#' @source \url{https://doi.org/10.15468/dl.qk4f2z}
"example_cube_2"


#'
#' \name{example_cube_1}
#' \alias{example_cube_1}
#' \title{Cube of GBIF Mammal Occurrences in Denmark}
#' \description{
#'   This is a small example cube containing the mammal occurrences in Denmark available on GBIF as of 16.03.2024.
#' }
#' \format{
#'   Contains a tibble with the columns 'year', 'eea_cell_code', 'taxonKey', 'obs', 'scientificName', 'rank', 'kingdom', 'xcoord', 'ycoord', and 'resolution'. This can be directly accessed using example_cube_1$data. Also contains metadata, including the data range, resolution, number of grid cells, coordinate range, total number of observations, number of species represented, and kingdoms represented. Simply type example_cube_1 to see the metadata.
#' }
#' \source{
#'   The occurrences were downloaded from GBIF: https://doi.org/10.15468/dl.5mb887
#' }
#' \examples{
#'   \dontrun{
#'     # Basic example of how to use the dataset
#'     denmark_mammals_or_map <- obs_richness_map(example_cube_1, level = "country", region = "Denmark")
#'     plot(denmark_mammals_or_map, title = "Mammals in Denmark (1751-2023): Observed Species Richness")
#'   }
#' }
#'
#'
#'
#' \name{example_cube_2}
#' \alias{example_cube_2}
#' \title{Cube of GBIF Insect Occurrences in Europe}
#' \description{
#'   This is a small example cube containing the insect occurrences in Europe available on GBIF as of 16.03.2024.
#' }
#' \format{
#'   Contains a tibble with the columns 'year', 'eea_cell_code', 'taxonKey', 'obs', 'scientificName', 'rank', 'kingdom', 'xcoord', 'ycoord', and 'resolution'. This can be directly accessed using example_cube_1$data. Also contains metadata, including the data range, resolution, number of grid cells, coordinate range, total number of observations, number of species represented, and kingdoms represented. Simply type example_cube_2 to see the metadata.
#' }
#' \source{
#'   The occurrences were downloaded from GBIF: https://doi.org/10.15468/dl.qk4f2z
#'   \examples{
#'     \dontrun{
#'       # Basic example of how to use the dataset
#'       europe_insects_or_map <- obs_richness_map(example_cube_2, level = "continent", region = "Europe")
#'       plot(europe_insects_or_map, title = "Insects in Europe (1751-2023): Observed Species Richness")
#'     }
#'   }
#'
#'
#'
