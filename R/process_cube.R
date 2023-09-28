# Function to load a GBIF data cube and its associated taxonomic information.
# Both files must be in the same directory, and have the same basic name with
# different endings (_cube.csv and _info.csv).
# The arguments first_year and final_year can be used to limit the observation
# years of your analysis dataset to a smaller range than the cube itself.
# To use the entire range of the cube, leave the arguments out of the function
# call.

#' Process a GBIF data cube
#'
#' @param cube_name The location and name of a data cube file to open.
#' @param tax_info The location and name of an associated taxonomic info file.
#' @param first_year The first year of occurrences (if different from cube).
#' @param final_year The final year of occurrences (if different from cube).
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' library("readr")
#' library("dplyr")
#'
#' cube_name <- "eu_modellingtaxa_cube.csv"
#' tax_info <- "eu_modellingtaxa_info.csv"
#' process_cube(cube_name, tax_info, 1970, 2010)
#'
#' process_cube(cube_name, tax_info)
#'
process_cube <- function(cube_name, tax_info, first_year = NA, final_year = NA) {

  # Read in data cube
  occurrence_data <- readr::read_csv(
    file = cube_name,
    col_types = cols(
      year = col_double(),
      eea_cell_code = col_character(),
      taxonKey = col_double(),
      n = col_double(),
      min_coord_uncertainty = col_double()
    ),
    na = ""
  )

  # Read in associated taxonomic info
  taxonomic_info <- readr::read_csv(
    file = tax_info,
    col_types = readr::cols(
      taxonKey = readr::col_double(),
      scientificName = readr::col_character(),
      rank = readr::col_factor(),
      taxonomicStatus = readr::col_factor(),
      kingdom = readr::col_factor(),
      includes = readr::col_character()
    ),
    na = ""
  )

  # Merge the two data frames together on 'taxonKey'
  merged_data <- dplyr::left_join(occurrence_data, taxonomic_info, by = "taxonKey")

  # Separate 'eea_cell_code' into resolution, coordinates
  merged_data <- merged_data %>%
    dplyr::mutate(
      xcoord = as.numeric(str_extract(eea_cell_code, "(?<=E)\\d+")),
      ycoord = as.numeric(str_extract(eea_cell_code, "(?<=N)\\d+")),
      resolution = str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
    )

  # Remove columns that are not needed
  merged_data <-
    merged_data %>%
    dplyr::select(-min_coord_uncertainty, -taxonomicStatus, -includes)

  # Rename column n to obs
  merged_data <-
    merged_data %>%
    dplyr::rename(obs = n)

  # Check whether start and end years are within dataset
  first_year <- merged_data %>%
    dplyr::select(year) %>%
    min() %>%
    ifelse(is.na(first_year),
           .,
           ifelse(first_year > ., first_year, .))
  final_year <- merged_data %>%
    dplyr::select(year) %>%
    max() - 1 %>%
    ifelse(is.na(final_year),
           .,
           ifelse(final_year < ., final_year, .))

  # Limit data set
  merged_data <-
    merged_data %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= final_year)

  merged_data

}
