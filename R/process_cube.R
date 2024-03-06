#' @title Process GBIF Data Cubes
#'
#' @description Processes a GBIF data cube and associated taxonomic information
#'   file. All files must reside in the same directory and share a base file
#'   name (e.g., 'my_mammals_cube.csv', 'my_mammals_info.csv').
#'
#' @param cube_name The location and name of a data cube file
#'   (e.g., 'data/my_mammals_cube.csv').
#' @param tax_info The location and name of an associated taxonomic info file
#'   (e.g.,  'data/my_mammals_info.csv').
#' @param datasets_info The location and name of an associated dataset info file
#'   (e.g., 'data/my_mammals_datasets.csv').
#' @param first_year (Optional) The first year of occurrences to include. If not
#'   specified, uses the earliest year present in the cube.
#' @param final_year (Optional) The final year of occurrences to include. If not
#'   specified, uses the latest year present in the cube.
#'
#' @return A tibble containing the processed GBIF occurrence data.
#'
#' @examples
#' # Assuming files are located in the 'data' directory:
#' processed_mammals_cube <- process_cube(cube_name = "data/mammals_cube.csv",
#'                                        tax_info = "data/mammals_info.csv")
#'
#' @export
process_cube <- function(cube_name, tax_info, datasets_info = NA, first_year = NA, final_year = NA) {

  # Read in data cube
  occurrence_data <- readr::read_csv(
    file = cube_name,
    col_types = readr::cols(
      year = readr::col_double(),
      eea_cell_code = readr::col_character(),
      n = readr::col_double(),
      min_coord_uncertainty = readr::col_double()
    ),
    na = ""
  )

  # Read in associated taxonomic info
  taxonomic_info <- readr::read_csv(
    file = tax_info,
    col_types = readr::cols(
      scientificName = readr::col_character(),
      rank = readr::col_factor(),
      taxonomicStatus = readr::col_factor(),
      kingdom = readr::col_factor(),
      includes = readr::col_character()
    ),
    na = ""
  )

  if(!is.na(datasets_info)) {

    # Read in associated dataset info
    datasets_info <- readr::read_csv(
      file = dataset_info,
      col_types = readr::cols(
        # datasetKey = readr::col_double(),
        datasetName = readr::col_factor(),
        dataType = readr::col_factor()
      ),
      na = ""
    )

  }

  if("speciesKey" %in% colnames(occurrence_data)) {

    occurrence_data <-
      occurrence_data %>%
      dplyr::rename(taxonKey = "speciesKey")

    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(eea_cell_code = gsub("-", "", eea_cell_code))

    taxonomic_info <-
      taxonomic_info %>%
      dplyr::rename(taxonKey = "speciesKey")


  }

  # Merged the three data frames together
  merged_data <- dplyr::left_join(occurrence_data, taxonomic_info, by = "taxonKey")

  if(!is.na(datasets_info)) {

    merged_data <- dplyr::left_join(datasets_info, by = "datasetKey")

  }

  # Separate 'eea_cell_code' into resolution, coordinates
  merged_data <- merged_data %>%
    dplyr::mutate(
      xcoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=E)\\d+")),
      ycoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=N)\\d+")),
      resolution = stringr::str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
    )

  if(!is.na(datasets_info)) {

      # Remove columns that are not needed
      merged_data <-
        merged_data %>%
        dplyr::select(-min_coord_uncertainty, -taxonomicStatus, -datasetKey, -includes, -notes)

  } else {

      # Remove columns that are not needed
      merged_data <-
        merged_data %>%
        dplyr::select(-min_coord_uncertainty, -taxonomicStatus, -includes)

  }

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
    dplyr::summarize(max_year = max(year)-1) %>%
    dplyr::pull(max_year) %>%
    ifelse(is.na(final_year),
           .,
           ifelse(final_year < ., final_year, .))

  # Limit data set
  merged_data <-
    merged_data %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= final_year)

  # Remove any duplicate rows
  merged_data <-
    merged_data %>%
    dplyr::distinct()

  cube <- new_processed_cube(merged_data)

}
