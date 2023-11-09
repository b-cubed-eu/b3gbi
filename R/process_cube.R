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
#' @param datasets_info The location and name of an associated dataset info file.
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
process_cube <- function(cube_name, tax_info, datasets_info = NA, first_year = NA, final_year = NA) {

  # Read in data cube
  occurrence_data <- readr::read_csv(
    file = cube_name,
    col_types = cols(
      year = col_double(),
      eea_cell_code = col_character(),
      n = col_double(),
      min_coord_uncertainty = col_double()
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
      rename(taxonKey = "speciesKey")

    occurrence_data <-
      occurrence_data %>%
      mutate(eea_cell_code = gsub("-", "", eea_cell_code))

    taxonomic_info <-
      taxonomic_info %>%
      rename(taxonKey = "speciesKey")


  }

  # Merged the three data frames together
  merged_data <- dplyr::left_join(occurrence_data, taxonomic_info, by = "taxonKey")

  if(!is.na(datasets_info)) {

  merged_data <- dplyr::left_join(datasets_info, by = "datasetKey")

  }


  # Separate 'eea_cell_code' into resolution, coordinates
  merged_data <- merged_data %>%
    dplyr::mutate(
      xcoord = as.numeric(str_extract(eea_cell_code, "(?<=E)\\d+")),
      ycoord = as.numeric(str_extract(eea_cell_code, "(?<=N)\\d+")),
      resolution = str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
    )

  if(!is.na(datasets_info)) {

  # Remove columns that are not needed
  merged_data <-
    merged_data %>%
    dplyr::select(-min_coord_uncertainty, -taxonomicStatus, -includes, -notes)

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

}
