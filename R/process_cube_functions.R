#' @title Process GBIF Data Cubes
#'
#' @description Processes a GBIF data cube and associated taxonomic information
#'   file. All files must reside in the same directory and share a base file
#'   name (e.g., 'my_mammals_cube.csv', 'my_mammals_info.csv').
#'
#' @param cube_name The location and name of a data cube file
#'   (e.g., 'inst/extdata/europe_species_cube.csv').
#' @param tax_info The location and name of an associated taxonomic info file
#'   (e.g.,  'inst/extdata/europe_species_info.csv').
#' @param datasets_info The location and name of an associated dataset info file
#'   (e.g., 'inst/extdata/europe_species_datasets.csv').
#' @param first_year (Optional) The first year of occurrences to include. If not
#'   specified, uses a default of 1600 to prevent false records (e.g. with year = 0).
#' @param last_year (Optional) The final year of occurrences to include. If not
#'   specified, uses the latest year present in the cube.
#'
#' @return A tibble containing the processed GBIF occurrence data.
#'
#' @examples
#' \dontrun{
#' cube_name <- system.file("extdata", "europe_species_cube.csv", package = "b3gbi")
#' tax_info <- system.file("extdata", "europe_species_info.csv", package = "b3gbi")
#' europe_example_cube <- process_cube(cube_name, tax_info)
#' europe_example_cube
#' }
#' @export
process_cube <- function(cube_name, tax_info = NULL, datasets_info = NULL, first_year = 1600, last_year = NULL) {

  # Check whether there is a separate taxonomic information file
  if (is.null(tax_info)) {
    # if not, assume cube is in the new format and call process_cube_new function
    proc_cube <- process_cube_new(cube_name, first_year, last_year)
    return(proc_cube)
  }

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

  if(!is.null(datasets_info)) {

    # Read in associated dataset info
    datasets_info <- readr::read_csv(
      file = datasets_info,
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

  if(!is.null(datasets_info)) {

    merged_data <- dplyr::left_join(merged_data, datasets_info, by = "datasetKey")

  }

  # Separate 'eea_cell_code' into resolution, coordinates
  merged_data <- merged_data %>%
    dplyr::mutate(
      xcoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=E)\\d+")),
      ycoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=N)\\d+")),
      resolution = stringr::str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
    )

  if(!is.null(datasets_info)) {

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
    ifelse(is.null(first_year),
           .,
           ifelse(first_year > ., first_year, .))
  last_year <- merged_data %>%
    dplyr::summarize(max_year = max(year)-1) %>%
    dplyr::pull(max_year) %>%
    ifelse(is.null(last_year),
           .,
           ifelse(last_year < ., last_year, .))

  # Limit data set
  merged_data <-
    merged_data %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= last_year)

  # Remove any duplicate rows
  merged_data <-
    merged_data %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  cube <- new_processed_cube(merged_data)

}



#' @noRd
process_cube_new <- function(cube_name, first_year = NULL, last_year = NULL) {

  # Read in data cube
  occurrence_data <- readr::read_delim(
    file = cube_name,
    col_types = readr::cols(
      yearmonth = readr::col_character(),
      eeacellcode = readr::col_character(),
      occurrences = readr::col_double(),
      mincoordinateuncertaintyinmeters = readr::col_double(),
      mintemporaluncertainty = readr::col_double(),
      familykey = readr::col_double(),
      specieskey = readr::col_double(),
      family = readr::col_character(),
      species = readr::col_character(),
      familycount = readr::col_double(),
    ),
    delim = "\t",
    na = ""
  )

  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(taxonKey = "specieskey",
                  scientificName = "species")

  # Remove NA values in eea_cell_code
  occurrence_data <-
    occurrence_data %>%
    dplyr::filter(!is.na(eeacellcode))

  occurrence_data <-
    occurrence_data %>%
    dplyr::mutate(eeacellcode = stringr::str_replace(eeacellcode, "W", "W-")) %>%
    dplyr::mutate(eeacellcode = stringr::str_replace(eeacellcode, "S", "S-"))

  # Separate 'eeacellcode' into resolution, coordinates
  occurrence_data <- occurrence_data %>%
    dplyr::mutate(
      xcoord = as.numeric(stringr::str_extract(eeacellcode, "(?<=[EW])-?\\d+")),
      ycoord = as.numeric(stringr::str_extract(eeacellcode, "(?<=[NS])-?\\d+")),
      resolution = stringr::str_replace_all(eeacellcode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", "")
    ) %>%
    dplyr::rename(eea_cell_code = eeacellcode)

  # Remove columns that are not needed
  occurrence_data <-
    occurrence_data %>%
    dplyr::select(-mincoordinateuncertaintyinmeters, -mintemporaluncertainty, -sex, -lifestage)


  # Rename column occurrences to obs
  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(obs = occurrences)

  # Convert yearmonth to just year
  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(year = yearmonth) %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(year, "(\\d{4})")))

  # Check whether start and end years are within dataset
  first_year <- occurrence_data %>%
    dplyr::select(year) %>%
    min() %>%
    ifelse(is.null(first_year),
           .,
           ifelse(first_year > ., first_year, .))
  last_year <- occurrence_data %>%
    dplyr::summarize(max_year = max(year)-1) %>%
    dplyr::pull(max_year) %>%
    ifelse(is.null(last_year),
           .,
           ifelse(last_year < ., last_year, .))

  # Limit data set
  occurrence_data <-
    occurrence_data %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= last_year)

  # Remove any duplicate rows
  occurrence_data <-
    occurrence_data %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  cube <- new_processed_cube(occurrence_data)

}
