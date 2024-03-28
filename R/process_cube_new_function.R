process_cube_new <- function(cube_name, first_year = NULL, final_year = NULL) {

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
  final_year <- occurrence_data %>%
    dplyr::summarize(max_year = max(year)-1) %>%
    dplyr::pull(max_year) %>%
    ifelse(is.null(final_year),
           .,
           ifelse(final_year < ., final_year, .))

  # Limit data set
  occurrence_data <-
    occurrence_data %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= final_year)

  # Remove any duplicate rows
  occurrence_data <-
    occurrence_data %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  cube <- new_processed_cube(occurrence_data)

}
