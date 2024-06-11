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
process_cube <- function(cube_name,
                         tax_info = NULL,
                         datasets_info = NULL,
                         first_year = 1600,
                         last_year = NULL,
                         ...) {

  # Check whether there is a separate taxonomic information file
  if (is.null(tax_info)) {
    # if not, assume cube is in the new format and call process_cube_new function
    proc_cube <- process_cube_new(cube_name = cube_name,
                                  first_year = first_year,
                                  last_year = last_year,
                                  ...)
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
    dplyr::rename(obs = n, cellCode = eea_cell_code)

  # Check whether start and end years are within dataset
  first_year <- merged_data %>%
    dplyr::select(year) %>%
    min(na.rm = TRUE) %>%
    ifelse(is.null(first_year),
           .,
           ifelse(first_year > ., first_year, .))
  last_year <- merged_data %>%
    dplyr::summarize(max_year = max(year, na.rm = TRUE)-1) %>%
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
process_cube_new <- function(cube_name,
                             grid_type = c("automatic", "eea", "mgrs", "eqdgc"),
                             first_year = NULL,
                             last_year = NULL,
                             cols_year = NULL,
                             cols_yearmonth = NULL,
                             cols_date = NULL,
                             cols_cellcode = NULL,
                             cols_occurrences = NULL,
                             cols_scientificname = NULL,
                             cols_mincoordinateuncertaintyinmeters = NULL,
                             cols_mintemporaluncertainty = NULL,
                             cols_kingdom = NULL,
                             cols_family = NULL,
                             cols_species = NULL,
                             cols_kingdomkey = NULL,
                             cols_familykey = NULL,
                             cols_specieskey = NULL,
                             cols_familycount = NULL) {

  # Read in data cube
  occurrence_data <- readr::read_delim(
    file = cube_name,
    delim = "\t",
    na = "",
    show_col_types = FALSE
  )

  grid_type = match.arg(grid_type)


  if (grid_type == "automatic") {

    if (!is.null(cols_cellcode)) {

      grid_type <- ifelse(tolower("eea") %in% tolower(cols_cellcode), "eea",
                          ifelse(tolower("mgrs") %in% tolower(cols_cellcode), "mgrs",
                                 ifelse(tolower("eqdgc") %in% tolower(cols_cellcode), "eqdgc", NA)))

      if (is.na(grid_type)) {
        stop("Could not detect grid type. Please specify manually.")
      } else {
        occurrence_data <-
          occurrence_data %>%
          dplyr::rename(cellCode = cols_cellcode)
      }


    } else {

      grid_type <- ifelse(tolower("eeaCellCode") %in% tolower(colnames(occurrence_data)), "eea",
                          ifelse(tolower("mgrsCellCode") %in% tolower(colnames(occurrence_data)), "mgrs",
                                 ifelse(tolower("eqdgcCellCode") %in% tolower(colnames(occurrence_data)), "eqdgc", NA)))

      if (is.na(grid_type)) {
        stop("Could not detect grid type. Please specify manually.")
      } else {

        old_cc_name <- colnames(occurrence_data)[grepl(paste0(grid_type, "CellCode"), colnames(occurrence_data), ignore.case=TRUE)]
        occurrence_data <-
          occurrence_data %>%
          dplyr::rename(cellCode = old_cc_name)
      }

    }

  } else {
    stopifnot("Could not find column containing grid cell codes. Please supply column name as an argument to the function." =
                tolower(paste0(grid_type, "CellCode")) %in% tolower(colnames(occurrence_data)))

    old_cc_name <- colnames(occurrence_data)[grepl(paste0(grid_type, "CellCode"), colnames(occurrence_data), ignore.case=TRUE)]
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename(cellCode = old_cc_name)
  }

  col_names_userlist <- list(cols_year,
                             cols_yearmonth,
                             cols_cellcode,
                             cols_occurrences,
                             cols_scientificname,
                             cols_mincoordinateuncertaintyinmeters,
                             cols_mintemporaluncertainty,
                             cols_kingdom,
                             cols_family,
                             cols_species,
                             cols_kingdomkey,
                             cols_familykey,
                             cols_specieskey,
                             cols_familycount)

  col_names_defaultlist <- list("year",
                                "yearMonth",
                                "cellCode",
                                "occurrences",
                                "scientificName",
                                "minCoordinateUncertaintyInMeters",
                                "minTemporalUncertainty",
                                "kingdom",
                                "family",
                                "species",
                                "kingdomKey",
                                "familyKey",
                                "speciesKey",
                                "familyCount")

  # rename user-supplied column names to defaults expected by package functions
  for (i in 1:length(col_names_userlist)) {
    if (!is.null(col_names_userlist[[i]])) {
      new_name <- col_names_defaultlist[[i]]
      old_name <- col_names_userlist[[i]]
      occurrence_data <-
        occurrence_data %>%
        dplyr::rename(!!new_name := old_name)
    }
  }

  # check for any default column names which do not match the default capitalization pattern and fix them
  for (i in 1:length(col_names_defaultlist)) {
    if (!col_names_defaultlist[[i]] %in% colnames(occurrence_data) & tolower(col_names_defaultlist[[i]]) %in% tolower(colnames(occurrence_data))) {
      new_name <- col_names_defaultlist[[i]]
      old_name <- colnames(occurrence_data)[grepl(new_name, colnames(occurrence_data), ignore.case=TRUE)]
      occurrence_data <-
        occurrence_data %>%
        dplyr::rename(!!new_name := old_name)
    }
  }

  # If year column missing but yearMonth column present, convert yearMonth to year
  if (!tolower("year") %in% tolower(colnames(occurrence_data)) & tolower("yearMonth") %in% tolower(colnames(occurrence_data))) {
    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(year = as.numeric(stringr::str_extract(yearMonth, "(\\d{4})")))
  }

  # If scientificName column missing but species column present, copy species to scientificName
  if ("species" %in% colnames(occurrence_data) & !("scientificName" %in% colnames(occurrence_data))) {
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename(scientificName = species)
  }

  # check if any essential columns (required by package functions) are missing
  required_colnames <- c("year", "occurrences", "scientificName", "speciesKey")
  missing_colnames <- required_colnames[which(!required_colnames %in% colnames(occurrence_data))]
  if(length(missing_colnames) >= 1) {
    stop(paste0("\nThe following columns could not be detected in cube:", missing_colnames, "\nPlease supply the missing column names as arguments to the function.\n"))
  }

  # rename occurrences and speciesKey columns to be consistent with the other package functions (should maybe change this throughout package?)
  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(obs = occurrences) %>%
    dplyr::rename(taxonKey = speciesKey)


  if (grid_type == "eea") {

    # Remove NA values in cell code
    occurrence_data <-
      occurrence_data %>%
      dplyr::filter(!is.na(cellCode))

    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "W", "W-")) %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "S", "S-"))

    # Separate cell code into resolution, coordinates
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(
        xcoord = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+")),
        ycoord = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+")),
        resolution = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", ""))

  } else if (grid_type == "mgrs") {

    # Remove NA values in cell code
    occurrence_data <-
      occurrence_data %>%
      dplyr::filter(!is.na(cellCode))

    # Need to skip 32UNC and divide the digits after the final letter into two halves. The first is north-south, second half is east-west.
    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "W", "W-")) %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "S", "S-"))

    # Separate cell code into resolution, coordinates
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(
        xcoord = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+")),
        ycoord = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+")),
        resolution = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", ""))

    } else if (grid_type == "eqdgc") {

    # Remove NA values in cell code
    occurrence_data <-
      occurrence_data %>%
      dplyr::filter(!is.na(cellCode))

    # Need to find W or E and take the 3 digits after it as eastwest (longitude), and find N or S and take the 2 digits after it as northsouth (latitude)
    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "W", "W-")) %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "S", "S-"))

    latlong <- convert_eqdgc_latlong(occurrence_data$cellCode)
    lat <- latlong[,1]
    long <- latlong[,2]

    occurrence_data$xcoord <- long
    occurrence_data$ycoord <- lat
    occurrence_data$resolution <- rep(paste0((1/(2^(nchar(occurrence_data$cellCode[1])-7))), "degrees"), nrow(occurrence_data))

    # # Separate cell code into resolution, coordinates
    # occurrence_data <- occurrence_data %>%
    #   dplyr::mutate(
    #     xcoord = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+")),
    #     ycoord = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+")),
    #     resolution = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", ""))
  }


  # Check whether start and end years are within dataset
  first_year <- occurrence_data %>%
    dplyr::select(year) %>%
    min(na.rm = TRUE) %>%
    ifelse(is.null(first_year),
           .,
           ifelse(first_year > ., first_year, .))
  last_year <- occurrence_data %>%
    dplyr::summarize(max_year = max(year, na.rm = TRUE)-1) %>%
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
