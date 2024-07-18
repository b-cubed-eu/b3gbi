#' @title Process GBIF Data Cubes
#'
#' @description Processes a GBIF data cube and (if applicable) an associated taxonomic
#'   information file. If your cube includes a taxonomic info file it is likely a
#'   previous generation cube and should be processed using 'process_cube_old'.
#'   The taxonomic info file must reside in the same directory as your cube and
#'   share a base file name (e.g., 'cubes/my_mammals_cube.csv', 'cubes/my_mammals_info.csv').
#'   If your cube does NOT include a taxonomic info file then it is likely a current
#'   generation cube and should be processed using the standard process_cube function
#'   The API used to generate the current generation cubes is very flexible and allows
#'   user-specified column names. Therefore, please check that the column names
#'   of your cube match the Darwin Core standard expected by the process_cube function.
#'   If they do not, you may need to enter them manually. The function will return
#'   an error if it cannot find all required columns.
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
#' @param data_type Specify the format of your input data (e.g. .csv file or data frame).
#' @param grid_type Specify which grid reference system your cube uses. By default
#'  the function will attempt to determine this automatically and return an error if it fails.
#'  If you want to perform analysis on a custom cube without grid codes (e.g. output
#'  from the gcube package), select 'none'. The function will then create a dummy column and
#'  fill it with zeros to avoid errors downstream when calculating indicators.
#' @param force_gridcode Force the function to assume a specific grid reference system.
#'  This may cause unexpected downstream issues, so it is not recommended. If you are
#'  getting errors related to grid cell codes, check to make sure they are valid.
#' @param cols_year The name of the column containing the year of occurrence (if
#'  something other than 'year'). This column is required unless you have a yearMonth
#'  column.
#' @param cols_yearMonth The name of the column containing the year and month of
#'  occurrence (if present and if other than 'yearMonth'). Use this if only if you
#'  do not have a year column. The b3gbi package does not use month data, so the
#'  function will convert your yearMonth column to a year column.
#' @param cols_cellCode The name of the column containing the grid reference codes
#'  (if other than 'cellCode'). This column is required.
#' @param cols_occurrences The name of the column containing the number of occurrence
#'  (if other than 'occurrences'). This column is required.
#' @param cols_scientificName The name of the column containing the scientific name
#'  of the species (if other than 'scientificName'). Note that it is not necessary
#'  to have both a species column and a scientificName column. One or the other is
#'  sufficient.
#' @param cols_minCoordinateUncertaintyInMeters The name of the column containing
#'  the minimum coordinate uncertainty of the occurrences (if other than
#'  'minCoordinateUncertaintyinMeters').
#' @param cols_minTemporalUncertainty The name of the column containing the minimum
#'  temporal uncertainty of the occurrences (if other than 'minTemporalUncertainty').
#' @param cols_kingdom The name of the column containing the kingdom the occurring
#'  species belongs to (if other than 'kingdom'). This column is optional.
#' @param cols_family The name of the column containing the family the occurring
#'  species belongs to (if other than 'family'). This column is optional.
#' @param cols_species The name of the column containing the name of the occurring
#'  species (if other than 'species'). Note that it is not necessary to have both a
#'  species column and a scientificName column. One or the other is sufficient.
#' @param cols_kingdomKey The name of the column containing the kingdom key of the
#'  occurring species (if other than 'kingdomKey'). This column is optinal.
#' @param cols_familyKey The name of the column containing the family key of the
#'  occurring species (if other than 'familykey'). This column is optional.
#' @param cols_speciesKey The name of the column containing the species key of the
#'  occurring species (if other than 'speciesKey'). This column is required, but
#'  note that if you have a 'taxonKey' column you can provide it as the speciesKey.
#' @param cols_familyCount The name of the column containing the occurrence count
#'  by family. This column is optional.
#' @param cols_sex The name of the column containing the sex of the observed
#'  individuals. This column is optional.
#' @param cols_lifeStage the name of the column containing the life stage of the
#'  observed individuals. This column is optional.
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
                         data_type = c("csv", "df"),
                         grid_type = c("automatic", "eea", "mgrs", "eqdgc", "none"),
                         first_year = NULL,
                         last_year = NULL,
                         force_gridcode = FALSE,
                         cols_year = NULL,
                         cols_yearMonth = NULL,
                         cols_cellCode = NULL,
                         cols_occurrences = NULL,
                         cols_scientificName = NULL,
                         cols_minCoordinateUncertaintyInMeters = NULL,
                         cols_minTemporalUncertainty = NULL,
                         cols_kingdom = NULL,
                         cols_family = NULL,
                         cols_species = NULL,
                         cols_kingdomKey = NULL,
                         cols_familyKey = NULL,
                         cols_speciesKey = NULL,
                         cols_familyCount = NULL,
                         cols_sex = NULL,
                         cols_lifeStage = NULL) {

  data_type = match.arg(data_type)

  if (data_type == "csv") {

    # Read in data cube
    occurrence_data <- readr::read_delim(
      file = cube_name,
      delim = "\t",
      na = "",
      show_col_types = FALSE
    )

  } else {

    # Check that input is a data frame
    stopifnot(is.data.frame(cube_name))

    # Read in data cube
    occurrence_data <- tibble::as_tibble(cube_name)

  }

  grid_type = match.arg(grid_type)

  if (grid_type == "automatic") {

    # check if the user has provided a name for the column containing grid cell codes
    if (!is.null(cols_cellCode)) {

      # check that the column name they provided exists
      if (!cols_cellCode %in% names(occurrence_data)) {

        stop("The column name you provided for grid cell codes does not exist. Please double check that you spelled it correctly.")

      }

      # try to identify the reference grid and return an error if it fails
      grid_code_sample <- occurrence_data[[cols_cellCode]][!is.na(occurrence_data[[cols_cellCode]])][1]
      grid_type <- detect_grid(grid_code_sample, stop_on_fail = TRUE)

      # if successful rename the user-specified column to the default
      occurrence_data <-
        occurrence_data %>%
        dplyr::rename(cellCode = cols_cellCode)

    } else {

      # if no name was provided loop through columns to find grid codes and identify reference grid
      for (col in colnames(occurrence_data)) {

        grid_code_sample <- occurrence_data[[col]][!is.na(occurrence_data[[col]])][1]
        grid_type <- detect_grid(grid_code_sample, stop_on_fail = FALSE)

        # check whether grid_type was successfully identified
        if (!is.na(grid_type)) {

          # if successful rename the found column to the default for grid cell codes
          occurrence_data <-
            occurrence_data %>%
            dplyr::rename(cellCode = col)

          # then end the loop
          break

        }

      }

      if (is.na(grid_type)) {

        # if grid cell codes could not be identified in any column, return an error
        stop("Could not detect grid type. Please specify manually.")

      }

    }

    # if the user has chosen 'none' as a grid type...
  } else if (grid_type == "none") {

      # create dummy column full of zeros
      occurrence_data$cellCode <- 0

    # if the user has specified a grid type...
  } else {

    # check if the user has provided a name for the column containing grid cell codes
    if (is.null(cols_cellCode)) {

      # if not, try to identify it automatically (returns an error if unsuccessful)
      cols_cellCode <- detect_grid_column(occurrence_data, grid_type)

    } else {

      # check that the column name they provided exists
      if (!cols_cellCode %in% names(occurrence_data)) {

        stop("The column name you provided for grid cell codes does not exist. Please double check that you spelled it correctly.")

      }

    }

    if (force_gridcode == FALSE & grid_type!="none") {

      grid_type_test <- ifelse(grid_type == "eea", stringr::str_detect(occurrence_data[[cols_cellCode]], "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"),
                               ifelse(grid_type == "mgrs", stringr::str_detect(occurrence_data[[cols_cellCode]], "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"),
                                      ifelse(grid_type == "eqdgc", stringr::str_detect(occurrence_data[[cols_cellCode]], "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"),
                                             NA)))

      if(!grid_type_test==TRUE) {

        stop("Cell codes do not match the expected format. Are you sure you have specified the correct grid system?
             It is recommended to leave grid_type on 'automatic'. If you are certain, you can use force_gridecode = TRUE
             to attempt to translate them anyway, but this could lead to unexpected downstream errors.")

      }

    }

    # rename it to the default
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename(cellCode = cols_cellCode)

  }

  # make a list of other user provided column names
  col_names_userlist <- list(cols_year,
                             cols_yearMonth,
                             cols_occurrences,
                             cols_scientificName,
                             cols_minCoordinateUncertaintyInMeters,
                             cols_minTemporalUncertainty,
                             cols_kingdom,
                             cols_family,
                             cols_species,
                             cols_kingdomKey,
                             cols_familyKey,
                             cols_speciesKey,
                             cols_familyCount,
                             cols_sex,
                             cols_lifeStage)

  # replace NULL values with NA
  col_names_userlist[sapply(col_names_userlist, is.null)] <- NA

  # list default column names to replace them with
  col_names_defaultlist <- list("year",
                                "yearMonth",
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
                                "familyCount",
                                "sex",
                                "lifeStage")

  # combine lists into data frame
  col_names <- data.frame("default" = unlist(col_names_defaultlist), "user" = unlist(col_names_userlist))

  # rename user-supplied column names to defaults expected by package functions
  names(occurrence_data)[names(occurrence_data) %in% col_names[,2]] <-
    col_names[,1][col_names[,2] %in% (names(occurrence_data))]

  # for (i in 1:length(col_names_userlist)) {
  #
  #   if (!is.null(col_names_userlist[i])) {
  #
  #     new_name <- col_names_defaultlist[i]
  #     old_name <- col_names_userlist[i]
  #     occurrence_data <-
  #       occurrence_data %>%
  #       dplyr::rename(!!new_name := old_name)
  #
  #   }
  #
  # }

  # check for any non-user-supplied column names which match the default names but not the capitalization pattern and fix them
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
  if (!"year" %in% colnames(occurrence_data) & "yearMonth" %in% colnames(occurrence_data)) {

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

  essential_cols <- c("year",
                      "occurrences",
                      "minCoordinateUncertaintyInMeters",
                      "minTemporalUncertainty",
                      "kingdomKey",
                      "familyKey",
                      "speciesKey",
                      "familyCount")
  # make sure that essential columns are the correct type
  occurrence_data <-
    occurrence_data %>%
    dplyr::mutate(across(any_of(essential_cols), as.numeric))


  # rename occurrences and speciesKey columns to be consistent with the other package functions (should maybe change this throughout package?)
  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(obs = occurrences) %>%
    dplyr::rename(taxonKey = speciesKey)

  # Remove NA values in cell code column
  occurrence_data <-
    occurrence_data %>%
    dplyr::filter(!is.na(cellCode))

  if (grid_type == "eea") {

    if (force_gridcode == FALSE) {

      if(!ifelse(stringr::str_detect(occurrence_data$cellCode[1], "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"), TRUE, FALSE)){

        stop("Cell codes do not match the expected format. Are you sure you have specified the correct grid system?
             It is recommended to leave grid_type on 'automatic'. If you are certain, you can use force_gridecode = TRUE
             to attempt to translate them anyway, but this could lead to unexpected downstream errors.")

      }

    }

    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "W", "W-")) %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "S", "S-"))

    # Separate cell code into resolution, coordinates
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(
        xcoord = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+"))*1000,
        ycoord = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+"))*1000,
        resolution = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", ""))

  } else if (grid_type == "mgrs") {

    if (force_gridcode == FALSE) {

      if(!ifelse(stringr::str_detect(occurrence_data$cellCode[1], "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"), TRUE, FALSE)){

        stop("Cell codes do not match the expected format. Are you sure you have specified the correct grid system?
             It is recommended to leave grid_type on 'automatic'. If you are certain, you can use force_gridecode = TRUE
             to attempt to translate them anyway, but this could lead to unexpected downstream errors.")

      }

    }

    #utm <- mgrs::mgrs_to_utm(occurrence_data$cellCode)
    #occurrence_data$xcoord <- utm$easting
    #occurrence_data$ycoord <- utm$northing
    latlong <- mgrs::mgrs_to_latlng(occurrence_data$cellCode)
    occurrence_data$xcoord <- latlong$lng
    occurrence_data$ycoord <- latlong$lat

    # this will not work properly if there is a - symbol in the code
    occurrence_data$resolution <- paste0(10^((9 - nchar(occurrence_data$cellCode[1])) / 2), "km")

  } else if (grid_type == "eqdgc") {

    if (force_gridcode == FALSE) {

      if(!ifelse(stringr::str_detect(occurrence_data$cellCode[1], "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"), TRUE, FALSE)){

        stop("Cell codes do not match the expected format. Are you sure you have specified the correct grid system?
             It is recommended to leave grid_type on 'automatic'. If you are certain, you can use force_gridecode = TRUE
             to attempt to translate them anyway, but this could lead to unexpected downstream errors.")

      }

    }

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

  }

  if(min(occurrence_data$year)==max(occurrence_data$year)) {

    first_year <- min(occurrence_data$year)
    last_year <- first_year

    warning("Cannot create trends with this dataset, as occurrences are all from the same year.")

  } else {

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

  }

  # Remove any duplicate rows
  occurrence_data <-
    occurrence_data %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  if (grid_type == "none") {
    cube <- new_sim_cube(occurrence_data, grid_type)
  } else {
    cube <- new_processed_cube(occurrence_data, grid_type)
  }

}

#' @rdname process_cube
#' @export
process_cube_old <- function(cube_name,
                             tax_info = NULL,
                             datasets_info = NULL,
                             first_year = 1600,
                             last_year = NULL) {

  if (is.null(tax_info)) {

    stop("Please provide a taxonomic information file using the argument tax_info.
    This function is only intended for processing older generation cubes made using
    the TriAS code. Current generation cubes built using the GBIF API should be
    processed using process_cube().")

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
      xcoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=E)\\d+"))*1000,
      ycoord = as.numeric(stringr::str_extract(eea_cell_code, "(?<=N)\\d+"))*1000,
      resolution = stringr::str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
    )

  if(!is.null(datasets_info)) {

    # Remove columns that are not needed
    merged_data <-
      merged_data %>%
      dplyr::select(-taxonomicStatus, -includes, -notes)

  } else {

    # Remove columns that are not needed
    merged_data <-
      merged_data %>%
      dplyr::select(-taxonomicStatus, -includes)

  }

  # Rename column n to obs
  merged_data <-
    merged_data %>%
    dplyr::rename(obs = n,
                  cellCode = eea_cell_code,
                  minCoordUncertaintyInMeters = min_coord_uncertainty)

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

  cube <- new_processed_cube(merged_data, grid_type = "eea")

}
