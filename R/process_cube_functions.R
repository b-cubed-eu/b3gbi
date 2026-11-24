#' @title Process GBIF Data Cubes
#'
#' @description Processes a GBIF occurrence cube (a CSV or parquet file, or a
#'  data frame) into a `processed_cube` object. Cubes produced by the GBIF cube API can
#'  have user-specified column names, so check that your column names match
#'  the Darwin Core names expected by this function; if not, supply them with
#'  the `cols_*` arguments. The function stops with an error if it cannot find
#'  all required columns.
#'
#' @param cube_name Either the path to a data cube CSV or parquet file (e.g.
#'   `system.file("extdata", "denmark_mammals_cube_eqdgc.csv", package =
#'   "b3gbi")`) or a data frame containing the cube. Reading parquet files
#'   requires the \pkg{arrow} package.
#' @param grid_type (Optional) The grid reference system your cube uses. One of
#'  `"automatic"` (default), `"eea"`, `"mgrs"`, `"eqdgc"`, `"isea3h"`,
#'  `"custom"` or `"none"`. With `"automatic"` the function attempts to detect
#'  the grid from the cell codes and returns an error if it fails. If you want
#'  to perform analysis on a cube with custom grid codes (e.g. output from the
#'  gcube package) or a cube without grid codes, select `"custom"` or `"none"`,
#'  respectively. Cubes with custom grid codes can only be used for time
#'  series, unless the cell geometries are given with `grid`; supplying `grid`
#'  sets the grid type to `"custom"`.
#' @param first_year (Optional) The first year of occurrences to include. If not
#'   specified, uses the earliest year present in the cube.
#' @param last_year (Optional) The final year of occurrences to include. If not
#'   specified, uses the latest year present in the cube.
#' @param force_gridcode (Optional) Logical. If `TRUE`, skips the check that
#'  cell codes match the expected format of `grid_type`. Not recommended;
#'  invalid codes may cause downstream errors. Default `FALSE`.
#' @param cols_year (Optional) The name of the column containing the year of
#' occurrence (if something other than 'year'). This column is required unless
#' you have a yearMonth column.
#' @param cols_yearMonth (Optional) The name of the column containing the year
#'  and month of occurrence (if present and if other than 'yearMonth'). Use this
#'  only if you do not have a year column. The b3gbi package does not use month
#'  data, so the function will convert your yearMonth column to a year column.
#' @param cols_yearMonthDay (Optional) The name of the column containing the
#'  year, month and day of occurrence (if present and if other than
#'  'yearMonthDay'). Use this only if you do not have year or yearMonth columns.
#'  The b3gbi package does not use day or month data, so the function will
#'  convert your yearMonthDay column to a year column.
#' @param cols_cellCode (Optional) The name of the column containing the grid
#' reference codes (if other than 'cellCode'). This column is required.
#' @param cols_occurrences (Optional) The name of the column containing the
#' number of occurrences (if other than 'occurrences'). This column is required.
#' @param cols_scientificName (Optional) The name of the column containing the
#'  scientific name of the species (if other than 'scientificName'). Note that
#'  it is not necessary to have both a species column and a scientificName
#'  column. One or the other is sufficient.
#' @param cols_minCoordinateUncertaintyInMeters (Optional) The name of the
#' column containing the minimum coordinate uncertainty of the occurrences (if
#' other than 'minCoordinateUncertaintyInMeters').
#' @param cols_minTemporalUncertainty (Optional) The name of the column
#'  containing the minimum temporal uncertainty of the occurrences (if other
#'  than 'minTemporalUncertainty').
#' @param cols_kingdom (Optional) The name of the column containing the kingdom
#'  the occurring species belongs to (if other than 'kingdom').
#' @param cols_family (Optional) The name of the column containing the family
#'  the occurring species belongs to (if other than 'family').
#' @param cols_species (Optional) The name of the column containing the name of
#'  the occurring species (if other than 'species'). Note that it is not
#'  necessary to have both a species column and a scientificName column. One or
#'  the other is sufficient.
#' @param cols_kingdomKey (Optional) The name of the column containing the
#'  kingdom key of the occurring species (if other than 'kingdomKey').
#' @param cols_familyKey (Optional) The name of the column containing the family
#'  key of the occurring species (if other than 'familyKey').
#' @param cols_speciesKey (Optional) The name of the column containing the
#'  species key of the occurring species (if other than 'speciesKey'). The
#'  column is required, but note that if you have a 'taxonKey' column you can
#'  provide it as the speciesKey.
#' @param cols_familyCount (Optional) The name of the column containing the
#'  occurrence count by family.
#' @param cols_sex (Optional) The name of the column containing the sex of the
#'  observed individuals.
#' @param cols_lifeStage (Optional) The name of the column containing the life
#'  stage of the observed individuals.
#' @param separator (Optional) The column-separating character in your csv file.
#'  This should be automatically recognized, so only specify this if you are
#'  having trouble.
#' @param grid (Optional) The geometries of the grid cells, for cubes that use
#'  a grid other than the supported ones (e.g. cubes from the BMD cubing
#'  engine, whose cells are identified by an ID and described in a separate
#'  dimension table). Either the path to a file (GeoParquet, GeoPackage,
#'  shapefile, GeoJSON, or a CSV with a WKT geometry column), an sf object, or
#'  a data frame with a WKT geometry column. Each grid cell must be a polygon
#'  with a unique ID, which is matched to the cube's grid cell column. With a
#'  grid, all indicators (maps and time series) can be calculated. Reading
#'  parquet files requires the \pkg{arrow} and \pkg{jsonlite} packages.
#' @param grid_id (Optional) The name of the ID column in `grid`. If not given,
#'  the column with the same name as `cols_cellCode` is used, or else the one
#'  column name shared by the cube and the grid (e.g. `grid_idx`).
#' @param grid_crs (Optional) The coordinate reference system of `grid` (e.g.
#'  `"EPSG:3035"`), needed only if the grid file does not contain one (e.g. a
#'  CSV with WKT geometries).
#'
#' @return An object of class `processed_cube` (or `sim_cube` when
#'  `grid_type` is `"none"`, or `"custom"` without `grid`): a list of metadata
#'  (years, number of species, grid type, resolution, ...) plus the processed
#'  occurrences in the `data` element. With a custom `grid`, the grid cells are
#'  stored in the `grid` element.
#'
#' @examples
#' \donttest{
#' cube_name <- system.file("extdata", "denmark_mammals_cube_eqdgc.csv",
#'                          package = "b3gbi")
#' denmark_example_cube <- process_cube(cube_name)
#' denmark_example_cube
#' }
#'
#' # A cube whose grid cells are defined in a separate file (e.g. a cube and
#' # dimension table from the BMD cubing engine):
#' # cube <- process_cube("cube.parquet", grid = "grid_cells.parquet")
#' @export
process_cube <- function(cube_name,
                         grid_type = c("automatic",
                                       "eea",
                                       "mgrs",
                                       "eqdgc",
                                       "isea3h",
                                       "custom",
                                       "none"),
                         first_year = NULL,
                         last_year = NULL,
                         force_gridcode = FALSE,
                         cols_year = NULL,
                         cols_yearMonth = NULL,
                         cols_yearMonthDay = NULL,
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
                         cols_lifeStage = NULL,
                         separator = NULL,
                         grid = NULL,
                         grid_id = NULL,
                         grid_crs = NULL) {

  yearMonth <- species <- occurrences <- speciesKey <- cellCode <- NULL
  year <- yearMonthDay <- . <- max_year <- NULL

  if (is_parquet_path(cube_name)) {

    occurrence_data <- read_parquet_cube(cube_name)

  } else if (is.character(cube_name) && length(cube_name) == 1) {
    if (is.null(separator)) {
      # Read in data cube
      # We first read a sample to detect the column name, or just read all as character
      # To be safe and efficient, we read everything as character and convert later
      occurrence_data <- readr::read_delim(
        file = cube_name,
        na = "",
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE
      )
    } else {
      # Read in data cube
      occurrence_data <- readr::read_delim(
        file = cube_name,
        delim = separator,
        na = "",
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE
      )
    }

  } else if (inherits(cube_name, "data.frame")) {

    # Read in data cube
    occurrence_data <- tibble::as_tibble(cube_name)

  } else {

    stop("`cube_name` should be a file path or dataframe.")

  }

  grid_type <- match.arg(grid_type)

  # check that the cube is not empty
  if (nrow(occurrence_data) == 0) {

    stop("The data cube is empty. Please check the file.")

  }

  # A user-supplied grid (cell geometries) makes this a custom-grid cube
  custom_grid <- NULL
  if (!is.null(grid)) {
    if (!grid_type %in% c("automatic", "custom")) {
      stop("`grid` can only be used with grid_type = 'custom' (or ",
           "'automatic').", call. = FALSE)
    }
    grid_type <- "custom"
    grid_sf <- read_custom_grid(grid, grid_crs)
    id_cols <- find_grid_id_columns(occurrence_data, grid_sf, cols_cellCode,
                                    grid_id)
    cols_cellCode <- id_cols$cube
    custom_grid <- prepare_custom_grid(grid_sf, id_cols$grid)
  }

  # check that user-provided column names are valid
  user_provided_cols <- list(
    "year" = cols_year,
    "yearMonth" = cols_yearMonth,
    "yearMonthDay" = cols_yearMonthDay,
    "cellCode" = cols_cellCode,
    "occurrences" = cols_occurrences,
    "scientificName" = cols_scientificName,
    "minCoordinateUncertaintyInMeters" = cols_minCoordinateUncertaintyInMeters,
    "minTemporalUncertainty" = cols_minTemporalUncertainty,
    "kingdom" = cols_kingdom,
    "family" = cols_family,
    "species" = cols_species,
    "kingdomKey" = cols_kingdomKey,
    "familyKey" = cols_familyKey,
    "speciesKey" = cols_speciesKey,
    "familyCount" = cols_familyCount,
    "sex" = cols_sex,
    "lifeStage" = cols_lifeStage
  )

  incorrect_cols <- character(0)

  for (default_name in names(user_provided_cols)) {
    user_name <- user_provided_cols[[default_name]]
    if (!is.null(user_name) && !user_name %in% names(occurrence_data)) {
      incorrect_cols <- c(incorrect_cols,
                          paste0("'",
                                 user_name,
                                 "' (for '", default_name, "')")
      )
    }
  }

  if (length(incorrect_cols) > 0) {
    stop(
      paste0(
        "The following user-provided column names were not found in the data: ",
        paste(
          incorrect_cols, collapse = ", "
        ),
        ". Please check the spelling and case of your column names."
      )
    )
  }

  if (grid_type == "automatic") {

    # check if the user has provided a name for the column containing grid codes
    if (!is.null(cols_cellCode)) {

      # check that the column name they provided exists
      if (!cols_cellCode %in% names(occurrence_data)) {

        stop(paste0(
          "The column name you provided for grid cell codes does not exist. ",
          "Please double check that you spelled it correctly."))

      }

      # try to identify the reference grid and return an error if it fails
      grid_code_sample <- occurrence_data[[cols_cellCode]][
        !is.na(occurrence_data[[cols_cellCode]])
      ][1]
      grid_type <- detect_grid(grid_code_sample, stop_on_fail = TRUE)

      # if successful rename the user-specified column to the default
      occurrence_data <-
        occurrence_data %>%
        dplyr::rename_with(.fn = ~"cellCode",
                           .cols = dplyr::all_of(cols_cellCode))

    } else {

      # if no name was provided loop through columns to find grid codes and
      # identify reference grid
      for (col in colnames(occurrence_data)) {

        grid_code_sample <- occurrence_data[[col]][
          !is.na(occurrence_data[[col]])
        ][1]
        grid_type <- detect_grid(grid_code_sample, stop_on_fail = FALSE)

        # check whether grid_type was successfully identified
        if (!is.na(grid_type)) {

          # if successful rename the found column to the default for grid codes
          occurrence_data <-
            occurrence_data %>%
            dplyr::rename_with(.fn = ~"cellCode", .cols = dplyr::all_of(col))

          # then end the loop
          break

        }

      }

      if (is.na(grid_type)) {

        # if grid cell codes could not be identified in any column, return error
        stop("Could not detect grid type. Please specify manually.")

      }

    }

    # if the user has chosen 'custom' as a grid type...
  } else if (grid_type == "custom") {

    # check if the user has provided a name for the column containing grid codes
    if (is.null(cols_cellCode)) {

      stop(paste0(
        "You have chosen custom grid type. Please provide the name of the ",
        "column containing grid cell codes."))

    }


    # check that the column name they provided exists
    if (!cols_cellCode %in% names(occurrence_data)) {

      stop(paste0("The column name you provided for grid cell codes does not ",
      "exist. Please double check that you spelled it correctly."))

    }

    # rename it to the default
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename_with(.fn = ~"cellCode",
                         .cols = dplyr::all_of(cols_cellCode))

    # if the user has chosen 'none' as a grid type...
  } else if (grid_type == "none") {

    # if the user has specified a grid type...
  } else {

    # check if the user has provided a name for the column containing grid codes
    if (is.null(cols_cellCode)) {

      # if not, try to identify it automatically (returns error if unsuccessful)
      cols_cellCode <- detect_grid_column(occurrence_data, grid_type)

    } else {

      # check that the column name they provided exists
      if (!cols_cellCode %in% names(occurrence_data)) {

        stop(paste0("The column name you provided for grid cell codes does ",
        "not exist. Please double check that you spelled it correctly."))

      }

    }

    if (force_gridcode == FALSE && grid_type != "none") {
      # Test the first non-missing cell code (missing codes are removed later)
      first_code <- stats::na.omit(occurrence_data[[cols_cellCode]])[1]

      grid_type_test <- ifelse(
        grid_type == "eea",
        stringr::str_detect(
          first_code,
          "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"
        ),
        ifelse(
          grid_type == "mgrs",
          stringr::str_detect(
            first_code,
            "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"
          ),
          ifelse(
            grid_type == "eqdgc",
            stringr::str_detect(
              first_code,
              "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"
            ),
            ifelse(
              grid_type == "isea3h",
              stringr::str_detect(
                first_code,
                "^-?[0-9]{15,}$"
              ),
              NA
            )
          )
        )
      )

      if (isFALSE(grid_type_test)) {

        stop(paste0(
          "Cell codes do not match the expected format. Are you sure you have ",
          "specified the correct grid system? It is recommended to leave ",
          "grid_type on 'automatic'. If you are certain, you can use ",
          "'force_gridcode = TRUE' to attempt to translate them anyway, but ",
          "this could lead to unexpected downstream errors."
        ))

      }

    }

    # rename it to the default
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename_with(.fn = ~"cellCode",
                         .cols = dplyr::all_of(cols_cellCode))

  }

  # make a list of other user provided column names
  col_names_userlist <- list(cols_year,
                             cols_yearMonth,
                             cols_yearMonthDay,
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
                                "yearMonthDay",
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
  col_names <- data.frame("default" = unlist(col_names_defaultlist),
                          "user" = unlist(col_names_userlist))

  # rename user-supplied column names to defaults expected by package functions
  for (i in (which(names(occurrence_data) %in% col_names[, 2]))) {
    names(occurrence_data)[i] <-
      col_names[, 1][which(col_names[, 2] %in% names(occurrence_data)[i])]
  }

  # check for any non-user-supplied column names which match the default names
  # but not the capitalization pattern and fix them
  for (i in seq_along(col_names_defaultlist)) {

    if (!col_names_defaultlist[[i]] %in% colnames(occurrence_data) &&
        tolower(col_names_defaultlist[[i]]) %in%
        tolower(colnames(occurrence_data))) {

      new_name <- col_names_defaultlist[[i]]
      old_name <- colnames(occurrence_data)[grepl(new_name,
                                                  colnames(occurrence_data),
                                                  ignore.case = TRUE)]
      occurrence_data <-
        occurrence_data %>%
        dplyr::rename_with(.fn = ~new_name, .cols = dplyr::all_of(old_name))

    }

  }

  # If year column missing but yearMonth present, convert yearMonth to year
  if (!"year" %in% colnames(occurrence_data)) {
    if ("yearMonth" %in% colnames(occurrence_data)) {
      occurrence_data <-
        occurrence_data %>%
        dplyr::mutate(year = as.numeric(stringr::str_extract(
          yearMonth, "(\\d{4})"))
        )
      # If year and yearMonth columns are both missing but yearMonthDay present,
      # convert yearMonthDay to year
    } else if ("yearMonthDay" %in% colnames(occurrence_data)) {
      occurrence_data <-
        occurrence_data %>%
        dplyr::mutate(year = as.numeric(stringr::str_extract(
          yearMonthDay, "(\\d{4})"))
        )
    } else {
      # Otherwise look for a date column (e.g. 'datetime', 'date', 'eventDate')
      date_col <- names(occurrence_data)[
        tolower(names(occurrence_data)) %in% c("datetime", "date", "eventdate")
      ]
      if (length(date_col) >= 1) {
        occurrence_data$year <- as.numeric(stringr::str_extract(
          as.character(occurrence_data[[date_col[1]]]), "(\\d{4})"))
      } else {
        stop("No year, yearMonth, yearMonthDay or date column found in cube. ",
             "Please specify column name manually.")
      }
    }
  }

  # check that years provided for filtering by year are valid
  if (!is.null(first_year)) {
    if (!is.numeric(first_year)) {
      stop("`first_year` should be a number.")
    }
    if (first_year >= max(occurrence_data$year, na.rm = TRUE)) {
      stop("`first_year` should be less than the max year in the data cube.")
    }
  }

  if (!is.null(last_year)) {
    if (!is.numeric(last_year)) {
      stop("`last_year` should be a number.")
    }
    if (last_year <= min(occurrence_data$year, na.rm = TRUE)) {
      stop("`last_year` should be greater than the min year in the data cube.")
    }
  }

  if (!is.null(first_year) && !is.null(last_year)) {
    if (last_year < first_year) {
      stop("`last_year` should not be less than `first_year`.")
    }
  }

  # If scientificName column missing but species column present, copy species
  # to scientificName
  if ("species" %in% colnames(occurrence_data) &&
      !("scientificName" %in% colnames(occurrence_data))) {
    occurrence_data <-
      occurrence_data %>%
      dplyr::rename(scientificName = species)
  }

  # check if any essential columns (required by package functions) are missing
  required_colnames <- c("year", "occurrences", "scientificName", "speciesKey")
  missing_colnames <- required_colnames[which(
    !required_colnames %in% colnames(occurrence_data)
  )]

  if (length(missing_colnames) >= 1) {
    stop(paste0(
      "\nThe following columns could not be detected in cube:",
      missing_colnames, "\nPlease supply the missing column names as ",
      "arguments to the function.\n"
    ))
  }

  numeric_cols <- c("year",
                    "occurrences",
                    "minCoordinateUncertaintyInMeters",
                    "minTemporalUncertainty",
                    "familyCount")

  # make sure that numeric columns are the correct type
  # Note: key columns (kingdomKey, familyKey, speciesKey) are intentionally
  # excluded because GBIF now uses string-based keys (e.g. "N" for Animalia)
  occurrence_data <-
    occurrence_data %>%
    dplyr::mutate(across(any_of(numeric_cols), as.numeric))

  # rename occurrences and speciesKey columns to be consistent with the other
  # package functions (should maybe change this throughout package?)
  occurrence_data <-
    occurrence_data %>%
    dplyr::rename(obs = occurrences) %>%
    dplyr::rename(taxonKey = speciesKey)

  if (grid_type != "none") {
    # Remove NA values in cell code column
    occurrence_data_filtered <-
      occurrence_data %>%
      dplyr::filter(!is.na(cellCode))
    # Check and report filtered out rows
    if (nrow(occurrence_data_filtered) != nrow(occurrence_data)) {
      n_filtered_rows <- nrow(occurrence_data) - nrow(occurrence_data_filtered)
      message("Removed ", n_filtered_rows, " rows with missing cell codes")
    }
    occurrence_data <- occurrence_data_filtered
  }

  if (!is.null(custom_grid)) {
    # Attach cell centroids and the typical cell size from the custom grid
    occurrence_data$cellCode <- as.character(occurrence_data$cellCode)
    matched <- occurrence_data$cellCode %in% custom_grid$cellCode
    if (!all(matched)) {
      unmatched <- unique(occurrence_data$cellCode[!matched])
      warning(sprintf(
        paste0("%d rows (%d grid cells, e.g. '%s') have no matching cell in ",
               "`grid` and were removed."),
        sum(!matched), length(unmatched), unmatched[1]
      ), call. = FALSE)
      occurrence_data <- occurrence_data[matched, ]
      if (nrow(occurrence_data) == 0) {
        stop("None of the cube's grid cells were found in `grid`. Please ",
             "check `grid_id` and `cols_cellCode`.", call. = FALSE)
      }
    }
    centroids <- sf::st_drop_geometry(custom_grid)[, c("cellCode", "xcoord",
                                                       "ycoord")]
    occurrence_data <- occurrence_data[, setdiff(names(occurrence_data),
                                                 c("xcoord", "ycoord"))]
    occurrence_data <- dplyr::left_join(occurrence_data, centroids,
                                        by = "cellCode")
    custom_grid <- custom_grid[custom_grid$cellCode %in%
                                 unique(occurrence_data$cellCode), ]
    occurrence_data$resolution <- custom_grid_resolution(custom_grid)
  }

  if (grid_type == "eea") {
    if (force_gridcode == FALSE) {
      if (!ifelse(
        stringr::str_detect(
          occurrence_data$cellCode[1],
          "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"
        ),
        TRUE,
        FALSE
      )) {
        stop(paste0(
          "Cell codes do not match the expected format. Are you sure you have ",
          "specified the correct grid system? It is recommended to leave ",
          "grid_type on 'automatic'. If you are certain, you can use ",
          "'force_gridcode = TRUE' to attempt to translate them anyway, but ",
          "this could lead to unexpected downstream errors."
        ))
      }
    }

    occurrence_data <-
      occurrence_data %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "W", "W-")) %>%
      dplyr::mutate(cellCode = stringr::str_replace(cellCode, "S", "S-"))

    occurrence_data <- occurrence_data %>%
      dplyr::bind_cols(
        eea_code_to_coords(.$cellCode) %>%
          dplyr::select(-cellCode)
      )

  } else if (grid_type == "mgrs") {
    if (force_gridcode == FALSE) {
      if (!ifelse(
        stringr::str_detect(
          occurrence_data$cellCode[1],
          "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"
        ),
        TRUE,
        FALSE
      )) {
        stop(paste0(
          "Cell codes do not match the expected format. Are you sure you have ",
          "specified the correct grid system? It is recommended to leave ",
          "grid_type on 'automatic'. If you are certain, you can use ",
          "'force_gridcode = TRUE' to attempt to translate them anyway, but ",
          "this could lead to unexpected downstream errors."
        ))
      }
    }

    utm <- mgrs_to_utm(occurrence_data$cellCode)
    occurrence_data$xcoord <- utm$easting
    occurrence_data$ycoord <- utm$northing
    occurrence_data$utmzone <- utm$zone
    occurrence_data$hemisphere <- utm$hemisphere

    # Resolution follows from the number of digits after the 100 km square
    # letters (0 digits = 100 km, 2 = 10 km, 4 = 1 km, ...). Use the most
    # common value in case a few codes are malformed.
    n_digits <- nchar(gsub("^\\s*[0-9]{1,2}[A-Za-z]{3}|\\s", "",
                           occurrence_data$cellCode))
    n_digits <- as.numeric(names(which.max(table(n_digits))))
    occurrence_data$resolution <- paste0(10^(2 - n_digits / 2), "km")

  } else if (grid_type == "eqdgc") {
    if (force_gridcode == FALSE) {
      if (!ifelse(
        stringr::str_detect(
          occurrence_data$cellCode[1],
          "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"
        ),
        TRUE,
        FALSE
      )) {
        stop(paste0(
          "Cell codes do not match the expected format. Are you sure you have ",
          "specified the correct grid system? It is recommended to leave ",
          "grid_type on 'automatic'. If you are certain, you can use ",
          "'force_gridcode = TRUE' to attempt to translate them anyway, but ",
          "this could lead to unexpected downstream errors."
        ))
      }
    }

    # Determine the resolution from the cellCode length (e.g., 0.125 for 1/8 degree)
    resolution_deg <- 1 / (2^(nchar(occurrence_data$cellCode[1]) - 7))
    # Calculate the required shift (half the cell size) to move from corner to center

    # 2. Convert cell codes to bottom-left coordinates
    latlong <- convert_eqdgc_latlong(occurrence_data$cellCode)

    # 3. Apply the shift to convert corner coordinates to cell centers (centroids)
    lat <- latlong[, 1]
    long <- latlong[, 2]

    occurrence_data$xcoord <- long
    occurrence_data$ycoord <- lat
    occurrence_data$resolution <- rep(paste0(
     resolution_deg, "degrees"
    ), nrow(occurrence_data))

  } else if (grid_type == "isea3h") {
    if (force_gridcode == FALSE) {
      # Basic validation for ISEA3H codes (long numeric strings)
      if (!ifelse(
        stringr::str_detect(occurrence_data$cellCode[1], "^-?[0-9]{15,}$"),
        TRUE,
        FALSE
      )) {
        stop(paste0(
          "Cell codes do not match the expected format for ISEA3H. ",
          "Are you sure you have specified the correct grid system?"
        ))
      }
    }

    # Convert cell codes to coordinates
    coords <- isea3h_code_to_coords(occurrence_data$cellCode)
    occurrence_data$xcoord <- coords$xcoord
    occurrence_data$ycoord <- coords$ycoord
    occurrence_data$resolution <- coords$resolution
  }

  if (min(occurrence_data$year, na.rm = TRUE)==max(occurrence_data$year,
                                                   na.rm = TRUE)) {
    first_year <- min(occurrence_data$year)
    last_year <- first_year
    warning(paste0("Cannot create trends with this dataset, as occurrences ",
    "are all from the same year."))
  } else {

    # Check whether start and end years are within dataset
    first_year <- occurrence_data %>%
      dplyr::select(year) %>%
      min(na.rm = TRUE) %>%
      ifelse(is.null(first_year),
             .,
             ifelse(first_year > ., first_year, .))
    last_year <- occurrence_data %>%
     # dplyr::summarize(max_year = max(year, na.rm = TRUE)-1) %>%
      dplyr::summarize(max_year = max(year, na.rm = TRUE)) %>%
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

  if (!is.null(custom_grid)) {
    cube <- new_processed_cube(occurrence_data, grid_type)
    custom_grid <- custom_grid[custom_grid$cellCode %in%
                                 unique(occurrence_data$cellCode), ]
    bb <- sf::st_bbox(custom_grid)
    cube$coord_range <- list(xmin = unname(bb["xmin"]),
                             xmax = unname(bb["xmax"]),
                             ymin = unname(bb["ymin"]),
                             ymax = unname(bb["ymax"]))
    cube$grid <- custom_grid
  } else if (grid_type == "none" || grid_type == "custom") {
    cube <- new_sim_cube(occurrence_data, grid_type)
  } else {
    cube <- new_processed_cube(occurrence_data, grid_type)
  }
  return(cube)
}
