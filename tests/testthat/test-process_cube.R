test_that("process_cube handles valid file input", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_no_error(process_cube(cube_file))
  result <- process_cube(cube_file)
  expect_s3_class(result, "processed_cube")
  important_cols <- c("cellCode",
                      "obs",
                      "taxonKey",
                      "scientificName",
                      "year")
  expect_true(all(important_cols %in% names(result$data)))
})

test_that("process_cube handles valid data frame input", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  expect_no_error(process_cube(cube_df))
  result <- process_cube(cube_df)
  expect_s3_class(result, "processed_cube")
})

test_that("process_cube errors on invalid cube_name type", {
  expect_error(process_cube(123),
               "`cube_name` should be a file path or dataframe.")
  expect_error(process_cube(list()),
               "`cube_name` should be a file path or dataframe.")
})

test_that("process_cube handles valid grid_type arguments", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_no_error(process_cube(cube_file, grid_type = "automatic"))
  expect_error(process_cube(cube_file, grid_type = "eea"))
  expect_error(process_cube(cube_file, grid_type = "mgrs"))
  expect_no_error(process_cube(cube_file, grid_type = "eqdgc"))
  expect_error(process_cube(cube_file, grid_type = "custom"))
  expect_no_error(process_cube(cube_file, grid_type = "none"))
})

test_that("process_cube errors on invalid grid_type argument", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file, grid_type = "invalid"),
               "'arg' should be one of")
})

test_that("process_cube renames columns when cols_year is provided", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, recordYear = year)
  result <- process_cube(cube_df, cols_year = "recordYear")
  expect_true("year" %in% names(result$data))
  expect_false("recordYear" %in% names(result$data))
})

test_that("process_cube errors if provided cols_year does not exist", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            cols_year = "wrong_year_col"))
})

test_that("process_cube converts yearMonth to year", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, yearMonth = year)
  result <- process_cube(cube_df)
  expect_true("year" %in% names(result$data))
  expect_false("yearMonth" %in% names(result$data))
  expect_type(result$data$year, "double")
})

test_that("process_cube renames cellCode when cols_cellCode is provided", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, gridCode = eqdgccellcode)
  result <- process_cube(cube_df, cols_cellCode = "gridCode")
  expect_true("cellCode" %in% names(result$data))
  expect_false("gridCode" %in% names(result$data))
})

test_that("process_cube errors if provided cols_cellCode does not exist", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            cols_cellCode = "wrong_cell_col"))
})

test_that(
  "process_cube renames occurrences when cols_occurrences is provided", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, abundance = occurrences)
  result <- process_cube(cube_df, cols_occurrences = "abundance")
  expect_true("obs" %in% names(result$data))
  expect_false("abundance" %in% names(result$data))
})

test_that("process_cube errors if provided cols_occurrences does not exist", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            cols_occurrences = "wrong_obs_col"))
})

test_that(paste0("process_cube renames scientificName when ",
                 "cols_scientificName is provided"), {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, taxonName = species)
  result <- process_cube(cube_df, cols_scientificName = "taxonName")
  expect_true("scientificName" %in% names(result$data))
  expect_false("taxonName" %in% names(result$data))
})

test_that(
  "process_cube errors if provided cols_scientificName does not exist", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            cols_scientificName = "wrong_sci_name_col"))
})

test_that(
  "process_cube renames species to scientificName if scientificName is missing"
, {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file)
  expect_true("scientificName" %in% names(result$data))
  expect_false("species" %in% names(result$data))
})

test_that("process_cube renames speciesKey when cols_speciesKey is provided", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, taxonID = specieskey)
  result <- process_cube(cube_df, cols_speciesKey = "taxonID")
  expect_true("taxonKey" %in% names(result$data))
  expect_false("taxonID" %in% names(result$data))
})

test_that("process_cube errors if provided cols_speciesKey does not exist", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            cols_speciesKey = "wrong_species_key_col"))
})

test_that("process_cube errors when essential columns are missing", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df_no_year <- select(cube_df, -year)
  expect_error(process_cube(cube_df_no_year),
               "\nThe following columns could not be detected in cube:year")
  cube_df_no_obs <- select(cube_df, -occurrences)
  expect_error(
    process_cube(cube_df_no_obs),
    "\nThe following columns could not be detected in cube:occurrences")
  cube_df_no_sci_name <- select(cube_df, -species)
  expect_error(
    process_cube(cube_df_no_sci_name),
    "\nThe following columns could not be detected in cube:scientificName")
  cube_df_no_species_key <- select(cube_df, -specieskey)
  expect_error(
    process_cube(cube_df_no_species_key),
    "\nThe following columns could not be detected in cube:speciesKey")
})

test_that(
  "process_cube automatically detects EEA grid and processes coordinates", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eea.csv",
                           package = "b3gbi") # Contains EEA grid codes
  result <- process_cube(cube_file, grid_type = "automatic")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
  expect_type(result$data$xcoord, "double")
  expect_type(result$data$ycoord, "double")
  expect_type(result$data$resolution, "character")
})

test_that("process_cube handles EEA grid when specified", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eea.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file, grid_type = "eea")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
})

test_that("process_cube errors on invalid EEA grid codes", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, eeacellcode = eqdgccellcode)
  expect_error(process_cube(cube_df,
                            grid_type = "eea",
                            cols_cellCode = "eeacellcode",
                            force_gridcode = FALSE),
               paste0("Cell codes do not match the expected format. Are you ",
               "sure you have specified the correct grid system?"))
})

test_that(paste0("process_cube automatically detects MGRS grid and processes ",
                 "coordinates"), {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_mgrs.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file, grid_type = "automatic")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
  expect_type(result$data$xcoord, "double")
  expect_type(result$data$ycoord, "double")
  expect_type(result$data$resolution, "character")
})

test_that("process_cube handles MGRS grid when specified", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_mgrs.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file, grid_type = "mgrs")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
})

test_that("process_cube errors on invalid MGRS grid codes", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, mgrscellcode = eqdgccellcode)
  expect_error(process_cube(cube_df,
                            grid_type = "mgrs",
                            cols_cellCode = "mgrscellcode",
                            force_gridcode = FALSE),
               paste0("Cell codes do not match the expected format. Are you ",
               "sure you have specified the correct grid system?"))
})

test_that(paste0("process_cube automatically detects EQDGC grid and processes ",
                 "coordinates"), {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file, grid_type = "automatic")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
  expect_type(result$data$xcoord, "double")
  expect_type(result$data$ycoord, "double")
  expect_type(result$data$resolution, "character")
})

test_that("process_cube handles EQDGC grid when specified", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  result <- process_cube(cube_file, grid_type = "eqdgc")
  expect_true("xcoord" %in% names(result$data))
  expect_true("ycoord" %in% names(result$data))
  expect_true("resolution" %in% names(result$data))
})

test_that("process_cube errors on invalid EQDGC grid codes", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eea.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, eqdgccellcode = eeacellcode)
  expect_error(process_cube(cube_df,
                            grid_type = "eqdgc",
                            cols_cellCode = "eqdgccellcode",
                            force_gridcode = FALSE),
               paste0("Cell codes do not match the expected format. Are you ",
               "sure you have specified the correct grid system?"))
})

##***
test_that(paste0(
  "process_cube handles custom grid type when cols_cellCode is provided"
), {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- rename(cube_df, customCode = eqdgccellcode)
  cube_df$customCode <- "customCode"
  result <- process_cube(cube_df,
                         grid_type = "custom",
                         cols_cellCode = "customCode")
  expect_true("cellCode" %in% names(result$data))
  expect_false("customCode" %in% names(result$data))
})

test_that("process_cube errors on custom grid type without cols_cellCode", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_error(process_cube(cube_file,
                            grid_type = "custom"),
               paste0("You have chosen custom grid type. Please provide the ",
               "name of the column containing grid cell codes."))
})

test_that("process_cube handles 'none' grid type", {
  cube_file <- system.file("extdata",
                           "denmark_mammals_cube_eqdgc.csv",
                           package = "b3gbi")
  expect_no_error(process_cube(cube_file, grid_type = "none"))
  result <- process_cube(cube_file, grid_type = "none")
  expect_true(!"cellCode" %in% names(result$data))
})

test_that("process_cube filters by first_year and last_year (continued)", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df_filtered <- filter(cube_df, year >= 2000 & year <= 2005)

  result_first_year <- process_cube(cube_df_filtered,
                                    first_year = 2003)
  expect_true(all(result_first_year$year >= 2003))

  result_last_year <- process_cube(cube_df_filtered,
                                   last_year = 2003)
  expect_true(all(result_last_year$year <= 2003))

  expect_error(process_cube(cube_df_filtered, first_year = 2010))
  expect_error(process_cube(cube_df_filtered, last_year = 1988))
  expect_error(process_cube(cube_df_filtered,
                            first_year = 2010,
                            last_year = 2012))
  expect_error(process_cube(cube_df_filtered,
                            first_year = 1988,
                            last_year = 1998))
  expect_error(process_cube(cube_df_filtered,
                            first_year = "two-thousand-one"))
  expect_error(process_cube(cube_df_filtered,
                            last_year = "two-thousand-three"))
  expect_no_error(process_cube(cube_df_filtered,
                               first_year = 2000,
                               last_year = 2005))
  result_defaults <- process_cube(cube_df)
  cube_data <- readr::read_delim(system.file("extdata",
                                             "denmark_mammals_cube_eqdgc.csv",
                                             package = "b3gbi"),
                                 delim = "\t",
                                 na = "",
                                 show_col_types = FALSE
  )
  expect_equal(min(result_defaults$data$year),
               min(cube_data$year, na.rm = TRUE))
  expect_equal(max(result_defaults$data$year),
               max(cube_data$year, na.rm = TRUE))
})

test_that("process_cube handles single year data without error and warns", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- filter(cube_df, year >= 2000 & year <= 2000)
  expect_warning(process_cube(cube_df),
                 paste0(
                   "Cannot create trends with this dataset, as occurrences ",
                   "are all from the same year."
                   )
  )
  result <- suppressWarnings(process_cube(cube_df))
  expect_equal(min(result$data$year), max(result$data$year))
})

test_that("process_cube removes NA values in cellCode (when not 'none')", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df$eqdgccellcode[1:5] <- NA
  result <- process_cube(cube_df)
  expect_false(any(is.na(result$data$cellCode)))
  result_none <- process_cube(cube_df, grid_type = "none")
  expect_true(any(is.na(result_none$data$eqdgccellcode)))
})

test_that("process_cube removes duplicate rows", {
  cube_df <- readr::read_delim(system.file("extdata",
                                           "denmark_mammals_cube_eqdgc.csv",
                                           package = "b3gbi"),
                               delim = "\t",
                               na = "",
                               show_col_types = FALSE
  )
  cube_df <- cube_df[complete.cases(cube_df),]
  cube_df[6:10,] <- cube_df[1:5,]
  result <- process_cube(cube_df)
  original_data <- readr::read_delim(
    system.file("extdata",
                "denmark_mammals_cube_eqdgc.csv",
                package = "b3gbi"),
    delim = "\t",
    na = "",
    show_col_types = FALSE
  )
  original_data <- original_data[complete.cases(original_data),]
  expect_lt(nrow(result$data), nrow(original_data))
  expect_equal(nrow(result$data), nrow(dplyr::distinct(cube_df)))
})

test_that("process_cube handles empty data frame", {
  empty_df <- data.frame()
  expect_error(process_cube(empty_df),
               "The data cube is empty. Please check the file.")
})
