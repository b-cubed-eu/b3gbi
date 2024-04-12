#' @title 'processed_cube' S3 Constructor
#'
#' @description This function constructs a 'processed_cube' S3 object, a specialized data
#' structure designed for biodiversity analysis within this package. It validates the input
#' data cube, calculates essential summary information, and prepares the object for further use.
#'
#' @param x A tibble data cube containing occurrence data.
#'
#' @return  A 'processed_cube' S3 object containing:
#'   * **Summary statistics:** First and last year of data, coordinate range, number of
#'     spatial cells, species, and observations.
#'   * **Diversity information:** Kingdoms represented, resolutions, indication of multiple
#'     resolutions and datasets, and types of occurrence records.
#'   * **Original data:** The input data cube.
#'
#' @note 'processed_cube' objects are used by various analysis functions within this package.
#'
#' @noRd
new_processed_cube <- function(x) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "eea_cell_code",
                  "taxonKey",
                  "obs",
                  "scientificName",
                  "xcoord",
                  "ycoord",
                  "resolution") %in% names(x)))
  if (all(c("datasetKey", "basisOfRecord") %in% names(x))) {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = list("xmin" = min(x$xcoord),
                                      "xmax" = max(x$xcoord),
                                      "ymin" = min(x$ycoord),
                                      "ymax" = max(x$ycoord)),
                   num_cells = length(unique(x$eea_cell_code)),
                   num_species = length(unique(x$taxonKey)),
                   num_obs = sum(x$obs),
                   kingdoms = ifelse("kingdom" %in% colnames(x), c(unique(x$kingdom)), "Data not present"),
                   num_families = ifelse("family" %in% colnames(x), length(unique(x$family)), "Data not present"),
                   resolutions = unique(x$resolution),
                   multi_res = ifelse(length(unique(x$resolution)) > 1, TRUE, FALSE),
                   num_datasets = length(unique(x$datasetKey)),
                   record_types = unique(x$basisOfRecord),
                   data = x),
              class = "processed_cube_dsinfo")
  } else {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = list("xmin" = min(x$xcoord),
                                      "xmax" = max(x$xcoord),
                                      "ymin" = min(x$ycoord),
                                      "ymax" = max(x$ycoord)),
                   num_cells = length(unique(x$eea_cell_code)),
                   num_species = length(unique(x$taxonKey)),
                   num_obs = sum(x$obs),
                   kingdoms = ifelse("kingdom" %in% colnames(x), c(unique(x$kingdom)), "Data not present"),
                   num_families = ifelse("family" %in% colnames(x), length(unique(x$family)), "Data not present"),
                   resolutions = unique(x$resolution),
                   multi_res = ifelse(length(unique(x$resolution)) > 1, TRUE, FALSE),
                   data = x),
              class = "processed_cube")
  }
}


#' @title 'indicator_ts' S3 Constructor
#'
#' @description This function creates an 'indicator_ts' S3 object, a specialized structure
#' for storing biodiversity indicator results calculated over time. These objects include
#' essential metadata and the calculated indicator time series.
#'
#' @param x A tibble data frame containing at least two columns:
#'   * 'year'
#'   * 'diversity_val' (calculated indicator value)
#' @param div_type The type of biodiversity indicator in short form (e.g., "obs_richness").
#' @param kingdoms A character vector of kingdoms included in the analysis.
#' @param num_species The total number of species in the dataset.
#' @param num_years The number of years in the time series.
#' @param species_names A character vector of species names, if a per-species indicator was calculated.
#' @param map_level The spatial level of the data (e.g., "country", "continent", "world").
#' @param map_region The name of the spatial region under analysis.
#' @param coord_range A named list specifying the coordinate range of the analysis
#' (elements: xmin, xmax, ymin, ymax).
#'
#' @return An 'indicator_ts' S3 object containing:
#'   * **Indicator name and type:**  Descriptive name of indicator (e.g., "Observed Species Richness") and short-form (e.g., "obs_richness").
#'   * **Data timeframe:** First year, last year, number of years.
#'   * **Spatial details:** Level (country, continent, world), region name, and coordinate range.
#'   * **Diversity information:**  Kingdoms, names of species, number of species.
#'   * **Time Series:** The input tibble containing year and indicator values.
#'
#' @noRd
new_indicator_ts <- function(x,
                         div_type,
                         map_level,
                         map_region,
                         kingdoms,
                         num_families,
                         num_species,
                         num_years,
                         species_names,
                         coord_range) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "diversity_val") %in% names(x)))
    id = div_type
  class(x) <- c("indicator_data", class(x))
  structure(list(div_name = get_indicator_name(id),
                 div_type = div_type,
                 first_year = min(x$year),
                 last_year = max(x$year),
                 num_years = num_years,
                 num_species = num_species,
                 map_level = map_level,
                 map_region = map_region,
                 kingdoms = kingdoms,
                 num_families = num_families,
                 coord_range = coord_range,
                 species_names = species_names,
                 data = x),
            class = c("indicator_ts", div_type),
            indicator_id = id,
            type = "ts")
}


#' @title 'indicator_map' S3 Constructor
#'
#' @description This function creates an 'indicator_map' S3 object, a specialized structure
#' designed for storing spatial biodiversity indicator results. This includes metadata about
#' the calculation and the indicator values mapped onto geographic cells.
#'
#' @param x An sf data frame containing at least two columns:
#'   * 'cellid': Unique ID for each spatial cell.
#'   * 'diversity_val': The calculated indicator value for the cell.
#' @param div_type The type of biodiversity indicator in short form (e.g., obs_richness").
#' @param cell_size Length of the grid cell sides, in kilometers.
#' @param map_level The spatial level of the map (e.g., "country", "continent", "world").
#' @param map_region The name of the spatial region under analysis.
#' @param kingdoms A character vector of kingdoms included in the analysis.
#' @param num_species The total number of species in the dataset.
#' @param first_year The first year of the indicator calculation timeframe.
#' @param last_year The last year of the indicator calculation timeframe.
#' @param num_years The number of years in the time series.
#'
#' @return An 'indicator_map' S3 object containing:
#'   * **Indicator name and type:**  Descriptive name of indicator (e.g. "Observed Species Richness") and short-form (e.g. "obs_richness").
#'   * **Cell information:** Number of cells, cell size (length x width).
#'   * **Spatial details:** Level (country, continent, world), region name, projection, and coordinate range.
#'   * **Diversity information:** Kingdoms, number of species, and names of species.
#'   * **Years analyzed:** First and last year, number of years.
#'   * **Mapped Results:** The input sf object, now containing indicator scores.
#'
#' @noRd
new_indicator_map <- function(x,
                          div_type,
                          cell_size,
                          map_level,
                          map_region,
                          kingdoms,
                          num_families,
                          num_species,
                          first_year,
                          last_year,
                          num_years,
                          species_names,
                          years_with_obs) {
  # check that x is both a data frame and sf object
  # and all necessary columns are present
  stopifnot(inherits(x, c("sf", "data.frame")),
            all(c("cellid",
                  "geometry") %in% names(x)))
  coord_range = sf::st_bbox(x)
 # names(coord_range) = c("xmin", "ymin", "xmax", "ymax")
  cell_size = paste(cell_size, "km^2")
  id = div_type
  class(x) <- c("indicator_data", class(x))
  structure(list(div_name = get_indicator_name(id),
                 div_type = div_type,
                 num_cells = length(x$cellid),
                 cell_size = cell_size,
                 map_level = map_level,
                 map_region = map_region,
                 projection = sf::st_crs(x$geometry)$input,
                 coord_range = coord_range,
                 first_year = first_year,
                 last_year = last_year,
                 num_years = num_years,
                 num_species = num_species,
                 kingdoms = kingdoms,
                 num_families = num_families,
                 species_names = species_names,
                 years_with_obs = years_with_obs,
                 data = x),
            class = c("indicator_map", div_type),
            indicator_id = id,
            type = "map")
}
