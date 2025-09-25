#' @title 'indicator_map' S3 Constructor
#'
#' @description This function creates an 'indicator_map' S3 object, a
#' specialized structure designed for storing spatial biodiversity indicator
#' results. This includes metadata about the calculation and the indicator
#' values mapped onto geographic cells.
#'
#' @param x An sf data frame containing at least two columns:
#'   * 'cellid': Unique ID for each spatial cell.
#'   * 'diversity_val': The calculated indicator value for the cell.
#' @param div_type The type of biodiversity indicator in short form (e.g.,
#'  obs_richness").
#' @param cell_size Length of the grid cell sides, in kilometers.
#' @param map_level The spatial level of the map (e.g., "country", "continent",
#'  "world").
#' @param map_region The name of the spatial region under analysis.
#' @param kingdoms A character vector of kingdoms included in the analysis.
#' @param num_species The total number of species in the dataset.
#' @param first_year The first year of the indicator calculation timeframe.
#' @param last_year The last year of the indicator calculation timeframe.
#' @param num_years The number of years in the time series.
#'
#' @return An 'indicator_map' S3 object containing:
#'   * **Indicator name and type:**  Descriptive name of indicator (e.g.
#'     "Observed Species Richness") and short-form (e.g. "obs_richness").
#'   * **Cell information:** Number of cells, cell size (length x width).
#'   * **Spatial details:** Level (country, continent, world), region name,
#'     projection, and coordinate range.
#'   * **Diversity information:** Kingdoms, number of species, and names of
#'     species.
#'   * **Years analyzed:** First and last year, number of years.
#'   * **Mapped Results:** The input sf object, now containing indicator scores.
#'
#' @noRd
new_indicator_map <- function(x,
                          div_type,
                          cell_size,
                          cell_size_units,
                          map_level,
                          map_region,
                          map_type,
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
  coord_range <- sf::st_bbox(x)[c("xmin", "ymin", "xmax", "ymax")]
  if (cell_size_units == "km") cell_size_units <- "km^2"
  cell_size <- paste(cell_size, cell_size_units)
  id <- div_type
  class(x) <- c("indicator_data", class(x))
  structure(list(
    div_name = get_indicator_name(id),
    div_type = div_type,
    num_cells = length(x$cellid),
    cell_size = cell_size,
    map_level = map_level,
    map_region = map_region,
    map_type = map_type,
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
    data = x
  ),
  class = c("indicator_map", div_type),
  indicator_id = id,
  type = "map")
}
