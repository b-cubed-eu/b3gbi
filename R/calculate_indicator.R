#' @title Calculate Gridded Biodiversity Indicators
#'
#' @description This function provides a flexible framework for calculating various biodiversity
#' indicators on a spatial grid. It prepares the data, creates a grid, calculates indicators,
#' and formats the output into an appropriate S3 object ('indicator_map' or 'virtual_indicator_map').
#'
#' @param x A data cube object ('processed_cube', 'processed_cube_dsinfo', or  'virtual_cube').
#' @param type The indicator to calculate. Supported options include:
#'   * 'hill0', 'hill1', 'hill2': Hill numbers (order 0, 1, and 2).
#'   * 'obs_richness': Observed species richness.
#'   * 'total_occ': Total number of occurrences.
#'   * 'newness': Mean year of occurrence.
#'   * 'density': Density of occurrences.
#'   * 'e9_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity scores.
#'   * 'spec_occ': Species occurrences.
#'   * 'tax_distinct': Taxonomic distinctness.
#' @param cs1 Length of the grid cell's sides, in kilometers, if square. (Default: 100)
#' @param cs2 Width of the grid cell's sides, in kilometers. (Default: 100)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param ... Additional arguments passed to specific indicator calculation functions.
#'
#' @return An S3 object of the appropriate class containing the calculated indicator values and metadata:
#'   * 'indicator_map' for real-world observational data.
#'   * 'virtual_indicator_map' for virtual species data.
#'
#' @examples
#' # Assuming 'my_data_cube' is a 'processed_cube' or 'virtual_cube' object
#' diversity_map <- calculate_indicator(my_data_cube, type = "obs_richness", level = "continent", region = "Africa")
#'
#' @export
calc_indicator.map <- function(x,
                                type,
                                cs1 = 100,
                                cs2 = 100,
                                level = c("continent", "country", "world"),
                                region = "Europe",
                                ...) {

stopifnot_error("Object class not recognized.",
                inherits(x, "processed_cube") |
                  inherits(x, "processed_cube_dsinfo") |
                  inherits(x, "virtual_cube"))

level <- match.arg(level)
type <- match.arg(type,
                  c("hill0",
                    "hill1",
                    "hill2",
                    "obs_richness",
                    "total_occ",
                    "newness",
                    "density",
                    "e9_evenness",
                    "pielou_evenness",
                    "ab_rarity",
                    "area_rarity",
                    "spec_occ",
                    "spec_range",
                    "tax_distinct"))

data <- x$data

# Collect information to add to final object
num_species <- x$num_species
first_year <- x$first_year
last_year <- x$last_year
num_years <- length(unique(data$year))

if (!inherits(x, "virtual_cube")) {

  kingdoms <- x$kingdoms
  species_names <- unique(data$scientificName)
  years_with_obs <- unique(data$year)

}

# Download Natural Earth data
map_data <- get_NE_data(level, region)

# Create grid from Natural Earth data
grid <- create_grid(map_data, cs1, cs2)

# Format spatial data and merge with grid
data <- prepare_spatial_data(data, grid)

# Calculate indicator
indicator <- calc_map(data, type = type, ...)

# Add grid-based rarity to grid
diversity_grid <-
  grid %>%
  dplyr::left_join(indicator, by = "cellid")

if (!inherits(x, "virtual_cube")) {

  diversity_obj <- new_indicator_map(diversity_grid,
                                 div_type = type,
                                 cs1 = cs1,
                                 cs2 = cs2,
                                 map_level = level,
                                 map_region = region,
                                 kingdoms = kingdoms,
                                 num_species = num_species,
                                 first_year = first_year,
                                 last_year = last_year,
                                 num_years = num_years,
                                 species_names = species_names,
                                 years_with_obs = years_with_obs)

} else {

  diversity_obj <- new_virtual_indicator_map(diversity_grid,
                                   div_type = type,
                                   cs1 = cs1,
                                   cs2 = cs2,
                                   map_level = level,
                                   map_region = region,
                                   num_species = num_species,
                                   first_year = first_year,
                                   last_year = last_year,
                                   num_years = num_years)

}

return(diversity_obj)

}
