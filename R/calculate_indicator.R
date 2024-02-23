calculate_indicator <- function(x,
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
indicator <- calc_map(data, ...)

# Add grid-based rarity to grid
diversity_grid <-
  grid %>%
  dplyr::left_join(diversity_cell, by = "cellid")

if (!inherits(x, "virtual_cube")) {

  diversity_obj <- indicator_map(diversity_grid,
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

  diversity_obj <- v_indicator_map(diversity_grid,
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
