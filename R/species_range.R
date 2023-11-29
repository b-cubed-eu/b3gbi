#' Calculate occurrences for individual species over time or across a grid
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param method A character vector.
#'
#' @return ?
#' @export
#'
species_range <- function(data, level = "country", region = "Germany", cs1 = 10, cs2 = 10) {

    # Download and prepare Natural Earth map data
    if (level == "country") {

      map_data <- rnaturalearth::ne_countries(scale = "medium",
                                              country = region) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "EPSG:3035")

    } else if (level == "continent") {

      map_data <- rnaturalearth::ne_countries(scale = "medium",
                                              continent = region) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "EPSG:3035")

    } else if (level == "world") {

      map_data <- rnaturalearth::ne_countries(scale = "medium") %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "EPSG:3035")

    }

    # Make a grid across the map area
    grid <- map_data %>%
      sf::st_make_grid(cellsize = c(cs1 * 1000, cs2 * 1000)) %>%
      sf::st_intersection(map_data) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_sf() %>%
      dplyr::mutate(cellid = row_number())

    # Add area column to grid
    grid$area_km2 <-
      grid %>%
      sf::st_area() %>%
      units::set_units("km^2")

    # Set map limits
    map_lims <- sf::st_buffer(grid, dist = 1000) %>%
      sf::st_bbox()

    # Scale coordinates of occurrences so the number of digits matches map
    data_scaled <-
      data %>%
      dplyr::mutate(xcoord = xcoord * 1000,
                    ycoord = ycoord * 1000)

    # Convert the x and y columns to the correct format for plotting with sf
    occ_sf <- sf::st_as_sf(data_scaled,
                           coords = c("xcoord", "ycoord"),
                           crs = "EPSG:3035")

    # Calculate intersection between occurrences and grid cells
    occ_grid_int <- sf::st_intersection(occ_sf, grid, left = TRUE)

    # Add cell numbers to occurrence data
    data_cell <-
      data_scaled %>%
      dplyr::inner_join(occ_grid_int) %>%
      dplyr::arrange(cellid)

    # Calculate total occurrences for each species by grid cell
    occurrence_cell <-
      data_cell %>%
      dplyr::mutate(obs = 1) %>%
      dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
      dplyr::arrange(cellid) %>%
      dplyr::select(cellid, taxonKey, scientificName, obs) %>%
      tibble::add_column(diversity_type = c("spec_range"))

    # Add total occurrences to grid
     occurrence_grid <-
       grid %>%
       dplyr::left_join(occurrence_cell, by = "cellid")

}
