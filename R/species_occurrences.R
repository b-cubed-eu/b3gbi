#' Plot gridded occurrences for individual species
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param method A character vector.
#'
#' @return
#' @export
#'
#' @examples
#' species_occurrences(processed_cube, "5573439")
species_occurrences <- function(data, species) {

  # Put year names into a vector
  year_names <- unique(data$year)

  if (method == "year") {

    # Calculate rarity for each species by year
    occurrences_year_taxon <-
      data %>%
      dplyr::mutate(occurrences = sum(obs), .by = c(year, taxonKey)) %>%
      dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
      dplyr::select(year, scientificName, occurrences)

    return(occurrences_year_taxon)

  } else if (method == "grid") {

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
    occ_grid_int <- sf::st_intersection(occ_sf, grid)

    # Add cell numbers to occurrence data
    data_cells <-
      data_scaled %>%
      dplyr::inner_join(occ_grid_int, keep = FALSE)

    # Calculate total summed rarity (in terms of abundance) for each grid cell
    occurrence_cell <-
      data_cells %>%
      dplyr::mutate(num_records = sum(obs), .by = c(taxonKey, cellid))# %>%
      dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
      dplyr::arrange(cellid) %>%
      dplyr::select(cellid, taxonKey, scientificName, num_records)

    # Add abundance-based rarity to grid
    occurrence_grid <-
      grid %>%
      dplyr::left_join(occurrence_cell, by = "cellid")


  }

}
