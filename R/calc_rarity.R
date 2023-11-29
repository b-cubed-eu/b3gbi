#' Calculate rarity trend
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param method A character vector.
#'   "observed" calculates the observed rarity trend from the cube data,
#'
#' @return A tibble
#' @export
#'
calc_rarity <- function(data,
                        method = "grid",
                        type = "area",
                        cs1 = 100,
                        cs2 = 100,
                        level = "country",
                        region = "Denmark") {

  # Put year names into a vector
  year_names <- unique(data$year)

  if (method == "year") {

  # Calculate rarity for each species by year
  rarity_year_taxon <-
    data %>%
    dplyr::mutate(records_year = sum(obs), .by = year) %>%
    dplyr::mutate(records_taxon = sum(obs), .by = c(year, taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (records_taxon / records_year)) %>%
    dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
    dplyr::select(year, scientificName, rarity)

    return(rarity_year_taxon)

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
    occ_grid_int <- sf::st_intersection(occ_sf, grid, left = TRUE)

    # Add cell numbers to occurrence data
    data_cells <-
      data_scaled %>%
      dplyr::inner_join(occ_grid_int)

    if (type == "abundance") {

      # Calculate total summed rarity (in terms of abundance) for each grid cell
      ab_rarity_cell <-
        data_cells %>%
        dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
        dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
        dplyr::summarise(diversity_val = sum(rarity), .by = "cellid") %>%
        tibble::add_column(diversity_type = "ab_rarity") %>%
        dplyr::arrange(cellid)

      # Add abundance-based rarity to grid
      ab_rarity_grid <-
        grid %>%
        dplyr::left_join(ab_rarity_cell, by = "cellid")

    } else if (type == "area") {

      # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
      # frequency for each species
      area_rarity_cell <-
        data_cells %>%
        dplyr::mutate(rec_tax_cell = sum(n_distinct(cellid)),
                      .by = c(taxonKey)) %>%
        dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(n_distinct(cellid)))) %>%
        dplyr::summarise(diversity_val = sum(rarity), .by = cellid) %>%
        tibble::add_column(diversity_type = "area_rarity")

      # Add grid-based rarity to grid
      area_rarity_grid <-
        grid %>%
        dplyr::left_join(area_rarity_cell, by = "cellid")

    }

  }

}
