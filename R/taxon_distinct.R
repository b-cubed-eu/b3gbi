taxon_distinct <- function(data, method = "grid", level = "continent", region = "Europe", cs1 = 100, cs2 = 100) {

  # Put year names into a vector
  year_names <- unique(data$year)

  if (method == "year") {

    # Retrieve taxonomic data from GBIF
    #  tax_hier <- classification(unique(data$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

    # Save data
    #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

    tax_hier <- readRDS("taxonomic_hierarchy.RDS")

    # Calculate total occurrences for each species by year
    tax_distinct_year <-
      data %>%
      tibble::add_column(diversity_val = NA) %>%
      dplyr::group_split(year) %>%
      purrr::map(. %>%
                   dplyr::mutate(diversity_val = tax_distinct_fn(.,
                                                                 tax_hier))) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(year, diversity_val) %>%
      tibble::add_column(diversity_type = "tax_distinct")

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
    data_cell <-
      data_scaled %>%
      dplyr::inner_join(occ_grid_int, keep = FALSE)


    # Retrieve taxonomic data from GBIF
  #  tax_hier <- taxize::classification(unique(data$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

    # Save data
  #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

    tax_hier <- readRDS("taxonomic_hierarchy.RDS")


    # Define function to calculate taxonomic distinctness
    tax_distinct_fn <- function(x, y) {
      temp <- names(y) %in% x$scientificName

      tax_hier_temp <- y[c(temp)]

      print(length(tax_hier_temp))

      n_spec <- length(tax_hier_temp)

      if(length(tax_hier_temp) < 3) {

        tax_distinct <- NA

        return(tax_distinct)

      } else {

        tax_tree <- taxize::class2tree(tax_hier_temp, check=FALSE)

        tax_distance <- tax_tree$distmat

        tax_distinct <- sum(tax_distance) / ((n_spec * (n_spec - 1)) / 2)

        return(tax_distinct)

      }

    }


    # Calculate taxonomic distinctness
    tax_distinct_cell <-
      data_cell %>%
      tibble::add_column(diversity_val = NA) %>%
      dplyr::group_split(cellid) %>%
      purrr::map(. %>%
                 dplyr::mutate(diversity_val = tax_distinct_fn(.,
                                                               tax_hier))) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
      dplyr::select(cellid, diversity_val) %>%
      tibble::add_column(diversity_type = "tax_distinct")

    # Add taxonomic distinctness to grid
    tax_distinct_grid <-
      grid %>%
      dplyr::left_join(tax_distinct_cell, by = "cellid")


  }

}
