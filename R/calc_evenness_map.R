calc_evenness_map <- function(data,
                     cs1 = 100,
                     cs2 = 100,
                     level = "country",
                     region = "Denmark",
                     coverage = 0.9,
                     cutoff_length = 5,
                     inext_sampsize = 100,
                     qval = 0,
                     knots = 10,
                     ...) {

  # Download and prepare Natural Earth map data for Europe
  if (level == "country") {

    map_data <- ne_countries(scale = "medium", country = region) %>%
      st_as_sf() %>%
      st_transform(crs = "EPSG:3035")

  } else if (level == "continent") {

    map_data <- ne_countries(scale = "medium", continent = region) %>%
      st_as_sf() %>%
      st_transform(crs = "EPSG:3035")

  } else if (level == "world") {

    map_data <- ne_countries(scale = "medium") %>%
      st_as_sf() %>%
      st_transform(crs = "EPSG:3035")

  }

  # Make a grid across the map area
  grid <- map_data %>%
    st_make_grid(cellsize = c(cs1 * 1000, cs2 * 1000)) %>%
    st_intersection(map_data) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())

  # Set map limits
  map_lims <- st_buffer(grid, dist = 1000) %>%
    st_bbox()

  # Scale coordinates of occurrences so the number of digits matches map
  merged_data_scaled <-
    merged_data %>%
    mutate(xcoord = xcoord * 1000,
           ycoord = ycoord * 1000)

  # Convert the x and y columns to the correct format for plotting with sf
  occ_sf <- st_as_sf(merged_data_scaled,
                     coords = c("xcoord", "ycoord"),
                     crs = "EPSG:3035")

  # Calculate intersection between occurrences and grid cells
  occ_grid_int <- st_intersection(occ_sf, grid, left = TRUE)

  # Add cell numbers to occurrence data
  merged_data_cells <-
    merged_data_scaled %>%
    dplyr::inner_join(occ_grid_int) %>%
    dplyr::group_by(cellid)

  # Calculate number of records for each species by grid cell
  spec_rec_cell <-
    merged_data_cells %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::group_by(eea_cell_code,
                                 scientificName) %>%
                 dplyr::summarise(spec_rec = sum(obs), .groups = "drop") %>%
                 tidyr::pivot_wider(names_from = scientificName, values_from = spec_rec) %>%
                 dplyr::select(-eea_cell_code) %>%
                 replace(is.na(.), 0)
    )

  # name list elements
  names(spec_rec_cell) <- unique(occ_grid_int$cellid)

  # Calculate adjusted evenness for each grid cell
  evenness_cell <- spec_rec_cell %>%
    purrr::map(~calc_evenness(.)) %>%
    unlist() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "cellid") %>%
    dplyr::rename(diversity_val = ".") %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused") %>%
    tibble::add_column(diversity_type = c("evenness"))

  # Add evenness values to grid
  evenness_grid <-
    grid %>%
    dplyr::left_join(evenness_cell, by = "cellid")

}
