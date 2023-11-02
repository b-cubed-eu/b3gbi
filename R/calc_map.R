# This function calculates and maps species richness across grid cells
# The user can choose the cell sizes and the region to map
# Input is ?, level should be "world", "continent", or "country"
# region is the specific continent, country, etc., or "world" for global.
# level and region therefore must match.
# Arguments cs1 and cs2 are width and height (in km) of the grid.
# coverage determines the coverage percentage to standardize at (0.9 = 90%)
# cutoff_length determines the smallest number of occurrences per grid cell
# for diversity estimation. Anything smaller becomes NA.
# inext_sampsize determines the maximum sample size to use for diversity estimation
# knots determines the number of different sample sizes to estimate at (between
# 0 and inext_sampsize)
# sub-sampling will be used for larger samples and extrapolation for smaller ones
# type can be "hill" for hill diversity measure, "even" for evenness,
# "obs_rich" for observed species richness, "total_obs" for total occurrences,
# "newness" for newness of data (mean occurrence year), "density" for the
# number of occurrences per square km
# qval determines which type of hill diversity measure to estimate,
# 0 for species richness, 1 for shannon hill diversity, 2 for simpson hill diversity

calc_map <- function(data,
                     cs1 = 100,
                     cs2 = 100,
                     level = "country",
                     region = "Denmark",
                     type = "hill",
                     qval = 0,
                     coverage = 0.9,
                     cutoff_length = 5,
                     inext_sampsize = 100,
                     knots = 10,
                     ...) {

  # Download and prepare Natural Earth map data
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

  # Add area column to grid
  grid$area_km2 <-
    grid %>%
    st_area %>%
    units::set_units("km^2")

  # Set map limits
  map_lims <- st_buffer(grid, dist = 1000) %>%
    st_bbox()

  # Scale coordinates of occurrences so the number of digits matches map
  data_scaled <-
    data %>%
    mutate(xcoord = xcoord * 1000,
           ycoord = ycoord * 1000)

  # Convert the x and y columns to the correct format for plotting with sf
  occ_sf <- st_as_sf(data_scaled,
                     coords = c("xcoord", "ycoord"),
                     crs = "EPSG:3035")

  # Calculate intersection between occurrences and grid cells
  occ_grid_int <- st_intersection(occ_sf, grid, left = TRUE)

  # Add cell numbers to occurrence data
  data_cell <-
    data_scaled %>%
    dplyr::inner_join(occ_grid_int) %>%
    arrange(cellid)

  #
  # # Remove grid cells with areas smaller than 20% of the largest one
  # grid_filtered <-
  #   grid %>%
  #   filter(area_km2 > 0.2 * max(area_km2))
  #
  # # Remove same grid cells from data
  # merged_data_cells_filtered <-
  #   merged_data_cells %>%
  #   filter(cellid %in% grid_filtered$cellid)


  if (type == "hill") {

  # Create list of occurrence matrices by grid cell, with species as rows
  spec_rec_raw_cell <-
    data_cell %>%
    dplyr::group_split(cellid) %>%
    purrr::map(. %>%
                 dplyr::group_by(eea_cell_code,
                                 taxonKey) %>%
                 tidyr::pivot_wider(names_from = taxonKey,
                                    values_from = obs) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-eea_cell_code,
                               -scientificName,
                               -kingdom,
                               -rank,
                               -geometry,
                               -resolution,
                               -xcoord,
                               -ycoord,
                               -year,
                               -area_km2) %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 dplyr::select(-cellid) %>%
                 tibble::rownames_to_column() %>%
                 tidyr::gather(variable,
                               value,
                               -rowname) %>%
                 tidyr::spread(rowname, value) %>%
                 'row.names<-'(., NULL) %>%
                 tibble::column_to_rownames(var = "variable") %>%
                 as.matrix() %>%
                 replace(. > 1, as.integer(1))
    )


  # name list elements
  names(spec_rec_raw_cell) <- unique(data_cell$cellid)

  # remove all cells with too little data to avoid errors from iNEXT
  spec_rec_raw_cell2 <- spec_rec_raw_cell %>%
    keep(., function(x) length(x) > cutoff_length)

  # Calculate diversity estimates
  # coverage_rare_cell <- spec_rec_raw_cell2 %>%
  #   iNEXT(endpoint=inext_sampsize, knots=knots, datatype="incidence_raw", q=qval)

  coverage_rare_cell <- spec_rec_raw_cell2 %>%
    estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

  # Extract estimated relative diversity
  est_diversity_cell <-
    coverage_rare_cell %>%
    #coverage_rare_cell$iNextEst$coverage_based %>%
    #dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
    #              .by = Assemblage) %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
    dplyr::rename(cellid = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q) %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")

  # Add diversity values to grid
  est_diversity_grid <-
    grid %>%
    dplyr::left_join(est_diversity_cell, by = "cellid")

  } else if (type == "obs_rich") {

    # Calculate species richness over the grid
    richness_cell <-
      data_cell %>%
      dplyr::summarize(diversity_val = sum(n_distinct(taxonKey)),
                       .by = "cellid") %>%
      tibble::add_column(diversity_type = c("obs_richness"))

    # Add observed richness to grid
    richness_grid <-
      grid %>%
      dplyr::left_join(richness_cell, by = "cellid")

  } else if (type == "total_obs") {

    # Calculate total number of observations over the grid
    obs_cell <-
      data_cell %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = "cellid") %>%
      tibble::add_column(diversity_type = c("total_obs"))

    # Add total observations to grid
    obs_grid <-
      grid %>%
      dplyr::left_join(obs_cell, by = "cellid")

  } else if (type == "newness") {

    # Calculate mean year of occurrence over the grid
    newness_cell <-
      data_cell %>%
      dplyr::summarize(diversity_val = round(mean(year)),
                       .by = "cellid") %>%
      tibble::add_column(diversity_type = c("newness"))

    # Add newness to grid
    newness_grid <-
      grid %>%
      dplyr::left_join(newness_cell, by = "cellid")

  } else if (type == "density") {

    # Calculate density of occurrences over the grid (per square km)
    density_cell <-
      data_cell %>%
      dplyr::reframe(diversity_val = sum(obs) / area_km2,
                     .by = "cellid") %>%
      dplyr::distinct(cellid, diversity_val) %>%
      tibble::add_column(diversity_type = c("density"))

    # Add occurrence density to grid
    density_grid <- grid %>%
      dplyr::left_join(density_cell, by = "cellid")

    # Change type to avoid errors when plotting
    density_grid$diversity_val <- as.numeric(density_grid$diversity_val)

  } else if (type == "even") {

    # Calculate adjusted evenness for each grid cell
    evenness_cell <-
      data_cell %>%
      dplyr::summarize(num_occ = sum(obs),
                       .by = c(cellid, taxonKey)) %>%
      dplyr::arrange(cellid) %>%
      tidyr::pivot_wider(names_from = cellid,
                         values_from = num_occ) %>%
      replace(is.na(.), 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      purrr::map(~calc_evenness(.)) %>%
      unlist() %>%
      as.data.frame() %>%
      dplyr::rename(diversity_val = ".") %>%
      tibble::rownames_to_column(var = "cellid") %>%
      dplyr::mutate(cellid = as.integer(cellid),
                    .keep = "unused") %>%
      tibble::add_column(diversity_type = c("evenness"))

    # Add evenness values to grid
    evenness_grid <-
      grid %>%
      dplyr::left_join(evenness_cell, by = "cellid")

  } else {

    stop("Invalid type argument.")

  }

}
