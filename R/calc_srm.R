# This function calculates and maps species richness across grid cells
# The user can choose the cell sizes and the region to map
# Input is ?, level should be "world", "continent", "country", or ?
# region is the specific continent, country, etc., or "world" for global.
# level and region therefore must match.
# Arguments cs1 and cs2 are width and height (in km) of the grid.

calc_srm <- function(data, cs1, cs2, ...) {

  # Download and prepare Natural Earth map data for Europe
  map_data <- ne_countries(scale = "medium", country = "Germany") %>%
    st_as_sf() %>%
    st_transform(crs = "EPSG:3035")

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

  #  ----------------------------------------

  # Calculate species richness over the grid
  richness_grid <- grid %>%
    st_join(occ_sf) %>%
    # mutate(overlap = ifelse(obs >= 1, 1, 0)) %>%
    group_by(cellid) %>%
    summarize(num_species = sum(n_distinct(obs)))


  #  ----------------------------------------

  # Add cell numbers to occurrence data
  merged_data_cells <-
    merged_data_scaled %>%
    dplyr::inner_join(occ_grid_int) %>%
    dplyr::group_by(cellid)

  # Create list of occurrence matrices by year, with species as rows
  spec_rec_raw_cell <-
    merged_data_cells %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::group_by(eea_cell_code,
                                 scientificName) %>%
                 tidyr::pivot_wider(names_from = scientificName,
                                    values_from = obs) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-eea_cell_code,
                               -taxonKey,
                               -kingdom,
                               -rank,
                               -geometry,
                               -resolution,
                               -xcoord,
                               -ycoord,
                               -year) %>%
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
  names(spec_rec_raw_cell) <- unique(occ_grid_int$cellid)

  # set sample size for inext function call
  inext_sampsize <- 150

  # Calculate diversity estimates
  coverage_rare_cell <- spec_rec_raw_cell %>%
    iNEXT(endpoint=inext_sampsize, knots = 20, nboot = 50, datatype="incidence_raw")

  # Extract estimated relative species richness
  est_richness <-
    coverage_rare_temp$iNextEst$coverage_based %>%
    dplyr::filter(t == inext_sampsize) %>%
    dplyr::select(Assemblage, qD) %>%
    dplyr::rename(year = Assemblage,
                  est_relative_richness = qD)

  # Calculate estimated relative richness as an index
  est_richness <-
    est_richness %>%
    mutate(index = est_relative_richness/lag(est_relative_richness)) %>%
    replace(is.na(.), 1) %>%
    mutate(index = cumprod(index))




  # Calculate rarefaction curves for each year
  future::plan(multisession)
  spec_rare_cell <- spec_rec_raw_cell %>%
    furrr::future_map(vegan::specaccum, method = "rarefaction")

  # Get the sampling effort level at which to interpolate
  sampling_effort <-
    spec_rare %>%
    purrr::map(~max(.$individuals)) %>%
    unlist() %>%
    min()

  # Interpolate richness at different sampling effort levels
  interpolated_richness <-
    spec_rare %>%
    purrr::map(~stats::approx(.$sites, .$richness, xout = sampling_effort)$y)

  # Calculate mean rarefied richness for each year
  mean_rarefied_richness <-
    interpolated_richness %>%
    purrr::map(mean, na.rm=TRUE)

  # Calculate adjusted species richness by year
  rarefied_df <-
    richness_by_year %>%
    tibble::add_column(mean_rarefied_richness = unlist(mean_rarefied_richness),
                       .after = "obs_richness") %>%
    dplyr::mutate(adj_richness = (obs_richness - mean_rarefied_richness),
                  .after = "mean_rarefied_richness")



  # -------------------------------------------

  # Plot richness
  richness_plot <-  ggplot(richness_grid) +
    #geom_sf(data = map_data, fill = "grey") +
    geom_sf(aes(fill = log(num_species + 1)), color = NA) +
    scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
    coord_sf(
      xlim = c(map_lims["xmin"], map_lims["xmax"]),
      ylim = c(map_lims["ymin"], map_lims["ymax"])
    ) +
    scale_x_continuous() +
    theme(
      plot.background = element_rect(fill = "#f1f2f3"),
      panel.background = element_rect(fill = "#2F4051"),
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      legend.text = element_blank()
    ) +
    labs(fill = "log of \nrichness")
  richness_plot



}
