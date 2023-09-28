# This function calculates and maps species richness across grid cells
# The user can choose the cell sizes and the region to map
# Input is ?, level should be "world", "continent", "country", or ?
# region is the specific continent, country, etc., or "world" for global.
# level and region therefore must match.

calc_srm <- function(data, ...) {

  # Download and prepare Natural Earth map data for Europe
  map_data <- ne_countries(scale = "medium", ...) %>%
    st_as_sf() %>%
    st_transform(crs = "EPSG:3035")

  # Filter the map data to only include Greece
  greece <- map_data %>%
    select(name = "Greece")

  # Make a grid across Greece
  grid_greece <- greece %>%
    st_make_grid(cellsize = c(100000, 100000)) %>%
    st_intersection(greece) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())

  # Calculate species richness over the grid
  richness_grid <- grid_greece %>%
    st_join(occ_sf_greece) %>%
    # mutate(overlap = ifelse(obs >= 1, 1, 0)) %>%
    group_by(cellid) %>%
    summarize(num_species = sum(n_distinct(obs)))

  lims_greece <- st_buffer(greece, dist = 1000) %>% st_bbox()

  # Plot richness
  richness_plot <-  ggplot(richness_grid) +
    geom_sf(data = greece, fill = "grey") +
    geom_sf(aes(fill = log(num_species + 1)), color = NA) +
    scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
    coord_sf(
      xlim = c(lims_greece["xmin"], lims_greece["xmax"]),
      ylim = c(lims_greece["ymin"], lims_greece["ymax"])
    ) +
    scale_x_continuous() +
    theme(
      plot.background = element_rect(fill = "#f1f2f3"),
      panel.background = element_rect(fill = "#2F4051"),
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank()
    ) + labs(fill = "richness")
  richness_plot




  # Calculate intersection between occurrences and grid cells
  occ_grid_int_greece <- st_intersection(occ_sf_greece, grid_greece, left = TRUE)

  # Extract coordinates of intersection points and create cell_id column
  occ_grid_coords_greece <- occ_grid_int_greece %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(year = occ_grid_int_greece$year,
           obs = occ_grid_int_greece$obs,
           cell_id = paste0(X, "_", Y))

  # Aggregate by cell ID, year and count unique taxa
  richness_year_greece2 <- occ_grid_coords_greece %>%
    group_by(cell_id, year) %>%
    summarize(taxon_count = n_distinct(obs))

  # Join richness data with grid data
  grid_richness_greece <- grid_greece %>%
    st_join(st_as_sf(occ_grid_coords_greece, coords = c("X", "Y"), crs = "EPSG: 3035")) %>%
    group_by(cell_id)

  # Create a base map of Greece
  base_map <- ggplot() +
    geom_sf(data = greece, fill = "lightgray", color = "gray")   # Plot Greece

  # Create a plot of the grid with richness values
  richness_map <- base_map +
    geom_sf(data = grid_richness_greece, aes(fill = obs)) +  # Plot the grid with richness values
    scale_fill_gradient(low = "white", high = "red") +  # Customize the color scale
    labs(title = "Richness Values in Greece by Year") +
    theme_minimal()

  richness_map






  # Scale the coordinates of occurrences so the number of digits matches
  merged_data_scaled <-
    merged_data %>%
    mutate(xcoord = xcoord * 1000, ycoord = ycoord * 1000)

  # Convert the x and y columns to the correct format for plotting with sf
  occ_sf <- st_as_sf(merged_data_scaled, coords = c("xcoord", "ycoord"), crs = "EPSG:3035")

  # Define EEA coordinate system
  crs_eea <- st_crs("EPSG:3035")

  # Define grid spacing and extent
  grid_spacing <- 1000 # 1 km
  grid_extent <- st_bbox(richness_year) %>%
    st_as_sfc() %>%
    st_set_crs(crs_eea) %>%
    st_buffer(grid_spacing * 2) %>%
    st_bbox()

  # Create grid
  grid <- st_make_grid(st_as_sfc(grid_extent), cellsize = grid_spacing) %>%
    st_sf() %>%
    mutate(cell_id = paste0(X, "_", Y))

  # Transform grid to EEA coordinate system
  grid_eea <- st_transform(grid, crs = crs_eea)

  # Calculate intersection between occurrences and grid cells
  occ_grid_int <- st_intersection(occ_sf, grid, left = TRUE)

  # Extract coordinates of intersection points and create cell_id column
  occ_grid_coords <- occ_grid_int %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(year = occ_grid_int$year,
           obs = occ_grid_int$obs,
           cell_id = paste0(X, "_", Y))

  # # Aggregate by cell ID, year and count unique taxa
  # richness_year <- occ_grid_coords %>%
  #  group_by(cell_id, year) %>%
  #  summarize(taxon_count = n_distinct(obs))
  #
  # # Join richness data with grid data
  # grid_richness <- st_join(richness_year, grid_eea, left = TRUE)

  # Aggregate by cell ID, year and count unique taxa
  richness_year <- occ_sf %>%
    group_by(eea_cell_code, year) %>%
    summarise(geometry = first(geometry), taxon_count = n_distinct(obs))

  saveRDS(richness_year, file="richness_year.RDS")


  year_to_plot <- 2020
  europe <- map_data_tr

  ggplot() +
    geom_sf(data = europe, fill = "white", color = "black") +
    geom_tile(data = subset(richness_year, year == year_to_plot),
              aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = taxon_count),
              alpha = 0.7) +
    scale_fill_gradient(low = "gray90", high = "blue", na.value = "gray90") +
    labs(title = paste("Species Richness Heatmap for Year", year_to_plot)) +
    theme_minimal()



  # Plot map with background layer of Greece, grid cells based on richness values, and occurrence data in 2022
  ggplot() +
    geom_sf(data = greece, fill = "lightgray") +
    geom_sf(data = filter(richness_year, year == 2021), aes(fill = taxon_count)) +
    scale_fill_gradient(low = "white", high = "blue") +
    #geom_sf(data = filter(occ_sf, year == 2021), color = "red", size = 0.5) +
    theme_void() +
    labs(title = "Occurrence data and taxon richness in Greece in 2021")





}
