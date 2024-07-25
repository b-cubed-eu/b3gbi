#' Create a Spatial Grid for Mapping
#'
#' Generates a grid of polygons covering a specified geographic area,
#' suitable for mapping data retrieved with the rnaturalearth package.
#'
#' @param map_data A spatial object (e.g., an sf object) representing the
#'   geographic area of interest.  Obtained from rnaturalearth.
#' @param cell_size Cell length in kilometers.
#' @return An sf object containing the grid polygons, with attributes:
#'   * `cellid`: A unique ID for each grid cell.
#'   * `area_km2`: Area of each grid cell in square kilometers.
#'
#' @examples
#' # Get some map data
#' germany_map <- rnaturalearth::ne_countries(country = "Germany",
#'                                            scale = "medium",
#'                                            returnclass = "sf")
#' # Change projection to EPSG:3035 (works well with metric grid size)
#' germany_map <- sf::st_transform(germany_map,
#'                                 crs = "EPSG:3035")
#' # Calculate a 100km x 100km grid and plot it
#' germany_grid <- create_grid(germany_map,
#'                             cell_size = 10)
#' plot(germany_grid)
#' @noRd
create_grid <- function(data,
                        map_data,
                        level,
                        cell_size,
                        cell_size_units,
                        make_valid,
                        cube_crs,
                        output_crs) {

  occ_sf <- sf::st_as_sf(data,
                         coords = c("xcoord", "ycoord"),
                         crs = cube_crs) %>%
    sf::st_transform(crs = output_crs)

  # Make a grid across the map area
  grid <- occ_sf %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(sf::st_bbox(occ_sf)$xmin,
                                sf::st_bbox(occ_sf)$ymin)) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  if (make_valid==TRUE) {

    grid <- sf::st_make_valid(grid)

  }

  # Add area column to grid
  grid$area_km2 <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)

}

#' Retrieve Map Data from rnaturalearth
#'
#' Downloads and prepares map data from the rnaturalearth package at
#' different geographic scales (country, continent, or world).
#'
#' @param level  The desired geographic scale: "country", "continent", or "world".
#' @param region  The specific region to retrieve data for (required when
#'  `level = "country"` or `level = "continent"`).
#' @return An sf object containing the map data, transformed to the
#'   appropriate projection.
#'
#' @examples
#' # Download country-level data for France
#' france_map <- get_NE_data(level = "country", region = "France")
#'
#' # Get continent-level data for Africa
#' africa_map <- get_NE_data(level = "continent", region = "Africa")
#'
#' # Retrieve a map of the entire world
#' world_map <- get_NE_data(level = "world")
#' @noRd
get_NE_data <- function(level, region, output_crs) {

  # Download and prepare Natural Earth map data
  if (level == "country") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            country = region,
                                            returnclass = "sf")

  } else if (level == "continent") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            continent = region,
                                            returnclass = "sf")

  } else if (level == "world") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            returnclass = "sf")

  }

    map_data <- map_data %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = output_crs)

  return(map_data)

}


#' @noRd
prepare_spatial_data <- function(data, grid, map_data, cube_crs, output_crs) {

  # Set map limits
  # map_lims <- sf::st_buffer(grid, dist = 1000) %>%
  #   sf::st_bbox()

  # Scale coordinates of occurrences so the number of digits matches map
  # data <-
  #   data %>%
  #   dplyr::mutate(xcoord = xcoord * 1000,
  #                 ycoord = ycoord * 1000)

  # data[, xcoord := xcoord * 1000][, ycoord := ycoord * 1000]

  # Convert the x and y columns to the correct format for plotting with sf
  occ_sf <- sf::st_as_sf(data,
                         coords = c("xcoord", "ycoord"),
                         crs = cube_crs) %>%
    sf::st_transform(crs = output_crs)

  # Set attributes as spatially constant to avoid warnings
  sf::st_agr(grid) <- "constant"
  sf::st_agr(occ_sf) <- "constant"

  # Calculate intersection between occurrences and grid cells
   occ_grid_int <- occ_sf[sf::st_intersects(occ_sf, grid) %>%
                            lengths > 0,] %>%
     sf::st_join(grid)

  # Add cell numbers to occurrence data
  data <-
    data %>%
    dplyr::inner_join(occ_grid_int) %>%
    suppressMessages() %>%
    dplyr::arrange(cellid)

  #
  # # Remove grid cells with areas smaller than 20% of the largest one
  # grid <-
  #   grid %>%
  #   filter(area_km2 > 0.2 * max(area_km2))
  #
  # # Remove same grid cells from data
  # data <-
  #   data %>%
  #   filter(cellid %in% grid$cellid)

  return(data)

}

#' @title Calculate Biodiversity Indicators Over Space or Time
#'
#' @description This function provides a flexible framework for calculating various biodiversity
#' indicators on a spatial grid or as a time series. It prepares the data, creates a grid, calculates indicators,
#' and formats the output into an appropriate S3 object ('indicator_map' or 'indicator_ts').
#'
#' @param data A data cube object (class 'processed_cube').
#' @param type The indicator to calculate. Supported options include:
#'   * 'obs_richness': Observed species richness.
#'   * 'total_occ': Total number of occurrences.
#'   * 'newness': Mean year of occurrence.
#'   * 'density': Density of occurrences.
#'   * 'williams_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity scores.
#' @param dim_type Dimension to calculate indicator over (time: 'ts', or space: 'map')
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param output_crs The CRS you want for your calculated indicator. (Leave blank
#'  to let the function choose a default based on grid reference system)
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#' @param spherical_geometry If set to FALSE, will temporarily disable spherical geometry
#' while the function runs. Should only be used to solve specific issues. (Default is TRUE)
#' @param make_valid Calls st_make_valid() from the sf package. Increases processing
#' time but may help if you are getting polygon errors. (Default is FALSE).
#' @param ... Additional arguments passed to specific indicator calculation functions.
#'
#' @return An S3 object containing the calculated indicator values and metadata.
#'
#' @examples
#' diversity_map <- compute_indicator_workflow(example_cube_1,
#'                                             type = "obs_richness",
#'                                             dim_type = "map",
#'                                             level = "country",
#'                                             region = "Denmark")
#' diversity_map
#'
#' @export
compute_indicator_workflow <- function(data,
                                       type,
                                       dim_type = c("map", "ts"),
                                       cell_size = NULL,
                                       level = c("continent", "country", "world"),
                                       region = "Europe",
                                       #cube_crs = NULL,
                                       output_crs = NULL,
                                       first_year = NULL,
                                       last_year = NULL,
                                       spherical_geometry = TRUE,
                                       make_valid = FALSE,
                                       ...) {

  stopifnot_error("Object class not recognized.",
                  inherits(data, "processed_cube") |
                    inherits(data, "processed_cube_dsinfo") |
                    inherits(data, "sim_cube"))

  type <- match.arg(type,
                    names(available_indicators))

  if (!is.null(first_year)) {
    first_year <- ifelse(first_year > data$first_year, first_year, data$first_year)
  } else {
    first_year <- data$first_year
  }

  if (!is.null(last_year)) {
    last_year <- ifelse(last_year < data$last_year, last_year, data$last_year)
  } else {
    last_year <- data$last_year
  }

  df <- data$data[(data$data$year >= first_year) & (data$data$year <= last_year),]

  # Collect information to add to final object
  num_species <- data$num_species
  num_years <- length(unique(df$year))
  species_names <- unique(df$scientificName)
  years_with_obs <- unique(df$year)

  if (!inherits(data, "sim_cube")) {

    dim_type <- match.arg(dim_type)

    level <- match.arg(level)

    cell_size_units <- stringr::str_extract(data$resolutions, "(?<=[0-9,.]{1,6})[a-z]*$")

    num_families <- data$num_families

    if (spherical_geometry==FALSE){

      # if spherical geometry is on, turn it off
      if (sf::sf_use_s2()) {
        sf::sf_use_s2(FALSE)
        turned_off <- TRUE
      }

    }

    if (dim_type == "ts") {

      year_names <- unique(df$year)
      map_lims <- unlist(list("xmin" = min(df$xcoord),
                              "xmax" = max(df$xcoord),
                              "ymin" = min(df$ycoord),
                              "ymax" = max(df$ycoord)))

    }

    kingdoms <- data$kingdoms

    #  if (is.null(cube_crs)) {

    if (data$grid_type == "eea") {

      cube_crs <- "EPSG:3035"

    } else if (data$grid_type == "eqdgc") {

      cube_crs <- "EPSG:4326"

    } else if (data$grid_type == "mgrs") {

      cube_crs <- "EPSG:4326"

    } else {

      stop("Grid reference system not found.")

    }

    #  }

    if (is.null(output_crs)) {

      if (data$grid_type == "eea") {

        output_crs <- "EPSG:3035"

      } else if (data$grid_type == "eqdgc") {

        output_crs <- "EPSG:4326"

      } else if (data$grid_type == "mgrs") {

        #output_crs <- get_crs_for_mgrs(df$cellCode)
        output_crs <- "EPSG:9822"

      } else {

        stop("Grid reference system not found.")

      }

    }

    # Check that the grid cell size (if provided) is sensible.
    # Determine a default size if nothing is provided.
    cell_size <- check_cell_size(cell_size, cell_size_units, data$resolution, level)

    if (dim_type == "map" | (!is.null(level) & !is.null(region))) {

      # Download Natural Earth data
      map_data <- get_NE_data(level, region, output_crs)

      # Create grid from Natural Earth data
      grid <- create_grid(df, map_data, level, cell_size, cell_size_units, make_valid, cube_crs, output_crs)

      # Format spatial data and merge with grid
      df <- prepare_spatial_data(df, grid, map_data, cube_crs, output_crs)

    } else {

      level <- "unknown"
      region <- "unknown"

    }

    # Assign classes to send data to correct calculator function
    subtype <- paste0(type, "_", dim_type)
    class(df) <- append(type, class(df))
    class(df) <- append(subtype, class(df))

    if (dim_type == "map") {

      # # Deal with uncertainty
      # if (cell_size_units == "km") {
      #
      #   df$uncertainty_cells <- df$minCoordinateUncertaintyInMeters / cell_size
      #
      # } else if (cell_size_units == "degrees") {
      #
      #   df$uncertainty_cells <- meters_to_decdeg(
      #     df,
      #     lat_col = "ycoord",
      #     lon_col = "xcoord",
      #     distance = "minCoordinateUncertaintyInMeters",
      #     na_action = "NA as NA") / cell_size
      #
      # } else {
      #
      #   stop("Resolution not recognized. Must be in km or degrees.")
      #
      # }

      # Calculate indicator
      indicator <- calc_map(df, ...)

      # Set attributes as spatially constant to avoid warnings when clipping
      sf::st_agr(grid) <- "constant"
      sf::st_agr(map_data) <- "constant"

      # Clip grid to map
      grid <- grid %>%
        sf::st_intersection(map_data) %>%
        dplyr::select(cellid,
                      area_km2,
                      geometry)

      # Add indicator values to grid
      diversity_grid <-
        grid %>%
        dplyr::left_join(indicator, by = "cellid")

      # diversity_grid <- dplyr::left_join(diversity_grid, df[,c("cellid", "uncertainty_cells")], by = "cellid", multiple = "first")

      # diversity_grid <- diversity_grid[!is.na(diversity_grid$diversity_val),]

    } else {

      # Calculate indicator
      indicator <- calc_ts(df, ...)

      # indicator <- dplyr::left_join(indicator, df[,c("year", "minTemporalUncertainty")], by = "year", multiple = "first")

      # Calculate confidence intervals
    #  conf_int <- calc_ci(df, ...)

    }

    if (spherical_geometry==FALSE) {

      # if spherical geometry had to be turned off, now turn it back on
      if(turned_off == TRUE) {
        sf::sf_use_s2(TRUE)
      }

    }

    # if the object is of the class sim_cube it contains no grid information, so
    # bypass the usual workflow and deal with it here
  } else {

    if (dim_type=="map") {
      stop("You have provided an object of class 'sim_cube' as input. As these objects
           do not contain grid information they can only be used to calculate
           indicators of dim_type 'ts'.")
    }

    year_names <- unique(df$year)
    level <- "unknown"
    region <- "unknown"
    num_families <- "NA"
    kingdoms <- "NA"
    map_lims <- unlist(list("xmin" = "NA",
                            "xmax" = "NA",
                            "ymin" = "NA",
                            "ymax" = "NA"))

    # Assign classes to send data to correct calculator function
    subtype <- paste0(type, "_", dim_type)
    class(df) <- append(type, class(df))
    class(df) <- append(subtype, class(df))

    # Calculate indicator
    indicator <- calc_ts(df, ...)

  }

  # Create indicator object

  if (dim_type == "map") {

    diversity_obj <- new_indicator_map(diversity_grid,
                                       div_type = type,
                                       cell_size = cell_size,
                                       map_level = level,
                                       map_region = region,
                                       kingdoms = kingdoms,
                                       num_families = num_families,
                                       num_species = num_species,
                                       first_year = first_year,
                                       last_year = last_year,
                                       num_years = num_years,
                                       species_names = species_names,
                                       years_with_obs = years_with_obs)

  } else {

    diversity_obj <- new_indicator_ts(dplyr::as_tibble(indicator),
                                      div_type = type,
                                      map_level = level,
                                      map_region = region,
                                      kingdoms = kingdoms,
                                      num_families = num_families,
                                      num_species = num_species,
                                      num_years = num_years,
                                      species_names = species_names,
                                      coord_range = map_lims)

  }

  return(diversity_obj)

}


