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
get_NE_data <- function(level, region, ne_type, ne_scale, output_crs) {



  if (is.null(ne_scale)) {
    ne_scale <- "medium"
  }

  if (ne_scale == "large" & ne_type == "tiny_countries") {
    stop("tiny_countries are only available for medium (50 km) or small (110 km)
          scale maps")
  }

  # Download and prepare Natural Earth map data
  if (level == "country") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            country = region,
                                            type = ne_type,
                                            returnclass = "sf")

  } else if (level == "continent") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            continent = region,
                                            returnclass = "sf")

  } else if (level == "world") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            returnclass = "sf")

  } else if (level == "sovereignty") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            type = ne_type,
                                            sovereignty = region,
                                            returnclass = "sf")

  } else if (level == "geounit") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            type = ne_type,
                                            geounit = region,
                                            returnclass = "sf")

  } else {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            returnclass = "sf")

  }

  map_data <- map_data %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = output_crs)

  return(map_data)

}


#' Create a Spatial Grid for Mapping
#'
#' Generates a grid of polygons covering a specified geographic area,
#' suitable for mapping data retrieved with the rnaturalearth package.
#'
#' @param data An object provided by compute_indicate_workflow containing
#'  occurrence data from a processed data cube.
#' @param cell_size Cell length in kilometers.
#' @param cell_size_units Cell size unit type: "km" or "degrees".
#' @param make_valid Run sf::st_make_valid() on grid. TRUE or FALSE.
#' @param cube_crs Coordinate reference system of the input data cube.
#' @param output_crs Coordinate reference system to use for the output grid.
#' @return An sf object containing the grid polygons, with attributes:
#'   * `cellid`: A unique ID for each grid cell.
#'   * `area`: Area of each grid cell.
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
                       # map_data,
                       # level,
                        cell_size,
                        cell_size_units,
                        make_valid,
                        cube_crs,
                        output_crs) {

  # example_cube_1 <- NULL; rm(example_cube_1)

  occ_sf <- sf::st_as_sf(data,
                         coords = c("xcoord", "ycoord"),
                         crs = cube_crs) %>%
    sf::st_transform(crs = output_crs)

  res <- as.numeric(
    stringr::str_extract(
      data$resolution[1],
      "^[0-9,.]{1,6}(?=[a-z])"
      )
    )

  offset_x <- sf::st_bbox(occ_sf)$xmin - (0.5 * res)
  offset_y <- sf::st_bbox(occ_sf)$ymin - (0.5 * res)

  # Make a grid across the map area
  grid <- occ_sf %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(offset_x, offset_y)) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  if (make_valid==TRUE) {

    grid <- sf::st_make_valid(grid)

  }

  if (cell_size_units == "km") {

    # Add area column to grid
    grid$area <-
      grid %>%
      sf::st_area() %>%
      units::set_units("km^2")

  }

  return(grid)

}


#' @noRd
prepare_spatial_data <- function(data,
                                 grid,
                                # map_data,
                                 cube_crs,
                                 output_crs) {

  cellid <- NULL

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
  #   filter(area > 0.2 * max(area))
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
#'   * 'occ_density': Density of occurrences.
#'   * 'williams_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity scores.
#'   * 'cum_richness': Cumulative species richness.
#'   * 'occ_turnover': Occupancy turnover.
#'   * 'spec_range': Species range size.
#'   * 'spec_occ': Species occurrences.
#'   * 'tax_distinct': Taxonomic distinctness.
#'   * 'hill0': Species richness (estimated by coverage-based rarefaction).
#'   * 'hill1': Hill-Shannon diversity (estimated by coverage-based rarefaction).
#'   * 'hill2': Hill-Simpson diversity (estimated by coverage-based rarefaction).
#' @param dim_type Dimension to calculate indicator over (time: 'ts', or space: 'map')
#' @param ci_type Type of bootstrap confidence intervals to calculate. (Default: "norm".
#'   Select "none" to avoid calculating bootstrap CIs.)
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'cube', 'continent', 'country', 'world', 'sovereignty',
#'  or 'geounit'. (Default: 'cube')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param ne_type The type of Natural Earth data to download: 'countries', 'map_units',
#'  'sovereignty', or 'tiny_countries'. (Default: "countries")
#' @param ne_scale The scale of Natural Earth data to download: 'small' - 110m,
#'  'medium' - 50m, or 'large' - 10m. (Default: "medium")
#' @param output_crs The CRS you want for your calculated indicator. (Leave blank
#'  to let the function choose a default based on grid reference system)
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#' @param spherical_geometry If set to FALSE, will temporarily disable spherical geometry
#' while the function runs. Should only be used to solve specific issues. (Default is TRUE)
#' @param make_valid Calls st_make_valid() from the sf package. Increases processing
#' time but may help if you are getting polygon errors. (Default is FALSE).
#' @param num_bootstrap Set the number of bootstraps to calculate for generating
#' confidence intervals. (Default: 1000)
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
                                       dim_type = c("map",
                                                    "ts"),
                                       ci_type = c("norm",
                                                   "basic",
                                                   "perc",
                                                   "bca",
                                                   "none"),
                                       cell_size = NULL,
                                       level = c("cube",
                                                 "continent",
                                                 "country",
                                                 "world",
                                                 "sovereignty",
                                                 "geounit"),
                                       region = "Europe",
                                       ne_type = c("countries",
                                                   "map_units",
                                                   "sovereignty",
                                                   "tiny_countries"),
                                       ne_scale = c("medium",
                                                    "small",
                                                    "large"),
                                       output_crs = NULL,
                                       first_year = NULL,
                                       last_year = NULL,
                                       spherical_geometry = TRUE,
                                       make_valid = FALSE,
                                       num_bootstrap = 1000,
                                       ...) {

  stopifnot_error("Object class not recognized.",
                  inherits(data, "processed_cube") |
                    inherits(data, "processed_cube_dsinfo") |
                    inherits(data, "sim_cube"))

  available_indicators <- NULL; rm(available_indicators)

  geometry <- area <- cellid <- NULL

  # List of indicators for which bootstrapped confidence intervals should not be calculated
  noci_list <- c("obs_richness",
                 "cum_richness",
                 "occ_turnover")

  type <- match.arg(type,
                    names(available_indicators))

  ci_type <- match.arg(ci_type)

  ne_type <- match.arg(ne_type)

  ne_scale <- match.arg(ne_scale)

  level <- match.arg(level)

  if (!is.null(first_year)) {
    first_year <- ifelse(first_year > data$first_year,
                         first_year,
                         data$first_year)
  } else {
    first_year <- data$first_year
  }

  if (!is.null(last_year)) {
    last_year <- ifelse(last_year < data$last_year, last_year, data$last_year)
  } else {
    last_year <- data$last_year
  }

  df <- data$data[(data$data$year >= first_year) &
                    (data$data$year <= last_year),]

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

    coord_range <- data$coord_range

    if (spherical_geometry==FALSE){

      # Store the current spherical geometry setting
      original_s2_setting <- sf::sf_use_s2()

      # if spherical geometry is on, turn it off
      if (original_s2_setting == TRUE) {
        sf::sf_use_s2(FALSE)
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
      map_data <- get_NE_data(level,
                              region,
                              ne_type,
                              ne_scale,
                              output_crs)

      # Create grid from Natural Earth data
      grid <- create_grid(df,
                         # map_data,
                         # level,
                          cell_size,
                          cell_size_units,
                          make_valid,
                          cube_crs,
                          output_crs)

      # Format spatial data and merge with grid
      df <- prepare_spatial_data(df,
                                 grid,
                                # map_data,
                                 cube_crs,
                                 output_crs)

    } else {

      level <- "unknown"
      region <- "unknown"

    }

    # Assign classes to send data to correct calculator function
    subtype <- paste0(type, "_", dim_type)
    class(df) <- append(type, class(df))
    class(df) <- append(subtype, class(df))

    if (dim_type == "map") {

      # Calculate indicator
      indicator <- calc_map(df, ...)

      # Set attributes as spatially constant to avoid warnings when clipping
      sf::st_agr(grid) <- "constant"
      sf::st_agr(map_data) <- "constant"

      # The following intersection operation requires special error handling
      # because it fails when the grid contains invalid geometries.
      # Therefore when invalid geometries are encountered, it will retry the
      # operation with spherical geometry turned off. This often succeeds.

      # Store the current spherical geometry setting
      original_s2_setting <- sf::sf_use_s2()

      result <- NULL  # Initialize to capture result of intersection

      tryCatch({
        # Attempt without altering the spherical geometry setting
        result <- grid %>%
          sf::st_intersection(map_data) %>%
          dplyr::select(cellid, area, geometry)
      }, error = function(e) {
        if (grepl("Error in wk_handle.wk_wkb", e)) {
          message(paste("Encountered a geometry error during intersection. This may be due",
                        "to invalid polygons in the grid."))
        } else {
          stop(e)
        }
      })

      if (is.null(result)) {
        # If intersection failed, turn off spherical geometry
        message("Retrying the intersection with spherical geometry turned off.")
        sf::sf_use_s2(FALSE)

        # Retry the intersection operation
        result <- grid %>%
          sf::st_intersection(map_data) %>%
          dplyr::select(cellid, area, geometry)

        # Notify success after retry
        message("Intersection succeeded with spherical geometry turned off.")

        # Restore original spherical setting
        sf::sf_use_s2(original_s2_setting)
      }

      # Set grid to result
      grid <- result

      # Add indicator values to grid
      diversity_grid <-
        grid %>%
        dplyr::left_join(indicator, by = "cellid")

    } else {

      # Calculate indicator
      indicator <- calc_ts(df, ...)

      if (ci_type!="none") {

        if (!type %in% noci_list) {

          indicator <- calc_ci(df,
                               indicator = indicator,
                               num_bootstrap=num_bootstrap,
                               ci_type = ci_type,
                               ...)

        } else {

          warning("Bootstrapped confidence intervals cannot be calculated for the chosen indicator.")

        }

      }

    }

    if (spherical_geometry==FALSE) {
      # restore the original spherical geometry setting
      sf::sf_use_s2(original_s2_setting)
    }

    # if the object is of the class sim_cube it contains no grid information, so
    # bypass the usual workflow and deal with it here
  } else {

    if (dim_type=="map") {

      stop(paste("You have provided an object of class 'sim_cube' as input.",
                 "As these objects do not contain grid information they can only",
                 "be used to calculate indicators of dim_type 'ts'."))

    } else {

      year_names <- unique(df$year)
      level <- "unknown"
      region <- "unknown"
      if (is.numeric(data$coord_range)) {
        map_lims <- data$coord_range
      } else {
          map_lims <- "Coordinates not provided"
      }

      kingdoms <- data$kingdoms
      num_families <- data$num_families

      # Assign classes to send data to correct calculator function
      subtype <- paste0(type, "_", dim_type)
      class(df) <- append(type, class(df))
      class(df) <- append(subtype, class(df))

      # Calculate indicator
      indicator <- calc_ts(df, ...)

      if (ci_type!="none") {

        if (!type %in% noci_list) {

          indicator <- calc_ci(df,
                               indicator = indicator,
                               num_bootstrap = 1000,
                               ci_type = ci_type,
                               ...)

        } else {

          warning("Bootstrapped confidence intervals cannot be alculated for the chosen indicator.")
        }


      }

    }

  }

  # Create indicator object

  if (dim_type == "map") {

    diversity_obj <- new_indicator_map(diversity_grid,
                                       div_type = type,
                                       cell_size = cell_size,
                                       cell_size_units = cell_size_units,
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


