#' Retrieve Map Data from rnaturalearth
#'
#' Downloads and prepares map data from the rnaturalearth package at
#' different geographic scales (country, continent, or world).
#'
#' @param level The desired geographic scale: 'country', 'continent', 'geounit',
#'   'sovereignty', 'world', or 'cube'. Cube refers to the geographic extent of
#'   the data cube.
#' @param region The specific region to retrieve data for (required when
#'   level = 'country' or level = 'continent').
#' @param ne_type The type of Natural Earth data to download: 'countries',
#'   'map_units', 'sovereignty', or 'tiny_countries'. (Default: 'countries')
#' @param ne_scale The scale of Natural Earth data to download: 'small' - 110m,
#'   'medium' - 50m, or 'large' - 10m. (Default: 'medium')
#' @param output_crs The CRS you want for your map data.
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
get_NE_data <- function(region,
                        output_crs,
                        level = "cube",
                        ne_type = "countries",
                        ne_scale = "medium",
                        cube_cell_codes = NULL,
                        include_water = FALSE,
                        buffer_dist_km = NULL,
                        data = NULL) {

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

  } else if (level == "world" || level == "cube") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            returnclass = "sf")

  } else {
    stop("Level not recognized. Must be country, continent, geounit,
         sovereignty, world, or cube.")
  }

  if (level == "cube" && !is.null(cube_cell_codes)) {
    # Convert MGRS to Lat/Long
    # latlong_coords <- mgrs::mgrs_to_latlng(cube_cell_codes)
    latlong_coords <- data.frame(lng = data$xcoord, lat = data$ycoord)

    expand_percent <- 0.1 # 10% buffer
    lng_range <- (max(latlong_coords$lng) - min(latlong_coords$lng))
    lat_range <- (max(latlong_coords$lat) - min(latlong_coords$lat))

    # Find bounding box
    min_lon <- min(latlong_coords$lng) - (expand_percent * lng_range)
    max_lon <- max(latlong_coords$lng) + (expand_percent * lng_range)
    min_lat <- min(latlong_coords$lat) - (expand_percent * lat_range)
    max_lat <- max(latlong_coords$lat) + (expand_percent * lat_range)

    map_data <- sf::st_make_valid(map_data)

    # Crop the world map
    map_data_cropped <- sf::st_crop(map_data,
                                    xmin = min_lon,
                                    xmax = max_lon,
                                    ymin = min_lat,
                                    ymax = max_lat)

    map_data <- map_data_cropped

  }

  if (include_water == TRUE) {
    map_data_water <- rnaturalearth::ne_download(scale = ne_scale,
                                                 type = "ocean",
                                                 category = "physical",
                                                 returnclass = "sf")

    get_s2_status <- sf_use_s2()
    sf_use_s2(FALSE)

    # Validate oceans
    map_data_water <- sf::st_make_valid(map_data_water)

    # Crop the oceans
    map_data_water_cropped <- sf::st_crop(map_data_water,
                                          xmin = min_lon,
                                          xmax = max_lon,
                                          ymin = min_lat,
                                          ymax = max_lat)

    # Merge the land with the oceans
    map_data_merged <- sf::st_union(map_data,
                                    map_data_water_cropped)

    # Project the merged and cropped map
    map_data <- sf::st_transform(map_data_merged,
                                 crs = output_crs)

    if (get_s2_status == TRUE){
      sf_use_s2(TRUE)
    }

  } else if (is.character(include_water) &&
             include_water == "buffered_coast") {

    if (level %in% c("country", "continent", "sovereignty", "geounit")) {

      message(
        paste0(
          "Buffering land by ",
          buffer_dist_km,
          " km to include coastal water areas."
        )
      )

      map_data_water <- sf::st_buffer(map_data, dist = buffer_dist_km * 1000)

      # Validate oceans
      map_data_water <- sf::st_make_valid(map_data_water)

      # Merge the land with the oceans
      map_data_merged <- sf::st_union(map_data,
                                      map_data_water)

      # Project the merged and cropped map
      map_data <- sf::st_transform(map_data_merged,
                                   crs = output_crs)

    } else {

      warning(
        paste0(
          "Buffered coast option is best used with 'country', 'continent',",
          "'sovereignty', or 'geounit' levels. Ignoring for 'world' or ",
          "'cube' and defaulting to land only if include_water is TRUE, or ",
          "skipping if FALSE."
        )
      )

    }

  } else {

    # Project the cropped map
    map_data <- sf::st_transform(map_data,
                                 crs = output_crs)

  }

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
#' @param grid_units Cell size unit type: "km" or "degrees".
#' @param make_valid Run sf::st_make_valid() on grid. TRUE or FALSE.
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
                        cell_size,
                        grid_units,
                        utm_crs = NULL,
                        make_valid = FALSE) {

  # Check that cell_size is appropriate
  if (grid_units == "km") {
    if (cell_size < 1000) {
      message(
        paste(
          "Warning: High resolution grids can take a while.",
          "Consider increasing cell size using the 'cell_size' parameter."
        )
      )
    }
    if (cell_size > 100000) {
      stop("Cell size must not be more than 100 km.")
    }
  } else if (grid_units == "degrees") {
     if (cell_size < 0.125) {
       message(
         paste(
           "Warning: High resolution grids can take a while.",
           "Consider increasing cell size using the 'cell_size' parameter."
         )
       )
     }
    if (cell_size > 10) {
      stop("Cell size must not be more than 10 degrees.")
    }
  } else {
    stop("Grid units not recognized. Must be km or degrees.")
  }

  # Handle UTM zones (if present)
  if ("utmzone" %in% names(data)) {
    # Get unique UTM zones
    unique_utm_zones <- unique(data$utmzone)
    grid_list <- list() # Initialize an empty list to store the grids

    # Iterate over each UTM zone
    # for (i in seq_along(unique_utm_zones)) {
    #   zone <- unique_utm_zones[i]
    #   zone_data <- data %>%
    #     dplyr::filter(utmzone == zone)  # Filter data for the current zone

     # zone_data <- sf::st_transform(zone_data, crs = utm_crs)
      zone_data <- sf::st_transform(data, crs = utm_crs)
      offset_x <- sf::st_bbox(zone_data)$xmin #- (0.5 * cell_size)
      offset_y <- sf::st_bbox(zone_data)$ymin #- (0.5 * cell_size)

        zone_grid <- sf::st_make_grid(
          zone_data,
          cellsize = c(cell_size, cell_size),
          offset = c(offset_x, offset_y),
          crs = sf::st_crs(zone_data)
        )

      zone_grid_sf <- sf::st_sf(geometry = zone_grid) %>%
        dplyr::mutate(cellid = dplyr::row_number())

        zone_grid_sf <- sf::st_make_valid(zone_grid_sf)

        zone_grid_sf$area <- sf::st_area(zone_grid_sf)
        if (grid_units == "km") {
          zone_grid_sf$area <- units::set_units(zone_grid_sf$area, "km^2")
        }
        else{
          zone_grid_sf$area <- units::set_units(zone_grid_sf$area, "m^2")
        }
   #   grid_list[[i]] <- zone_grid_sf # Store the grid in the list
    # }
    # Combine grids, transforming to a common CRS (e.g., first UTM zone)
    # if (length(grid_list) > 0) {
    #   combined_grid <- grid_list[[1]]  # Start with the first grid
    #   if (length(grid_list) > 1) {
    #     for (i in 2:length(grid_list)) {
    #       # Transform each subsequent grid to the CRS of the first grid
    #      # grid_list[[i]] <- sf::st_transform(grid_list[[i]], crs = sf::st_crs(grid_list[[1]]))
    #       combined_grid <- rbind(combined_grid, grid_list[[i]])
    #     }
    #   }
    #   grid <- combined_grid
    # }
    # else{
    #   grid <- st_sf(geometry = st_sfc()) # Return empty if no grids.
    # }
        grid <- zone_grid_sf

  } else {

  # Calculate the offset for the grid
  offset_x <- sf::st_bbox(data)$xmin - (0.5 * cell_size)
  offset_y <- sf::st_bbox(data)$ymin - (0.5 * cell_size)

  # Make a grid across the cube
  grid <- data %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(offset_x, offset_y)) %>%
   # sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  if (make_valid==TRUE) {

    grid <- sf::st_make_valid(grid)

  }

  if (grid_units == "km") {

    # Add area column to grid
    grid$area <-
      grid %>%
      sf::st_area() %>%
      units::set_units("km^2")

  }

  }

  return(grid)

}

#' Intersect Occurrences with Grid Cells
#'
#' This function takes occurrence data, a grid, and CRS information, and it
#' returns the occurrence data with cell IDs added based on spatial
#' intersection.
#'
#' @param data An object provided by compute_indicate_workflow containing
#' occurrence data from a processed data cube.
#' @param grid An sf object containing the grid polygons.
#' @param cube_crs The CRS of the data cube.
#' @param output_crs The CRS you want for your calculated indicator.
#'
#' @noRd
prepare_spatial_data <- function(data,
                                 df,
                                 grid,
                                 cube_crs,
                                 output_crs) {

  cellid <- NULL

  # # Convert the x and y columns to the correct format for plotting with sf
  # occ_sf <- sf::st_as_sf(data,
  #                        coords = c("xcoord", "ycoord"),
  #                        crs = cube_crs) %>%
  #   sf::st_transform(crs = output_crs)

  # Set attributes as spatially constant to avoid warnings
  sf::st_agr(grid) <- "constant"
  sf::st_agr(df) <- "constant"

  # Calculate intersection between occurrences and grid cells
   occ_grid_int <- df[sf::st_intersects(df, grid) %>%
                            lengths > 0,] %>%
     sf::st_join(grid)

   # Check if there is any spatial intersection
   if (nrow(occ_grid_int) == 0) {
     stop("No spatial intersection between occurrence data and grid.")
   }

   if ("geometry" %in% names(data)) {
     data <- select(data, -geometry)
   }

  # Add cell numbers to occurrence data
  data <-
    data %>%
    dplyr::inner_join(occ_grid_int) %>%
    suppressMessages() %>%
    dplyr::arrange(cellid)

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
#'  while the function runs. Should only be used to solve specific issues. (Default is TRUE)
#' @param make_valid Calls st_make_valid() from the sf package. Increases processing
#'  time but may help if you are getting polygon errors. (Default is FALSE).
#' @param num_bootstrap Set the number of bootstraps to calculate for generating
#'  confidence intervals. (Default: 1000)
#' @param crs_unit_convert Force a particular output CRS even when it has
#'  different units than the input CRS. (Default: FALSE)
#' @param shapefile_path Path of an external shapefile to merge into the workflow. For example,
#'  if you want to calculate your indicator particular features such as protected areas or wetlands.
#' @param invert Calculate an indicator over the inverse of the shapefile (e.g.
#'  if you have a protected areas shapefile this would calculate an indicator over
#'  all non protected areas)
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
                                       crs_unit_convert = FALSE,
                                       shapefile_path = NULL,
                                       invert = FALSE,
                                       include_water = FALSE,
                                       buffer_dist_km = 50,
                                       ...) {

  stopifnot_error("Object class not recognized.",
                  inherits(data, "processed_cube") |
                    inherits(data, "processed_cube_dsinfo") |
                    inherits(data, "sim_cube"))

  # Early shapefile path check
  if (!is.null(shapefile_path) && !file.exists(shapefile_path)) {
    stop("Shapefile not found at the specified path.")
  }

  # Check for empty cube
  if (nrow(data$data) == 0) {
    stop("No data found in the cube.")
  }

  # Check that obs column exists
  if (!"obs" %in% colnames(data$data)) {
    stop("No occurrences found in the data.")
  }

  available_indicators <- NULL; rm(available_indicators)

  geometry <- area <- cellid <- NULL

  # List of indicators for which bootstrapped confidence intervals should not
  # be calculated
  noci_list <- c("obs_richness",
                 "cum_richness",
                 "occ_turnover")

  type <- match.arg(type,
                    names(available_indicators))

  ci_type <- match.arg(ci_type)

  ne_type <- match.arg(ne_type)

  ne_scale <- match.arg(ne_scale)

  level <- match.arg(level)

  # Store the current spherical geometry setting
  original_s2_setting <- sf::sf_use_s2()

  if (!is.null(first_year) && !is.null(last_year) && first_year > last_year) {
    stop("First year must be less than or equal to last year.")
  }

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

    # input_units <- stringr::str_extract(data$resolutions,
    #                                     "(?<=[0-9,.]{1,6})[a-z]*$")

    num_families <- data$num_families

    coord_range <- data$coord_range

    if (spherical_geometry==FALSE){
      # Temporarily disable spherical geometry
      sf::sf_use_s2(FALSE)
    }

    if (dim_type == "ts") {

      year_names <- unique(df$year)
      map_lims <- unlist(list("xmin" = min(df$xcoord),
                              "ymin" = min(df$ycoord),
                              "xmax" = max(df$xcoord),
                              "ymax" = max(df$ycoord)))

    }

    kingdoms <- data$kingdoms

    if (data$grid_type == "eea") {

      cube_crs <- "EPSG:3035"

    } else if (data$grid_type == "eqdgc") {

      cube_crs <- "EPSG:4326"

    } else if (data$grid_type == "mgrs") {

      #cube_crs <- "EPSG:4326"
      cube_crs <- guess_mgrs_epsg(data, df, data$coord_range)

    } else {

      stop("Grid reference system not found.")

    }

    if (is.null(output_crs)) {

      if (data$grid_type == "eea") {

        output_crs <- "EPSG:3035"

      } else if (data$grid_type == "eqdgc") {

        output_crs <- "EPSG:4326"

      } else if (data$grid_type == "mgrs") {

        #output_crs <- get_crs_for_mgrs(df$cellCode)
        output_crs <- guess_mgrs_epsg(data, df, data$coord_range)
        #output_crs <- "EPSG:4326"

      } else {

        stop("Grid reference system not found.")

      }

    }

    input_units <- check_crs_units(cube_crs)

    output_units <- check_crs_units(output_crs)

    if (crs_unit_convert == FALSE && input_units != output_units) {

      stop(
        paste0(
          "Cube CRS units are ", input_units, " while output CRS ",
          "units are ", output_units, ".\n The conversion could increase ",
          "processing time and lead to invalid output.\n If you are certain ",
          "you want to proceed you can force it with 'crs_unit_convert = TRUE'."
        )
      )

    } else if (crs_unit_convert == TRUE && input_units != output_units) {
      warning(
        paste0(
          "Cube CRS units are ", input_units, " while output CRS ",
          "units are ", output_units, ".\n The conversion could lead ",
          "to invalid output."
        )
      )
    }

    if (stringr::str_detect(data$resolution, "degrees")) {
      input_cell_size <- as.numeric(stringr::str_extract(data$resolution,
                                                  "[0-9.]+(?=degrees)"))
    } else if (stringr::str_detect(data$resolution, "km")) {
      input_cell_size <- as.numeric(stringr::str_extract(data$resolution,
                                                  "[0-9.]+(?=km)"))
    } else {
      stop("Resolution units not recognized. Must be km or degrees.")
    }

    cell_size <- check_cell_size(cell_size, data$resolution, level)

    if (dim_type == "map" | (!is.null(level) & !is.null(region))) {

      if (data$grid_type == "mgrs") {

        df_sf_output <- create_sf_from_utm(df, output_crs)

      } else {

      df_sf_input <- sf::st_as_sf(df,
                             coords = c("xcoord", "ycoord"),
                             crs = cube_crs)

      df_sf_output <- sf::st_transform(df_sf_input, crs = output_crs)

      }


      if (crs_unit_convert == TRUE &&
          input_units != output_units &&
          output_units != "degrees") {

        output_units <- "m"
        grid <- reproject_and_create_grid(df_sf_input,
                                          c(input_cell_size, input_cell_size),
                                          output_crs,
                                          c((cell_size * 1000),
                                            (cell_size * 1000)),
                                          input_units = input_units,
                                          target_units = output_units)
        output_units <- "km"

      } else if (crs_unit_convert == TRUE &&
                 input_units != output_units &&
                 output_units == "degrees") {

        grid <- reproject_and_create_grid(df_sf_input,
                                          c(input_cell_size, input_cell_size),
                                          output_crs,
                                          c((cell_size),
                                            (cell_size)),
                                          input_units = input_units,
                                          target_units = output_units)

      } else {

        # Create grid
        grid <- create_grid(df_sf_output,
                            cell_size,
                            output_units,
                            output_crs,
                            make_valid)

      }

      # Format spatial data and merge with grid
      df <- prepare_spatial_data(df,
                                 df_sf_output,
                                 grid,
                                 cube_crs,
                                 output_crs)

      if (data$grid_type == "mgrs") {

        # Download Natural Earth data
        map_data <- get_NE_data(region,
                                output_crs,
                                level,
                                ne_type,
                                ne_scale,
                                cube_cell_codes = df$cellCode,
                                include_water,
                                buffer_dist_km,
                                df)

      } else {

        # Download Natural Earth data
        map_data <- get_NE_data(region,
                                output_crs,
                                level,
                                ne_type,
                                ne_scale,
                                cube_cell_codes = NULL,
                                include_water,
                                buffer_dist_km,
                                df)

      }

      map_data <- sf::st_make_valid(map_data)

      # Set attributes as spatially constant to avoid warnings when clipping
      sf::st_agr(grid) <- "constant"
      sf::st_agr(map_data) <- "constant"

      # The following intersection operation requires special error handling
      # because it fails when the grid contains invalid geometries.
      # Therefore when invalid geometries are encountered, it will retry the
      # operation with spherical geometry turned off. This often succeeds.

      result <- NULL  # Initialize to capture result of intersection

      tryCatch({
        # Attempt without altering the spherical geometry setting
        result <- grid %>%
          sf::st_intersection(map_data) %>%
          dplyr::select(cellid, area, geometry)
      }, error = function(e) {
        if (grepl("Error in wk_handle.wk_wkb", e)) {
          message(
            paste0(
              "Encountered a geometry error during intersection. This may be ",
              "due to invalid polygons in the grid."
            )
          )
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

      }

      if (spherical_geometry == TRUE) {
        # Restore original spherical setting
        sf::sf_use_s2(original_s2_setting)
      }

      # Check if there is any spatial intersection
      if (nrow(result) == 0) {
        stop("No spatial intersection between map data and grid.")
      }

      # Set grid to result
      grid <- result

      # Set attributes as spatially constant to avoid warnings when clipping
      sf::st_agr(grid) <- "constant"

      # Shapefile Filtering
      if (!is.null(shapefile_path)) {

        is_wkt_file <- grepl("\\.wkt$", tolower(shapefile_path))

        if (is_wkt_file) {

          shapefile <- sf::st_as_sfc(readLines(shapefile_path), crs = 4326)

        } else {

          shapefile <- sf::read_sf(shapefile_path)

        }

        if (sf::st_crs(grid) != sf::st_crs(shapefile)) {
          shapefile <- sf::st_transform(shapefile, crs = sf::st_crs(grid))
        }

        if (invert) {
          grid <- sf::st_difference(grid, sf::st_union(shapefile))
        } else {
          grid <- sf::st_filter(grid, shapefile)
        }

        # Handle empty geometries after spatial operations
        grid <- grid[!sf::st_is_empty(grid), ]

        if (nrow(grid) == 0) {
          stop("No grid cells remain after shapefile filtering.")
        }
      }

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

      # Add indicator values to grid
      diversity_grid <-
        grid %>%
        dplyr::left_join(indicator, by = "cellid")

    } else {

      # Spatial Filtering for Time Series using rnaturalearth
      if (!is.null(level) && !is.null(region)) {

        map_data <- get_NE_data(region,
                                output_crs,
                                level,
                                ne_type,
                                ne_scale,
                                cube_cell_codes = NULL,
                                include_water,
                                buffer_dist_km,
                                df)

        if (data$grid_type == "mgrs") {

          df_sf <- create_sf_from_utm(df, output_crs)

        } else {

        df_sf <- sf::st_as_sf(df,
                              coords = c("xcoord", "ycoord"),
                              crs = cube_crs)
        df_sf <- sf::st_transform(df_sf, crs = output_crs)

        }

        if (sf::st_crs(df_sf) != sf::st_crs(map_data)) {
          map_data <- sf::st_transform(map_data,
                                       crs = sf::st_crs(df_sf))
        }

        map_data <- sf::st_make_valid(map_data)

        # Set attributes as spatially constant to avoid warnings when clipping
        sf::st_agr(map_data) <- "constant"
        sf::st_agr(df_sf) <- "constant"

        # Initialize filtered_sf as NULL to capture the result of the
        # intersection
        filtered_sf <- NULL

        tryCatch({

          # Attempt without altering the spherical geometry setting
          filtered_sf <- sf::st_filter(df_sf, sf::st_union(map_data))
        }, error = function(e) {
          if (grepl("Error in wk_handle.wk_wkb", e)) {
            message(paste("Encountered a geometry error during intersection. ",
                          "This may be due to invalid polygons in the grid."))
          } else {
            stop(e)
          }
        })

        if (is.null(filtered_sf)) {
          # If intersection failed, turn off spherical geometry
          message(
            "Retrying the intersection with spherical geometry turned off."
          )
          sf::sf_use_s2(FALSE)

          # Retry the intersection operation
          filtered_sf <- sf::st_intersection(df_sf, sf::st_union(map_data))

          # Notify success after retry
          message("Intersection succeeded with spherical geometry turned off.")

        }

        if (spherical_geometry == TRUE) {
          # Restore original spherical setting
          sf::sf_use_s2(original_s2_setting)
        }

       # filtered_sf <- sf::st_intersection(df_sf, sf::st_union(map_data))

        # Filter the original data frame
        df <- df[df$cellid %in% filtered_sf$cellid, ]

        if (nrow(df) == 0) {
          stop("No data points remain after spatial filtering.")
        }
      }

      # Shapefile Filtering
      if (!is.null(shapefile_path)) {
        shapefile <- sf::read_sf(shapefile_path)

        # df_sf <- sf::st_as_sf(df,
        #                       coords = c("xcoord", "ycoord"),
        #                       crs = cube_crs)

        # Set attributes as spatially constant to avoid warnings when clipping
        sf::st_agr(df_sf) <- "constant"
        sf::st_agr(shapefile) <- "constant"

        if (sf::st_crs(df_sf) != sf::st_crs(shapefile)) {
          shapefile <- sf::st_transform(shapefile,
                                        crs = sf::st_crs(df_sf))
        }

        # Union the shapefile, handle any errors
        shapefile_union <- tryCatch({
          sf::st_union(shapefile)
        }, error = function(e) {
          stop(paste("Error unioning shapefile:", e$message))
        })

        # Check for validity of the unioned shapefile
        if (!sf::st_is_valid(shapefile_union)) {
          message("Unioned shapefile is invalid. Attempting to make it valid.")
          shapefile_union <- sf::st_make_valid(shapefile_union)
          if (!sf::st_is_valid(shapefile_union)) {
            stop("Could not make unioned shapefile valid.")
          }
        }

        tryCatch({
          if (invert) {
            filtered_df <- sf::st_difference(df_sf, shapefile_union)
          } else {
            filtered_df <- sf::st_filter(df_sf, shapefile)
          }
        }, error = function(e) {
          if (grepl("Error in wk_handle.wk_wkb", e)) {
            message(
              paste0(
                "Geometry error during st_difference/st_filter. Retrying ",
                "with spherical geometry off."
              )
            )
            sf::sf_use_s2(FALSE)
            if (invert) {
              filtered_df <- sf::st_difference(df_sf, shapefile_union)
            } else {
              filtered_df <- sf::st_filter(df_sf, shapefile)
            }
          } else {
            stop(e)
          }
        }, finally = {
          if (spherical_geometry == TRUE) {
            # Restore original spherical setting
            sf::sf_use_s2(original_s2_setting)
          }
        })

        # Filter the original data frame
        df <- df[df$cellid %in% filtered_df$cellid, ]

        if (nrow(df) == 0) {
          stop("No data points remain after shapefile filtering.")
        }
      }

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

          warning(
            paste0(
              "Bootstrapped confidence intervals cannot be calculated for the ",
              "chosen indicator."
            )
          )

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
                 "As these objects do not contain grid information they can ",
                 "only be used to calculate indicators of dim_type 'ts'."))

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

          warning(
            paste0(
              "Bootstrapped confidence intervals cannot be calculated for the ",
              "chosen indicator."
            )
          )
        }


      }

    }

  }

  # Create indicator object

  if (dim_type == "map") {

    diversity_obj <- new_indicator_map(diversity_grid,
                                       div_type = type,
                                       cell_size = cell_size,
                                       cell_size_units = output_units,
                                       map_level = level,
                                       map_region = region,
                                       kingdoms = kingdoms,
                                       num_families = num_families,
                                       num_species = num_species,
                                       first_year = first_year,
                                       last_year = last_year,
                                       num_years = num_years,
                                       species_names = species_names,
                                       years_with_obs = years_with_obs,
                                       map_lims = sf::st_bbox(map_data))

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


