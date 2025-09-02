#' Retrieve Map Data from rnaturalearth
#'
#' Downloads and prepares map data from the rnaturalearth package at
#' different geographic scales (country, continent, or world).
#'
#' @param latlong_bbox A bbox object in latitude and longitude ("EPSG:4326") to
#'   use for cropping the downloaded map data (required if level = 'cube').
#' @param projected_crs The projected CRS to convert the cropped map to.
#' @param region The specific region to retrieve data for (required unless
#'   level = 'cube' or 'world').
#' @param level The desired geographic scale: 'country', 'continent', 'geounit',
#'   'sovereignty', 'world', or 'cube'. Cube refers to the geographic extent of
#'   the data cube.
#' @param ne_type The type of Natural Earth data to download: 'countries',
#'   'map_units', 'sovereignty', or 'tiny_countries'. (Default: 'countries')
#' @param ne_scale The scale of Natural Earth data to download: 'small' - 110m,
#'   'medium' - 50m, or 'large' - 10m. (Default: 'medium')
#' @param include_ocean Include oceans if TRUE. Set to "buffered_coast" to
#'   instead create a buffer around the land area. If set to FALSE occurrences
#'   that fall outside of land boundaries will not be included in the analysis.
#' @param buffer_dist_km Distance to buffer around the land if you choose
#'   "buffered_coast" for the include_ocean parameter.
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
get_NE_data <- function(latlong_bbox = NULL,
                        projected_crs,
                        region = NULL,
                        level = "cube",
                        ne_type = "countries",
                        ne_scale = "medium",
                        include_ocean = TRUE,
                        buffer_dist_km = NULL
                        ) {

  scalerank <- featurecla <- geometry <- x <- . <- NULL

  if (ne_scale == "large" && ne_type == "tiny_countries") {
    stop("tiny_countries are only available for medium (50 km) or small (110 km)
          scale maps")
  }

  if (level == "cube" && is.null(latlong_bbox)) {
    stop("A bounding box in EPSG:4326 (latitude and longitude) is required when
         level = 'cube'.")
  }

  if (!(level == "cube" || level == "world") && is.null(region)) {
    stop("You must provide a region unless level is set to 'cube' or 'world'.")
  }

  if (is.null(projected_crs)) {
    stop("No projected CRS provided.")
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

  # Project and validate the map
  map_data_projected <-
    map_data %>%
    sf::st_transform(crs = "ESRI:54012") %>%
    sf::st_make_valid()

  if (level == "cube") {

    expand_percent <- 0.5 # 10% buffer
    lng_range <- unname(latlong_bbox["xmax"] - latlong_bbox["xmin"])
    lat_range <- unname(latlong_bbox["ymax"] - latlong_bbox["ymin"])

    # Find bounding box
    min_lon <- unname(latlong_bbox["xmin"] - (expand_percent * lng_range))
    max_lon <- unname(latlong_bbox["xmax"] + (expand_percent * lng_range))
    min_lat <- unname(latlong_bbox["ymin"] - (expand_percent * lat_range))
    max_lat <- unname(latlong_bbox["ymax"] + (expand_percent * lat_range))

    latlong_extent <- c("xmin" = min_lon,
                        "xmax" = max_lon,
                        "ymin" = min_lat,
                        "ymax" = max_lat)

    # Project the extent
    extent_projected <-
      sf::st_bbox(latlong_extent, crs = "EPSG:4326") %>%
      sf::st_as_sfc() %>%
      sf::st_transform(crs = "ESRI:54012")

    # Set attributes as spatially constant to avoid warnings when clipping
    sf::st_agr(map_data_projected) <- "constant"

    # Crop the world map
    map_data_projected <- sf::st_crop(map_data_projected,
                                      extent_projected) %>%
      sf::st_make_valid() %>%
      sf::st_union() %>%
      sf::st_as_sf()

    extent_projected <- sf::st_bbox(map_data_projected)

  } else {

    extent_projected <- sf::st_bbox(map_data_projected)

    map_data_projected <- sf::st_make_valid(map_data_projected) %>%
      sf::st_union() %>%
      sf::st_as_sf()

  }

    if (include_ocean == TRUE) {

      extent_projected_polygon <- sf::st_as_sfc(extent_projected)

      map_data_ocean <- sf::st_difference(extent_projected_polygon,
                                          map_data_projected)

      # Validate oceans
      map_data_ocean <- sanitize_geometries(map_data_ocean)
      map_data_ocean <- map_data_ocean %>%
        sf::st_make_valid()

      # Remove empty geometries
      map_data_projected <- sanitize_geometries(map_data_projected)
      map_data_projected <- map_data_projected %>%
        sf::st_make_valid() %>%
        dplyr::filter(!is.na(sf::st_geometry(.))) %>%
        dplyr::filter(!sf::st_is_empty(.))

      # Check if the land data is empty before performing the union
      if (nrow(map_data_projected) == 0) {
        # If there's no land in the bounding box, return just the ocean layer
        map_data_projected <- map_data_ocean
      } else {
        # Otherwise, perform the union as normal
        #map_data_projected <- sf::st_union(map_data_projected, map_data_ocean)
        map_data_ocean_ <- sf::st_as_sf(map_data_ocean)
        map_data_projected <- bind_rows(map_data_projected,
                                               map_data_ocean_)
        map_data_projected <- map_data_projected %>%
          dplyr::group_by() %>%
          dplyr::reframe(geometry = sf::st_union(x)) %>%
          dplyr::ungroup() %>%
          sf::st_as_sf()

      }

      # Merge the land with the oceans
    #  map_data_projected <- sf::st_union(map_data_projected, map_data_ocean)

    } else if (is.character(include_ocean) &&
               include_ocean == "buffered_coast") {

        message(
          paste0(
            "Buffering land by ",
            buffer_dist_km,
            " km to include coastal water areas."
          )
        )

      if (!level %in% c("country", "sovereignty", "geounit") ||
          length(region) > 1) {
        warning(
          paste0(
            "Buffering the coastal areas may produce unexpected results when ",
            "used with level = 'cube' (default) or with areas encompassing ",
            "multiple countries, as it may create overlapping buffers or ",
            "buffer areas that you did not intend. These undesirable effects ",
            "might be mitigated by reducing the buffer distance and/or ",
            "setting visible_gridlines = FALSE when plotting."
          )
        )
      }

        map_data_ocean <- sf::st_buffer(map_data_projected,
                                        dist = buffer_dist_km * 1000)

        # Validate oceans
        map_data_ocean <- sf::st_make_valid(map_data_ocean)

        # Merge the land with the oceans
        map_data_projected <- sf::st_union(map_data_projected,
                                           map_data_ocean)

    }

  map_data_projected <- sf::st_transform(map_data_projected,
                                         crs = projected_crs)

  return(map_data_projected)

}


#' Create a Spatial Grid for Mapping
#'
#' Generates a grid of polygons covering a specified geographic area,
#' suitable for mapping data retrieved with the rnaturalearth package.
#'
#' @param bbox An sf bbox object provided by compute_indicate_workflow.
#' @param cell_size Cell length in kilometers.
# #' @param grid_units Cell size unit type: "km" or "degrees".
#' @param projected_crs The projected CRS the grid should be converted to.
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
create_grid <- function(bbox,
                        cell_size,
                        projected_crs,
                        make_valid = FALSE) {

  # Make a grid across the cube
  grid <- bbox %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(bbox["xmin"], bbox["ymin"])) %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  # Validate grid if make_valid set to TRUE
  if (make_valid==TRUE) {
    grid <- sf::st_make_valid(grid)
  }

  # Transform grid to projected crs
  grid <- sf::st_transform(grid, projected_crs)

  # Add area column to grid
  grid$area <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)
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
#' @param shapefile_path Path of an external shapefile to merge into the workflow. For example,
#'  if you want to calculate your indicator particular features such as protected areas or wetlands.
#' @param shapefile_crs CRS of a .wkt shapefile. If your shapefile is .wkt and you do
#'  NOT use this parameter, the CRS will be assumed to be EPSG:4326 and the
#'  coordinates will be read in as lat/long. If your shape is NOT a .wkt the CRS
#'  will be determined automatically.
#' @param invert Calculate an indicator over the inverse of the shapefile (e.g.
#'  if you have a protected areas shapefile this would calculate an indicator over
#'  all non protected areas)
#' @param include_ocean Include occurrences which fall outside the land area.
#'  Default is TRUE. Set as "buffered_coast" to include a set buffer size around
#'  the land area rather than the entire ocean area.
#' @param buffer_dist_km The distance to buffer around the land if include_ocean
#'  is set to "buffered_coast".
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
                                       shapefile_path = NULL,
                                       shapefile_crs = NULL,
                                       invert = FALSE,
                                       include_ocean = TRUE,
                                       buffer_dist_km = 50,
                                       ...) {

  stopifnot_error("Object class not recognized.",
                  inherits(data, "processed_cube") |
                    inherits(data, "processed_cube_dsinfo") |
                    inherits(data, "sim_cube"))

  available_indicators <- NULL; rm(available_indicators)

  geometry <- area <- cellid <- NULL

  # Check for empty cube
  if (nrow(data$data) == 0) {
    stop("No data found in the cube.")
  }

  # Check that obs column exists
  if (!"obs" %in% colnames(data$data)) {
    stop("No occurrences found in the data.")
  }

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

  # Ensure user has entered reasonable first and last years, then filter the
  # data accordingly. If user-chosen first and/or last years are outside the
  # range of the data, the actual first and last years of the data will be used.
  if (!is.null(first_year) && !is.null(last_year) && first_year > last_year) {
    stop("First year must be less than or equal to last year.")
  }
  # set first year
  if (!is.null(first_year)) {
    first_year <- ifelse(first_year > data$first_year,
                         first_year,
                         data$first_year)
  } else {
    first_year <- data$first_year
  }
  # set last year
  if (!is.null(last_year)) {
    last_year <- ifelse(last_year < data$last_year, last_year, data$last_year)
  } else {
    last_year <- data$last_year
  }
  # filter
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
    num_families <- data$num_families
    coord_range <- data$coord_range
    kingdoms <- data$kingdoms

    if (spherical_geometry==FALSE){
      # Temporarily disable spherical geometry
      sf::sf_use_s2(FALSE)
    }

    # Check shapefile path and load if found
    if (!is.null(shapefile_path)) {
      if (!file.exists(shapefile_path)) {
        stop("Shapefile not found at the specified path.")
      } else {
        is_wkt_file <- grepl("\\.wkt$", tolower(shapefile_path))
        if (is_wkt_file) {
          if (is.null(shapefile_crs)) {
            shapefile_crs <- "EPSG:4326"
            warning(paste(
              "You have provided the location to a .wkt shapefile without",
              "specifying the CRS. Assuming CRS to be EPSG:4326."))
          }
          shapefile <- sf::st_as_sfc(readLines(shapefile_path),
                                     crs = shapefile_crs)
        } else {
          shapefile <- sf::read_sf(shapefile_path)
        }
      }
    } else {
      shapefile <- NULL
    }

    # Determine cube CRS
    if (data$grid_type == "eea") {
      cube_crs <- "EPSG:3035"
    } else if (data$grid_type == "eqdgc") {
      cube_crs <- "EPSG:4326"
    } else if (data$grid_type == "mgrs") {
      cube_crs <- guess_utm_epsg(data)
    } else {
      stop("Grid reference system not found.")
    }

    # Determine projection to use for internal processing
    # Get cube extent in lat/long
    if (data$grid_type == "mgrs") {
      cube_bbox_latlong <- mgrs_to_latlong_bbox(df)
    } else {
      cube_bbox <- sf::st_bbox(c(xmin = coord_range[[1]],
                                 xmax = coord_range[[2]],
                                 ymin = coord_range[[3]],
                                 ymax = coord_range[[4]]),
                               crs = cube_crs)
      cube_bbox_latlong <- sf::st_as_sfc(cube_bbox) %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_bbox()
    }

    # Choose appropriate projection CRS
    if (data$grid_type == "eea") {
      projected_crs <- "EPSG:3035"
    } else if (data$grid_type == "mgrs") {
      projected_crs <- guess_utm_epsg(data)
    } else if (data$grid_type == "eqdgc") {
      projected_crs <- guess_utm_epsg(cube_bbox_latlong)
    } else {
      stop("Grid reference system not found.")
    }

    # Get cube extent in projected crs
    if (data$grid_type == "mgrs") {
      cube_bbox_projected <- sf::st_as_sfc(cube_bbox_latlong) %>%
        sf::st_transform(crs = projected_crs) %>%
        sf::st_bbox()
    } else {
      cube_bbox_projected <- sf::st_as_sfc(cube_bbox) %>%
        sf::st_transform(crs = projected_crs) %>%
        sf::st_bbox()
    }
    cube_polygon_projected <- sf::st_as_sfc(cube_bbox_projected)

    # Determine output CRS if not provided
    if (is.null(output_crs)) {
      if (data$grid_type == "eea" || data$grid_type == "mgrs") {
        output_crs <- projected_crs
      } else if (data$grid_type == "eqdgc") {
        output_crs <- "EPSG:4326"
      } else {
        stop("Grid reference system not found.")
      }
    } else {
    # If output CRS is provided, check if it is valid
      check_crs_units(output_crs)
    }



    # # Get units based on CRS
    # input_units <- check_crs_units(cube_crs)
    # projection_units <- check_crs_units(projection_crs)
    output_units <- check_crs_units(output_crs)
    #
    # # Check if input and output units match, and if not, stop with an error
    # # or issue a warning if the user has explicitly set crs_unit_convert to TRUE
    # if (crs_unit_convert == FALSE && input_units != output_units) {
    #   stop(
    #     paste0(
    #       "Cube CRS units are ", input_units, " while output CRS ",
    #       "units are ", output_units, ".\n The conversion could increase ",
    #       "processing time and lead to invalid output.\n If you are certain ",
    #       "you want to proceed you can force it with 'crs_unit_convert = TRUE'."
    #     )
    #   )
    # } else if (crs_unit_convert == TRUE && input_units != output_units) {
    #   warning(
    #     paste0(
    #       "Cube CRS units are ", input_units, " while output CRS ",
    #       "units are ", output_units, ".\n The conversion could lead ",
    #       "to invalid output."
    #     )
    #   )
    # }

    # Get current cell size
    if (stringr::str_detect(data$resolution, "degrees")) {
      input_cell_size <- as.numeric(stringr::str_extract(data$resolution,
                                                         "[0-9.]+(?=degrees)"))
    } else if (stringr::str_detect(data$resolution, "km")) {
      input_cell_size <- as.numeric(stringr::str_extract(data$resolution,
                                                         "[0-9.]+(?=km)"))
    } else {
      stop("Resolution units not recognized. Must be km or degrees.")
    }

    # Get cube area
    cube_area_sqkm <-
      cube_polygon_projected %>%
      sf::st_area() %>%
      units::set_units("km^2")

    # # Get cube cell codes for mgrs data, or set to NULL if not mgrs
    # # This is for transforming map data, as mgrs requires a localized CRS
    # if (data$grid_type == "mgrs") {
    #   cube_cell_codes <- df$cellCode
    # } else {
    #   cube_cell_codes <- NULL
    # }

    # Set output cell size (or if user provided one, check that it makes sense)
    cell_size <- check_cell_size(cell_size,
                                 data$resolution,
                                 level,
                                 cube_area_sqkm)

    # Create an sf object from cube data
    if (data$grid_type == "mgrs") {
      df_sf_input <- create_sf_from_utm(df, "EPSG:4326")
    } else {
      # Create an sf object from quarter-degree or EEA data
      df_sf_input <- sf::st_as_sf(df,
                                  coords = c("xcoord", "ycoord"),
                                  crs = cube_crs)
    }

    df_sf_projected <- sf::st_transform(df_sf_input, crs = projected_crs)

    # If shape file provided, check whether its bounding box intersects the
    # bounding box of the cube. We first convert to lat/long for consistency.
    if (!is.null(shapefile)) {
      # Get shapefile bounding box
      shapefile_latlong <- sf::st_transform(shapefile, crs = "EPSG:4326")
      shapefile_bbox_latlong <- sf::st_bbox(shapefile_latlong)
      # Convert bounding boxes to polygon geometries (sfc objects)
      shapefile_bbox_latlong_sfc <- sf::st_as_sfc(shapefile_bbox_latlong)
      cube_bbox_latlong_sfc <- sf::st_as_sfc(cube_bbox_latlong)
      if (sf::st_intersects(shapefile_bbox_latlong_sfc,
                            cube_bbox_latlong_sfc,
                            sparse = FALSE)[1, 1] == FALSE) {
        stop("Shapefile bounding box does not intersect the cube's area.")
      }

      # If there is an intersection of the latlong bounding boxes, we can
      # proceed with full intersection of the shapefile and the cube. But first
      # we match projections.
      if (sf::st_crs(shapefile) != projected_crs) {
        shapefile_projected <- sf::st_transform(shapefile, crs = projected_crs)
      }
      if (invert) {
        shapefile_merge <- sf::st_difference(cube_polygon_projected,
                                             sf::st_union(shapefile_projected))
      } else {
        shapefile_merge <- sf::st_union(cube_polygon_projected,
                                        shapefile_projected)
      }

      # Handle empty geometries after spatial operations
      if (length(shapefile_merge) == 0) {
        is_empty <- TRUE
      } else {
        is_empty <- sf::st_is_empty(shapefile_merge)
      }

      if (is_empty) {
        stop("Shapefile does not seem to be within the area of the cube.")
      }
      # Get bounding box of new object
      merged_bbox <- sf::st_bbox(shapefile_merge)

      # Filter data frame based on shapefile polygon
      # Initialize filtered_sf as NULL to capture the result of the
      # intersection
      filtered_sf <- NULL
      tryCatch({
        # Attempt without altering the spherical geometry setting
        filtered_sf <- sf::st_filter(df_sf_projected,
                                     sf::st_union(shapefile_merge))
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
        filtered_sf <- sf::st_filter(df_sf_projected,
                                     sf::st_union(shapefile_merge))
        # Notify success after retry
        message("Intersection succeeded with spherical geometry turned off.")
      }
      if (spherical_geometry == TRUE) {
        # Restore original spherical setting
        sf::sf_use_s2(original_s2_setting)
      }
      # Filter the original data frame
      df <- df[df$cellCode %in% filtered_sf$cellCode, ]
      if (nrow(df) == 0) {
        stop("No data points remain after spatial filtering.")
      }
    }
    # }

    # Unify object names
    if (!is.null(shapefile)) {
      bbox_latlong <- merged_bbox %>%
        sf::st_as_sfc() %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_bbox()
      bbox_projected <- merged_bbox
      data_projected <- filtered_sf
    } else {
      bbox_latlong <- cube_bbox_latlong
      bbox_projected <- cube_bbox_projected
      data_projected <- df_sf_projected
    }

    # Select appropriate bbox for grid creation
    if (data$grid_type == "eqdgc") {
      bbox_for_grid <- bbox_latlong
    } else {
      bbox_for_grid <- bbox_projected
    }

    # Retrieve and validate Natural Earth data
    map_data <- get_NE_data(bbox_latlong,
                            projected_crs,
                            region,
                            level,
                            ne_type,
                            ne_scale,
                            include_ocean,
                            buffer_dist_km) %>%
      sf::st_make_valid()

    if (data$grid_type == "eea") {
      map_data <- sf::st_transform(map_data, crs = projected_crs)
    }

    if (dim_type == "map") {

      # Create grid
      grid <- create_grid(bbox_for_grid,
                          cell_size,
                          projected_crs,
                          # output_units,
                          # output_crs,
                          make_valid)

      sf::st_agr(grid) <- "constant"

      # The following intersection operation requires special error handling
      # because it fails when the grid contains invalid geometries.
      # Therefore when invalid geometries are encountered, it will retry the
      # operation with spherical geometry turned off. This often succeeds.
      result <- NULL  # Initialize to capture result of intersection
      tryCatch({
        # Attempt without altering the spherical geometry setting
        result <- grid %>%
          sf::st_intersection(map_data) %>%
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
      }, error = function(e) {
        if (grepl("Error in wk_handle.wk_wkb", e) ||
            grepl("TopologyException", e)) {
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
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
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
      clipped_grid <- result

      # Assign data to grid
      data_gridded <- sf::st_join(data_projected, clipped_grid)
      data_gridded_nogeom <- sf::st_drop_geometry(data_gridded)

      # Assign classes to send data to correct calculator function
      subtype <- paste0(type, "_", dim_type)
      class(data_gridded_nogeom) <- append(type, class(data_gridded_nogeom))
      class(data_gridded_nogeom) <- append(subtype, class(data_gridded_nogeom))

      # Calculate indicator
      indicator <- calc_map(data_gridded_nogeom, ...)

      # Add indicator values to grid
      diversity_grid <-
        clipped_grid %>%
        dplyr::left_join(indicator, by = "cellid")

      diversity_grid <- sf::st_transform(diversity_grid, crs = output_crs)

    } else {

      data_filtered <- sf::st_filter(data_projected, map_data)
      data_filtered_nogeom <- sf::st_drop_geometry(data_filtered)

      # Assign classes to send data to correct calculator function
      subtype <- paste0(type, "_", dim_type)
      class(data_filtered_nogeom) <- append(type, class(data_filtered_nogeom))
      class(data_filtered_nogeom) <- append(subtype, class(data_filtered_nogeom))

      # Calculate indicator
      indicator <- calc_ts(data_filtered_nogeom, ...)
      # Calculate confidence intervals
      if (ci_type!="none") {
        if (!type %in% noci_list) {
          indicator <- calc_ci(data_filtered_nogeom,
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
      year_names <- unique(df$year)
      map_lims <- sf::st_transform(data_filtered, crs = output_crs) %>%
        sf::st_bbox()
    }

  } else {
    # if the object is of the class sim_cube it contains no grid information, so
    # bypass the usual workflow and deal with it here

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
      # Calculate confidence intervals
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

  if (spherical_geometry==FALSE) {
    # restore the original spherical geometry setting
    sf::sf_use_s2(original_s2_setting)
  }

  # Create indicator object
  if (dim_type == "map") {

    if (data$grid_type != "eqdgc") {
      cell_size <- cell_size / 1000
    }
    # Build indicator_map object
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
                                       years_with_obs = years_with_obs)
  } else {
    # Build indicator_ts object
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
