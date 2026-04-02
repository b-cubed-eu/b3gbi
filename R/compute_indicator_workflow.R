#' @title Calculate Biodiversity Indicators Over Space or Time
#'
#' @description This function provides a flexible framework for calculating
#'  various biodiversity indicators on a spatial grid or as a time series. It
#'  prepares the data, creates a grid, calculates indicators, and formats the
#'  output into an appropriate S3 object ('indicator_map' or 'indicator_ts').
#'
#' @param data A data cube object (class 'processed_cube').
#' @param type The indicator to calculate. Supported options include:
#'   * 'obs_richness': Observed species richness.
#'   * 'total_occ': Total number of occurrences.
#'   * 'newness': Mean year of occurrence.
#'   * 'occ_density': Density of occurrences.
#'   * 'spec_richness_density': Species richness density (richness / area).
#'   * 'williams_evenness', 'pielou_evenness': Evenness measures.
#'   * 'ab_rarity', 'area_rarity':  Abundance-based and area-based rarity
#'     scores.
#'   * 'cum_richness': Cumulative species richness.
#'   * 'occ_turnover': Occupancy turnover.
#'   * 'spec_range': Species range size.
#'   * 'spec_occ': Species occurrences.
#'   * 'tax_distinct': Taxonomic distinctness.
#'   * 'hill0': Species richness (estimated by coverage-based rarefaction).
#'   * 'hill1': Hill-Shannon diversity (estimated by coverage-based
#'     rarefaction).
#'   * 'hill2': Hill-Simpson diversity (estimated by coverage-based
#'     rarefaction).
#'   * 'completeness': Sample completeness (Sample Coverage).
#' @param dim_type (Optional) Dimension to calculate indicator over time: 'ts',
#'  or space: 'map'. (Default: 'map')
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm"). Select "none" to avoid calculating bootstrap
#'  CIs.
#' @param cell_size (Optional) Length of grid cell sides, in km or degrees.
#'  If set to "grid" (default), this will use the existing grid size of your
#'  cube. If set to "auto", this will be automatically determined according to
#'  the geographical level selected. This is 100 km or 1 degree for 'continent'
#'  or 'world', 10 km or (for a degree-based CRS) the native resolution of the
#'  cube for 'country', 'sovereignty' or 'geounit'. If level is set to 'cube',
#'  cell size will be the native resolution of the cube for a degree-based CRS,
#'  or for a km-based CRS, the cell size will be determined by the area of the
#'  cube: 100 km for cubes larger than 1 million sq km, 10 km for cubes between
#'  10 thousand and 1 million sq km, 1 km for cubes between 100 and 10 thousand
#'  sq km, and 0.1 km for cubes smaller than 100 sq km. Alternatively, the user
#'  can manually select the grid cell size (in km or degrees). Note that the
#'  cell size must be a whole number multiple of the cube's resolution.
#' @param level (Optional) Spatial level: 'cube', 'continent', 'country',
#'  'world', 'sovereignty', or 'geounit'. (Default: 'cube')
#' @param region (Optional) The region of interest (e.g., "Europe"). This
#'  parameter is ignored if level is set to 'cube' or 'world'. (Default: NULL)
#' @param ne_type (Optional) The type of Natural Earth data to download:
#'  'countries', 'map_units', 'sovereignty', or 'tiny_countries'. This parameter
#'  is ignored if level is set to 'cube' or 'world'. (Default: "countries")
#' @param ne_scale (Optional) The scale of Natural Earth data to download:
#'  'small' - 110m, 'medium' - 50m, or 'large' - 10m. (Default: "medium")
#' @param output_crs (Optional) The CRS you want for your calculated indicator.
#'  (Leave blank to let the function choose a default based on grid reference
#'  system.)
#' @param first_year (Optional) Exclude data before this year. (Uses all data in
#'  the cube by default.)
#' @param last_year (Optional) Exclude data after this year. (Uses all data in
#'  the cube by default.)
#' @param spherical_geometry (Optional) If set to FALSE, will temporarily
#'  disable spherical geometry while the function runs. Should only be used to
#'  solve specific issues. (Default is TRUE).
#' @param make_valid (Optional) Calls st_make_valid() from the sf package
#'  after creating the grid. Increases processing time but may help if you are
#'  getting polygon errors. (Default is FALSE).
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 100)
#' @param shapefile_path (optional) Path of an external shapefile to merge into
#'  the workflow. For example, if you want to calculate your indicator
#'  particular features such as protected areas or wetlands.
#' @param shapefile_crs (Optional) CRS of a .wkt shapefile. If your shapefile
#'  is .wkt and you do NOT use this parameter, the CRS will be assumed to be
#'  EPSG:4326 and the coordinates will be read in as lat/long. If your shape is
#'  NOT a .wkt the CRS will be determined automatically.
#' @param invert (optional) Calculate an indicator over the inverse of the
#'  shapefile (e.g. if you have a protected areas shapefile this would calculate
#'  an indicator over all non protected areas within your cube). Default is
#'  FALSE.
#' @param include_land (Optional) Include occurrences which fall within the
#'  land area. Default is TRUE. *Note that this purely a geographic filter, and
#'  does not filter based on whether the occurrence is actually terrestrial.
#'  Grid cells which fall partially on land and partially on ocean will be
#'  included even if include_land is FALSE. To exclude terrestrial and/or
#'  freshwater taxa, you must manually filter your data cube before calculating
#'  your indicator.
#' @param include_ocean (Optional) Include occurrences which fall outside the
#'  land area. Default is TRUE. Set as "buffered_coast" to include a set buffer
#'  size around the land area rather than the entire ocean area. *Note that this
#'  is purely a geographic filter, and does not filter based on whether the
#'  occurrence is actually marine. Grid cells which fall partially on land and
#'  partially on ocean will be included even if include_ocean is FALSE. To
#'  exclude marine taxa, you must manually filter your data cube before
#'  calculating your indicator.
#' @param buffer_dist_km (Optional) The distance to buffer around the land if
#'  include_ocean is set to "buffered_coast". Default is 50 km.
#' @param force_grid (Optional) Forces the calculation of a grid even if this
#'  would not normally be part of the pipeline, e.g. for time series. This
#'  setting is required for the calculation of rarity or Hill diversity, and is
#'  forced on by indicators that require it. (Default: FALSE)
#' @param ... Additional arguments passed to specific indicator calculation
#'  functions.
#'
#' @return An S3 object containing the calculated indicator values and metadata.
#'
#' @examples
#' diversity_map <- compute_indicator_workflow(example_cube_1,
#'   type = "obs_richness",
#'   dim_type = "map",
#'   level = "country",
#'   region = "Denmark"
#' )
#' diversity_map
#'
#' @export
compute_indicator_workflow <- function(data,
                                       type,
                                       dim_type = c(
                                         "map",
                                         "ts"
                                       ),
                                       ci_type = c(
                                         "norm",
                                         "basic",
                                         "perc",
                                         "bca",
                                         "none"
                                       ),
                                       cell_size = "grid",
                                       level = c(
                                         "cube",
                                         "continent",
                                         "country",
                                         "world",
                                         "sovereignty",
                                         "geounit"
                                       ),
                                       region = "Europe",
                                       ne_type = c(
                                         "countries",
                                         "map_units",
                                         "sovereignty",
                                         "tiny_countries"
                                       ),
                                       ne_scale = c(
                                         "medium",
                                         "small",
                                         "large"
                                       ),
                                       output_crs = NULL,
                                       first_year = NULL,
                                       last_year = NULL,
                                       spherical_geometry = TRUE,
                                       make_valid = FALSE,
                                       num_bootstrap = 100,
                                       shapefile_path = NULL,
                                       shapefile_crs = NULL,
                                       invert = FALSE,
                                       include_land = TRUE,
                                       include_ocean = TRUE,
                                       buffer_dist_km = 50,
                                       force_grid = FALSE,
                                       ...) {
  # Save original cell_size to determine whether to use native grid later
  original_cell_size <- cell_size

  # Extract gridded_average from dots if present
  dots <- list(...)
  gridded_average <- if ("gridded_average" %in% names(dots)) {
    dots$gridded_average
  } else {
    FALSE
  }

  wrong_class(data,
    class = c("processed_cube", "processed_cube_dsinfo", "sim_cube"),
    reason = "unrecognized"
  )

  # For gridded average completeness, we MUST have a grid coarser than the native
  # cube resolution to ensure multiple native cells (samples) per grid cell.
  if (type == "completeness" && gridded_average == TRUE && is.null(cell_size)) {
    native_res_str <- if ("resolution" %in% names(data$data)) data$data$resolution[1] else NULL
    if (!is.null(native_res_str)) {
      res_val <- as.numeric(gsub("[a-zA-Z]", "", native_res_str))
      res_unit <- gsub("[0-9.]", "", native_res_str)

      # Force a 4x coarser grid (16 native cells per grid cell)
      coarser_res <- res_val * 4
      cell_size <- paste0(coarser_res, res_unit)
      message("Forcing coarser grid resolution for completeness: ", cell_size)
    }
  }

  # Null assignments
  xcoord <- ycoord <- ll <- ul <- NULL

  # Check for empty cube
  if (nrow(data$data) == 0) {
    stop("No data found in the cube.")
  }

  # Check that obs column exists
  if (!"obs" %in% colnames(data$data)) {
    stop("No occurrences found in the data.")
  }

  # Check that user has not excluded both land and ocean data
  if (!include_land && !include_ocean) {
    stop("You must include either land or ocean data, or both.")
  }

  # List of indicators that require grid cells for temporal calculations
  ind_req_grid_list <- c(
    "area_rarity",
    "hill0",
    "hill1",
    "hill2"
  )

  if (type == "completeness" && gridded_average == TRUE) {
    ind_req_grid_list <- c(ind_req_grid_list, "completeness")
    force_grid <- TRUE
  }

  # List of indicators for which bootstrapped confidence intervals should not
  # be calculated
  noci_list <- c(
    "obs_richness",
    "spec_richness_density",
    "cum_richness",
    "occ_turnover",
    "tax_distinct",
    "hill0",
    "hill1",
    "hill2",
    "completeness"
  )

  type <- match.arg(type, names(available_indicators))
  dim_type <- match.arg(dim_type)
  ci_type <- match.arg(ci_type)
  ne_type <- match.arg(ne_type)
  ne_scale <- match.arg(ne_scale)
  level <- match.arg(level)

  # Check that user is not trying to calculate an indicator that requires grid
  # cell assignment with a cube that lacks a supported grid system.
  if (!data$grid_type %in% c("eea", "mgrs", "eqdgc", "isea3h")) {
    if (dim_type == "map") {
      stop(
        paste0(
          "Grid system is either unsupported or missing. Spatial ",
          "indicators require a supported grid system. Currently ",
          "supported grid systems are: EEA, MGRS, EQDGC, ISEA3H"
        )
      )
    } else if (type %in% ind_req_grid_list) {
      stop(
        paste0(
          "You are attempting to calculate an indicator that requires ",
          "assigned grid cells, but this cube has a missing or ",
          "unsupported grid system. Please choose a different indicator."
        )
      )
    } else if (!is.null(shapefile_path)) {
      stop(
        paste(
          "Unsupported or missing grid system. Shapefile cannot be used."
        )
      )
    } else if (level != "cube") {
      level <- "cube"
      warning("Unsupported or missing grid system. Setting level to 'cube'.")
    }
  }

  if (type %in% c("hill0", "hill1", "hill2") &&
    ci_type %in% c("norm", "basic", "bca")) {
    message(
      paste0(
        "Note: Hill diversity measures are calculated by the iNEXT package, ",
        "therefore bootstrap confidence intervals will be calculated using ",
        "the standard iNEXT method, similar to the 'percentile' method of ",
        "the 'boot' package."
      )
    )
  }

  # Ensure user has entered reasonable first and last years, then filter the
  # data accordingly. If user-chosen first and/or last years are outside the
  # range of the data, the actual first and last years of the data will be used.
  if (!is.null(first_year) && !is.null(last_year) && first_year > last_year) {
    stop("First year must be less than or equal to last year.")
  }
  # Set first year
  if (!is.null(first_year)) {
    first_year <- ifelse(first_year > data$first_year,
      first_year,
      data$first_year
    )
  } else {
    first_year <- data$first_year
  }
  # Set last year
  if (!is.null(last_year)) {
    last_year <- ifelse(last_year < data$last_year, last_year, data$last_year)
  } else {
    last_year <- data$last_year
  }
  # Filter years
  df <- data$data[(data$data$year >= first_year) &
    (data$data$year <= last_year), ]

  # Collect information to add to final object
  num_species <- data$num_species
  num_years <- length(unique(df$year))
  species_names <- unique(df$scientificName)
  years_with_obs <- unique(df$year)
  kingdoms <- data$kingdoms
  num_families <- data$num_families

  # Ensure coord_range is valid for ISEA3H or other grids
  if (is.null(data$coord_range) || any(is.na(unlist(data$coord_range)))) {
    if (nrow(df) > 0) {
      data$coord_range <- list(
        xmin = min(df$xcoord, na.rm = TRUE),
        xmax = max(df$xcoord, na.rm = TRUE),
        ymin = min(df$ycoord, na.rm = TRUE),
        ymax = max(df$ycoord, na.rm = TRUE)
      )
    }
  }

  coord_range <- data$coord_range
  map_lims <- if (is.list(coord_range)) {
    unlist(coord_range)
  } else {
    coord_range
  }
  region <- if (level == "cube") {
    "cube"
  } else if (level == "world") {
    "world"
  } else {
    region
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
            "specifying the CRS. Assuming CRS to be EPSG:4326."
          ))
        }
        shapefile <- sf::st_as_sfc(readLines(shapefile_path),
          crs = shapefile_crs
        )
      } else {
        shapefile <- sf::read_sf(shapefile_path)
      }
    }
  } else {
    shapefile <- NULL
  }

  if (!is.null(shapefile) ||
    dim_type == "map" ||
    level != "cube" ||
    force_grid == TRUE) {
    # Determine cube CRS
    if (data$grid_type == "eea") {
      cube_crs <- "EPSG:3035"
    } else if (data$grid_type == "eqdgc") {
      cube_crs <- "EPSG:4326"
    } else if (data$grid_type == "mgrs") {
      cube_crs <- guess_utm_epsg(data)
    } else if (data$grid_type == "isea3h") {
      cube_crs <- "EPSG:4326"
    } else {
      stop("Grid reference system not found.")
    }

    # Get cube extent in lat/long
    if (data$grid_type == "mgrs") {
      cube_bbox_latlong <- mgrs_to_latlong_bbox(df)
    } else {
      cube_bbox <- sf::st_bbox(
        c(
          xmin = coord_range[[1]],
          xmax = coord_range[[2]],
          ymin = coord_range[[3]],
          ymax = coord_range[[4]]
        ),
        crs = cube_crs
      )
      cube_bbox_latlong <- sf::st_as_sfc(cube_bbox) %>%
        sf::st_transform(crs = "EPSG:4326") %>%
        sf::st_bbox()
    }

    # Choose appropriate projection CRS
    if (data$grid_type == "eea") {
      projected_crs <- cube_crs
    } else if (data$grid_type == "eqdgc") {
      projected_crs <- "ESRI:54012"
    } else if (data$grid_type == "mgrs") {
      projected_crs <- guess_utm_epsg(cube_bbox_latlong)
    } else if (data$grid_type == "isea3h") {
      projected_crs <- "ESRI:54012"
    } else {
      stop("Grid reference system not found.")
    }

    # Store the current spherical geometry setting
    original_s2_setting <- sf::sf_use_s2()

    if (spherical_geometry == FALSE) {
      # Temporarily disable spherical geometry
      sf::sf_use_s2(FALSE)
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
      } else if (data$grid_type == "isea3h") {
        output_crs <- "EPSG:4326"
      } else {
        stop("Grid reference system not found.")
      }
    } else {
      # If output CRS is provided, check if it is valid
      check_crs_units(output_crs)
    }

    # # Get units based on CRS
    output_units <- check_crs_units(output_crs)

    # Create an sf object from cube data
    if (data$grid_type == "mgrs") {
      df_sf_input <- create_sf_from_utm(df, "EPSG:4326")
    } else if (data$grid_type == "eea") {
      res_size <- as.numeric(stringr::str_extract(df$resolution, "[0-9.]+(?=km)"))
      half_res_size <- res_size / 2
      df_offset <- df %>%
        dplyr::mutate(
          xcoord_offset = xcoord + half_res_size,
          ycoord_offset = ycoord + half_res_size
        )
      # Create an sf object from EEA data
      df_sf_input <- sf::st_as_sf(df_offset,
        coords = c("xcoord_offset", "ycoord_offset"),
        crs = "EPSG:3035"
      )
    } else {
      # Create an sf object from quarter-degree or EEA data
      # For ISEA3H, the coordinates are centroids in EPSG:4326
      df_sf_input <- sf::st_as_sf(df,
        coords = c("xcoord", "ycoord"),
        crs = cube_crs
      )
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
        sparse = FALSE
      )[1, 1] == FALSE) {
        stop("Shapefile bounding box does not intersect the cube's area.")
      }

      # If there is an intersection of the latlong bounding boxes, we can
      # proceed with full intersection of the shapefile and the cube. But first
      # we match projections.
      if (sf::st_crs(shapefile) != projected_crs) {
        shapefile_projected <- sf::st_transform(shapefile, crs = projected_crs)
      }
      if (invert) {
        shapefile_merge <- sf::st_difference(
          cube_polygon_projected,
          sf::st_union(shapefile_projected)
        )
      } else {
        shapefile_merge <- sf::st_union(
          cube_polygon_projected,
          shapefile_projected
        )
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
      tryCatch(
        {
          # Attempt without altering the spherical geometry setting
          filtered_sf <- sf::st_filter(
            df_sf_projected,
            sf::st_union(shapefile_merge)
          )
        },
        error = function(e) {
          if (grepl("Error in wk_handle.wk_wkb", e)) {
            message(paste(
              "Encountered a geometry error during intersection. ",
              "This may be due to invalid polygons in the grid."
            ))
          } else {
            stop(e)
          }
        }
      )
      if (is.null(filtered_sf)) {
        # If intersection failed, turn off spherical geometry
        message(
          "Retrying the intersection with spherical geometry turned off."
        )
        sf::sf_use_s2(FALSE)
        # Retry the intersection operation
        filtered_sf <- sf::st_filter(
          df_sf_projected,
          sf::st_union(shapefile_merge)
        )
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
    } else if (level != "cube") {
      # Early filtering for non-cube levels without shapefile
      # This ensures 'df' used for grid creation is limited to the region
      if (data$grid_type == "mgrs") {
        # Use create_sf_from_utm to correctly handle multi-zone MGRS data
        df_sf_temp <- create_sf_from_utm(df, output_crs = projected_crs)
      } else {
        df_sf_temp <- sf::st_as_sf(df, coords = c("xcoord", "ycoord"), crs = cube_crs)
        df_sf_temp <- sf::st_transform(df_sf_temp, crs = projected_crs)
      }

      # We need map_data to filter
      # Disable s2 during map data retrieval to avoid topology errors
      orig_s2_ne_early <- sf::sf_use_s2()
      sf::sf_use_s2(FALSE)
      map_data_list_temp <- get_ne_data(
        projected_crs,
        cube_bbox_latlong,
        region,
        level,
        ne_type,
        ne_scale,
        include_land,
        include_ocean,
        buffer_dist_km
      )
      filtered_sf_temp <- sf::st_filter(df_sf_temp, map_data_list_temp$combined)
      sf::sf_use_s2(orig_s2_ne_early)

      df <- df[df$cellCode %in% filtered_sf_temp$cellCode, ]
      if (nrow(df) == 0) {
        # Don't stop yet, maybe they are just outside the land but in ocean
        # Actually get_ne_data should handle ocean if include_ocean=TRUE
        stop("No data points remain in the specified region after spatial filtering.")
      }

      # Update coord_range to match the filtered data
      data$coord_range <- list(
        xmin = min(df$xcoord, na.rm = TRUE),
        xmax = max(df$xcoord, na.rm = TRUE),
        ymin = min(df$ycoord, na.rm = TRUE),
        ymax = max(df$ycoord, na.rm = TRUE)
      )
      coord_range <- data$coord_range
    } else {
      # For level = 'cube', also ensure coord_range matches the filtered 'df'
      if (nrow(df) > 0) {
        data$coord_range <- list(
          xmin = min(df$xcoord, na.rm = TRUE),
          xmax = max(df$xcoord, na.rm = TRUE),
          ymin = min(df$ycoord, na.rm = TRUE),
          ymax = max(df$ycoord, na.rm = TRUE)
        )
        coord_range <- data$coord_range
      }
    }

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
    # Disable s2 during map data retrieval to avoid topology errors
    orig_s2_ne <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    map_data_list <- get_ne_data(
      projected_crs,
      bbox_latlong,
      region,
      level,
      ne_type,
      ne_scale,
      include_land,
      include_ocean,
      buffer_dist_km
    )
    sf::sf_use_s2(orig_s2_ne)
    map_data <- map_data_list$combined
    saved_layer <- map_data_list$saved

    # Define the final study polygon
    if (!is.null(shapefile)) {
      # Final polygon is determined by shapefile intersection
      final_study_polygon <- sf::st_as_sf(shapefile_merge)
    } else {
      # Final polygon is determined by Natural Earth map data
      final_study_polygon <- map_data
    }

    # Calculate the final, correct area of the study region
    final_area_sqkm <-
      final_study_polygon %>%
      sf::st_union() %>% # Union handles multi-polygons (e.g., countries)
      sf::st_transform(crs = "ESRI:54012") %>% # Mollweide equal-area projection
      sf::st_area() %>%
      units::set_units("km^2")

    if (dim_type == "map" || force_grid == TRUE) {
      # Set output cell size (or if user provided one, check if it makes sense)
      # Compatibility fix: check both plural and singular resolution slots
      res_val <- data$resolutions[1] %||% data$resolution
      resolution_arg <- if (data$grid_type == "isea3h") "isea3h" else res_val

      cell_size <- check_cell_size(
        cell_size,
        resolution_arg,
        level,
        final_area_sqkm
      )
    }
  } else {
    # This block handles level == "cube", no shapefile, dim_type != "map"

    if (!"sim_cube" %in% class(data)) {
      # Determine cube CRS
      cube_crs <- if (data$grid_type == "eea") {
        "EPSG:3035"
      } else if (data$grid_type == "mgrs") {
        guess_utm_epsg(data)
      } else {
        "EPSG:4326"
      }

      # Calculate the area of the cube's extent
      cube_bbox <- sf::st_bbox(
        c(
          xmin = coord_range[[1]],
          xmax = coord_range[[2]],
          ymin = coord_range[[3]],
          ymax = coord_range[[4]]
        ),
        crs = cube_crs
      )

      final_area_sqkm <-
        sf::st_as_sfc(cube_bbox) %>%
        sf::st_transform(crs = "ESRI:54012") %>% # Mollweide equal-area projection
        sf::st_area() %>%
        units::set_units("km^2")
    } else {
      final_area_sqkm <- NA
    }
  }

  if (dim_type == "map" || force_grid == TRUE) {
    # Create grid
    if (data$grid_type == "isea3h") {
      # Use pre-existing centroids to build hexagonal polygons
      grid <- create_isea3h_grid(df, projected_crs)
    } else if (data$grid_type %in% c("mgrs", "eea", "eqdgc")) {
      # Use native grid polygons to avoid aliasing issues (#104)
      # ALWAYS use the native data resolution for grid creation.
      # User-specified cell_size is handled at the aggregation level,
      # not at the grid geometry level, to ensure every data cellCode
      # has a matching grid cell.
      res_info <- if (!is.null(data$resolutions)) data$resolutions[1] else NULL
      grid <- create_native_grid(df, projected_crs, data$grid_type, res_info)
    } else {
      grid <- create_grid(
        bbox_for_grid,
        cell_size,
        projected_crs,
        make_valid
      )
    }

    sf::st_agr(grid) <- "constant"

    # Define the object to be used for intersection
    # If a shapefile is provided, use it directly. Otherwise, use the map data.
    if (!is.null(shapefile)) {
      if (invert) {
        # If invert is TRUE, we want the area *outside* the shapefile but inside
        # the cube. We use st_difference for this.
        intersection_target <- sf::st_difference(
          cube_polygon_projected, sf::st_union(shapefile_projected)
        )
      } else {
        # If invert is FALSE, we simply want the area of the shapefile
        intersection_target <- shapefile_projected
      }
      # Convert to sf if not already
      intersection_target <- sf::st_as_sf(intersection_target)
    } else {
      intersection_target <- map_data
    }

    # Ensure intersection target is valid and buffered slightly to avoid precision losses
    # Disable s2 to avoid topology exceptions with complex geometries
    original_s2_for_target <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    intersection_target <- sf::st_make_valid(intersection_target)
    # Only apply safety buffer if in a projected CRS (meters), not geographic (degrees)
    if (!sf::st_is_longlat(intersection_target)) {
      intersection_target <- sf::st_buffer(intersection_target, dist = 1) %>%
        sf::st_make_valid()
    }
    sf::sf_use_s2(original_s2_for_target)
    sf::st_agr(intersection_target) <- "constant"

    # Intersect grid with intersection target
    # Keep s2 disabled for spatial operations on complex map geometries
    if (data$grid_type == "isea3h") {
      # ISEA3H requires accurate clipping for dateline/pole handling
      clipped_grid <- intersect_grid_with_polygon(grid, intersection_target)
    } else if (level == "cube" && is.null(shapefile)) {
      # For cube level without shapefile, skip spatial filtering to prevent outlier loss
      clipped_grid <- grid
    } else {
      # Use st_intersection to clip grid cells to the map boundary.
      # Unlike st_filter (which removes cells), st_intersection clips cells
      # at the boundary, preserving coastal cells that partially overlap land.
      sf::sf_use_s2(FALSE)
      clipped_grid <- sf::st_intersection(
        grid,
        sf::st_union(intersection_target)
      )
      sf::sf_use_s2(original_s2_for_target)
      # Remove empty results from cells entirely outside the boundary
      clipped_grid <- clipped_grid[!sf::st_is_empty(clipped_grid), ]
    }

    if (spherical_geometry == TRUE) {
      # Restore original spherical setting
      sf::sf_use_s2(original_s2_setting)
    }

    # Filter data to only those within the intersection target
    data_filtered <- if (level == "cube" && is.null(shapefile)) {
      data_projected
    } else {
      sf::sf_use_s2(FALSE)
      result <- sf::st_filter(data_projected, intersection_target)
      sf::sf_use_s2(original_s2_for_target)
      result
    }

    # Assign data to grid
    if (data$grid_type %in% c("isea3h", "mgrs", "eea", "eqdgc") &&
      "cellCode" %in% colnames(clipped_grid) &&
      gridded_average == FALSE) {
      # These grids have a native cellCode mapping.
      # Join by cellCode instead of spatial join to preserve this assignment.
      # This avoids aliasing and gaps when using different projections.
      # SKIP this if gridded_average is TRUE, as we are aggregating to a
      # different (coarser) scale where native cellCodes won't match perfectly.

      # Include area in the lookup as some indicators (e.g. density) require it
      lookup_cols <- c("cellCode", "cellid")
      if ("area" %in% names(clipped_grid)) lookup_cols <- c(lookup_cols, "area")

      cell_lookup <- sf::st_drop_geometry(clipped_grid[, lookup_cols])
      data_pre_join <- sf::st_drop_geometry(data_filtered)
      n_pre_join <- nrow(data_pre_join)
      data_final <- dplyr::left_join(
        data_pre_join, cell_lookup,
        by = "cellCode",
        relationship = "many-to-one"
      )
      n_unmatched <- sum(is.na(data_final$cellid))
      if (n_unmatched > 0) {
        unmatched_codes <- unique(data_final$cellCode[is.na(data_final$cellid)])
        n_obs_lost <- sum(data_final$obs[is.na(data_final$cellid)], na.rm = TRUE)
        warning(
          sprintf(
            paste0(
              "%s of %s data rows (%s occurrences) could not be matched to a ",
              "grid cell. This affects %s unique cell codes. ",
              "This may be due to cell codes that could not be parsed into ",
              "grid coordinates. Use `options(warn=1)` to see the specific ",
              "cell codes on the next line."
            ),
            format(n_unmatched, big.mark = ","),
            format(n_pre_join, big.mark = ","),
            format(n_obs_lost, big.mark = ","),
            format(length(unmatched_codes), big.mark = ",")
          )
        )
        if (length(unmatched_codes) <= 20) {
          warning(paste("Unmatched cell codes:", paste(unmatched_codes, collapse = ", ")))
        } else {
          warning(paste("First 20 unmatched cell codes:", paste(head(unmatched_codes, 20), collapse = ", ")))
        }
      }
      data_final <- data_final[!is.na(data_final$cellid), ]
    } else {
      # For spatial join, only keep cellid and area from the grid to avoid
      # renaming conflicts with data columns (like cellCode).
      lookup_cols <- "cellid"
      if ("area" %in% names(clipped_grid)) lookup_cols <- c(lookup_cols, "area")

      data_final <- data_filtered %>%
        sf::st_join(clipped_grid[, lookup_cols], join = sf::st_nearest_feature)
    }

    # If user specified a coarser cell_size, aggregate data to coarser grid
    # BEFORE indicator calculation so indicators are calculated correctly
    if (original_cell_size != "grid" &&
        data$grid_type %in% c("eea", "mgrs", "eqdgc")) {
      agg_result <- aggregate_data_to_coarser_grid(
        data_final, clipped_grid, cell_size,
        if (data$grid_type == "eqdgc") "EPSG:4326" else projected_crs
      )
      data_final <- agg_result$data
      clipped_grid <- agg_result$grid
    }

    data_final_nogeom <- if (inherits(data_final, "sf")) {
      sf::st_drop_geometry(data_final)
    } else {
      data_final
    }
    map_lims <- sf::st_transform(
      clipped_grid,
      crs = output_crs
    ) %>% sf::st_bbox()
  } else if (level != "cube") {
    data_final <- sf::st_filter(data_projected, map_data)
    data_final_nogeom <- sf::st_drop_geometry(data_final)
    map_lims <- sf::st_transform(data_final, crs = output_crs) %>%
      sf::st_bbox()
  } else {
    data_final <- df
    data_final_nogeom <- df
  }

  # Assign classes to send data to correct calculator function
  subtype <- paste0(type, "_", dim_type)
  class(data_final_nogeom) <- append(type, class(data_final_nogeom))
  class(data_final_nogeom) <- append(subtype, class(data_final_nogeom))

  # Make total area available for use in certain indicator calculations
  if (!is.null(final_area_sqkm)) {
    attr(data_final_nogeom, "total_area_sqkm") <- as.numeric(final_area_sqkm)
  }

  # print(sum(data_final_nogeom$obs))

  if (dim_type == "map") {
    # Calculate indicator
    indicator <- calc_map(data_final_nogeom, ...)

    # Add indicator values to grid
    join_cols <- unique(c("cellid", intersect(names(clipped_grid), names(indicator))))
    diversity_grid <-
      clipped_grid %>%
      dplyr::left_join(indicator, by = join_cols)

    # Transform to output CRS
    diversity_grid <- sf::st_transform(diversity_grid, crs = output_crs)

    # Recalculate area (grid cells were already clipped to boundary above)
    if ("area" %in% names(diversity_grid)) {
      diversity_grid$area <- diversity_grid %>%
        sf::st_area() %>%
        units::set_units("km^2")
    }
  } else {
    # Calculate indicator
    indicator <- calc_ts(data_final_nogeom, ...)

    # Calculate confidence intervals
    if (ci_type != "none") {
      if (!type %in% noci_list) {
        indicator <- calc_ci(data_final_nogeom,
          indicator = indicator,
          num_bootstrap = num_bootstrap,
          ci_type = ci_type,
          ...
        )
      } else {
        if (!type %in% c("hill0", "hill1", "hill2", "completeness")) {
          warning(
            paste0(
              "Bootstrapped confidence intervals cannot be calculated for the ",
              "chosen indicator."
            )
          )
        }
      }
    }

    if (ci_type == "none" &&
      type %in% c("hill0", "hill1", "hill2")) {
      if ("ll" %in% names(indicator)) {
        indicator <- indicator %>% select(c(-ll, -ul))
      }
    }
  }

  if (spherical_geometry == FALSE) {
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
      map_type = ne_type,
      kingdoms = kingdoms,
      num_families = num_families,
      num_species = num_species,
      first_year = first_year,
      last_year = last_year,
      num_years = num_years,
      species_names = species_names,
      years_with_obs = years_with_obs,
      grid_type = data$grid_type
    )
  } else {
    # Build indicator_ts object
    diversity_obj <- new_indicator_ts(dplyr::as_tibble(indicator),
      div_type = type,
      map_level = level,
      map_region = region,
      map_type = ne_type,
      kingdoms = kingdoms,
      num_families = num_families,
      num_species = num_species,
      num_years = num_years,
      species_names = species_names,
      coord_range = map_lims
    )
  }

  return(diversity_obj)
}
