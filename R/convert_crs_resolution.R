#' Calculate approximate meters per degree at a given latitude
#'
#' @param latitude Latitude in decimal degrees
#' @return Named vector with meters per degree longitude and latitude
#'
#' @noRd
#'
meters_per_degree <- function(latitude) {
  # Constants for Earth's dimensions (WGS84)
  a <- 6378137.0  # semi-major axis in meters
  f <- 1/298.257223563  # flattening
  b <- a * (1 - f)  # semi-minor axis

  # Convert latitude to radians
  lat_rad <- latitude * pi/180

  # Calculate meters per degree longitude at this latitude
  meters_per_degree_lon <- (pi/180) * a * cos(lat_rad)

  # Calculate meters per degree latitude (changes slightly with latitude)
  # This is an approximation that works well for most purposes
  meters_per_degree_lat <- (pi/180) * b

  return(c(longitude = meters_per_degree_lon, latitude = meters_per_degree_lat))
}

#' Determine appropriate cell size when changing CRS
#'
#' @param data Input spatial data (sf or terra object)
#' @param input_resolution Original resolution as vector c(x, y)
#' @param input_crs CRS of the input data (EPSG code or proj4string)
#' @param target_crs Target CRS (EPSG code or proj4string)
#' @param input_units Units of the input data (degrees or km)
#' @param target_units Units of the output (degrees or km)
#' @param target_resolution Optional. If provided, validates if it's appropriate
#' @return A list with recommended cell size in target CRS units and validation result
#'
#' @noRd
#'
determine_cell_size <- function(data,
                                input_resolution,
                                input_crs,
                                target_crs,
                                input_units,
                                target_units,
                                target_resolution = NULL) {

  # Get the centroid of the data to use for conversion calculations
  if (inherits(data, "sf")) {
    data_bbox <- sf::st_bbox(data)
    centroid <- c(
      (data_bbox["xmin"] + data_bbox["xmax"]) / 2,
      (data_bbox["ymin"] + data_bbox["ymax"]) / 2
    )
  } else if (inherits(data, "SpatRaster")) {
    # For terra objects
    data_bbox <- terra::ext(data)
    centroid <- c(
      (data_bbox[1] + data_bbox[2]) / 2,
      (data_bbox[3] + data_bbox[4]) / 2
    )
  } else {
    stop("Data must be an sf or terra object")
  }

  # Check if target CRS uses km (not always correctly reported by units)
  is_km_crs <- grepl("\\+units=km", sf::st_crs(target_crs)$proj4string)

  # For debugging
  message(sprintf("Input CRS units: %s", input_units))
  message(sprintf("Target CRS units: %s%s", target_units, if(is_km_crs) " (km detected in proj4string)" else ""))

  # Initialize result list
  result <- list(
    recommended_cell_size = NULL,
    is_valid = TRUE,
    message = "Cell size is appropriate"
  )

  # Case 1: Both CRSs use the same units
  if (input_units == target_units && !is_km_crs) {
    # Direct conversion, just use the same resolution numbers
    result$recommended_cell_size <- input_resolution
    message(sprintf("Same units: Using input resolution: %.6f x %.6f %s",
                    input_resolution[1], input_resolution[2], input_units))

    # Check if target_resolution is finer than input
    if (!is.null(target_resolution)) {
      if (any(target_resolution < input_resolution)) {
        result$is_valid <- FALSE
        result$message <- "Target resolution is finer than input resolution"
      } else {
        result$recommended_cell_size <- target_resolution
      }
    }

    return(result)
  }

  # Case 2: Converting between degrees and meters/feet/km
  if (input_units == "degrees" && (target_units == "m" || target_units == "ft" || is_km_crs)) {

    # Get the centroid in geographic coordinates for conversion
    if (sf::st_crs(input_crs)$epsg != 4326) {
      centroid_geo <- sf::st_transform(
        sf::st_sfc(sf::st_point(centroid), crs = input_crs),
        4326
      )
      centroid_lat <- as.numeric(sf::st_coordinates(centroid_geo)[2])
    } else {
      centroid_lat <- as.numeric(centroid[2])
    }

    # Get meters per degree at this latitude
    mpd <- meters_per_degree(centroid_lat)

    # Convert degrees to meters
    cell_size_m_x <- input_resolution[1] * mpd["longitude"]
    cell_size_m_y <- input_resolution[2] * mpd["latitude"]

    message(sprintf("At latitude %.4f: %.6f degrees = %.2f meters (longitude)",
                    centroid_lat, input_resolution[1], cell_size_m_x))
    message(sprintf("At latitude %.4f: %.6f degrees = %.2f meters (latitude)",
                    centroid_lat, input_resolution[2], cell_size_m_y))

    # Handle different target units
    if (is_km_crs) {
      # Convert to km
      recommended_x <- cell_size_m_x# / 1000
      recommended_y <- cell_size_m_y# / 1000
      message(sprintf("Converting to km: %.2f km x %.2f km", recommended_x, recommended_y))
    } else if (target_units == "m") {
      # Keep as meters
      recommended_x <- cell_size_m_x
      recommended_y <- cell_size_m_y
      message(sprintf("Using meters: %.2f m x %.2f m", recommended_x, recommended_y))
    } else if (target_units == "ft") {
      # Convert to feet
      recommended_x <- cell_size_m_x * 3.28084
      recommended_y <- cell_size_m_y * 3.28084
      message(sprintf("Converting to feet: %.2f ft x %.2f ft", recommended_x, recommended_y))
    }

    result$recommended_cell_size <- c(recommended_x, recommended_y)

    # Validate target_resolution if provided
    if (!is.null(target_resolution)) {
      if (any(target_resolution < result$recommended_cell_size)) {
        result$is_valid <- FALSE
        result$message <- "Target resolution is too fine for the input data"
      } else {
        result$recommended_cell_size <- target_resolution
      }
    }

    return(result)
  }

  # Case 3: Converting from meters/feet/km to degrees
  if ((target_units == "degrees") && (input_units == "m" || input_units == "ft" || is_km_crs)) {
    # Get the centroid in geographic coordinates for conversion
    centroid_geo <- sf::st_transform(
      sf::st_sfc(sf::st_point(centroid), crs = input_crs),
      4326
    )
    centroid_lat <- sf::st_coordinates(centroid_geo)[2]

    # Get meters per degree at this latitude
    mpd <- meters_per_degree(centroid_lat)

    # Convert to meters first if needed
    if (input_units == "ft") {
      cell_size_m_x <- input_resolution[1] * 0.3048
      cell_size_m_y <- input_resolution[2] * 0.3048
    } else if (is_km_crs) {
      cell_size_m_x <- input_resolution[1] * 1000
      cell_size_m_y <- input_resolution[2] * 1000
    } else {
      cell_size_m_x <- input_resolution[1]
      cell_size_m_y <- input_resolution[2]
    }

    # Convert meters to degrees
    recommended_x <- cell_size_m_x / mpd["longitude"]
    recommended_y <- cell_size_m_y / mpd["latitude"]

    message(sprintf("Converting from %s to degrees: %.6f degrees x %.6f degrees",
                    if(is_km_crs) "km" else input_units, recommended_x, recommended_y))

    result$recommended_cell_size <- c(recommended_x, recommended_y)

    # Validate target_resolution if provided
    if (!is.null(target_resolution)) {
      if (any(target_resolution < result$recommended_cell_size)) {
        result$is_valid <- FALSE
        result$message <- "Target resolution is too fine for the input data"
      } else {
        result$recommended_cell_size <- target_resolution
      }
    }

    return(result)
  }

  # Case 4: Converting between meters, feet, and km
  if ((input_units == "m" || input_units == "ft" || is_km_crs) &&
      (target_units == "m" || target_units == "ft" || is_km_crs)) {

    # Convert to meters first as common unit
    if (input_units == "ft") {
      cell_size_m_x <- input_resolution[1] * 0.3048
      cell_size_m_y <- input_resolution[2] * 0.3048
    } else if (is_km_crs && input_units == "m") {
      cell_size_m_x <- input_resolution[1] * 1000
      cell_size_m_y <- input_resolution[2] * 1000
    } else {
      cell_size_m_x <- input_resolution[1]
      cell_size_m_y <- input_resolution[2]
    }

    # Convert from meters to target units
    if (target_units == "ft") {
      recommended_x <- cell_size_m_x * 3.28084
      recommended_y <- cell_size_m_y * 3.28084
    } else if (is_km_crs) {
      recommended_x <- cell_size_m_x / 1000
      recommended_y <- cell_size_m_y / 1000
    } else {
      recommended_x <- cell_size_m_x
      recommended_y <- cell_size_m_y
    }

    message(sprintf("Converting from %s to %s: %.2f x %.2f %s",
                    if(is_km_crs) "km" else input_units,
                    if(is_km_crs) "km" else target_units,
                    recommended_x, recommended_y,
                    if(is_km_crs) "km" else target_units))

    result$recommended_cell_size <- c(recommended_x, recommended_y)

    # Validate target_resolution if provided
    if (!is.null(target_resolution)) {
      if (any(target_resolution < result$recommended_cell_size)) {
        result$is_valid <- FALSE
        result$message <- "Target resolution is too fine for the input data"
      } else {
        result$recommended_cell_size <- target_resolution
      }
    }

    return(result)
  }

  # If we get here, we couldn't determine a good conversion
  result$is_valid <- FALSE
  result$message <- "Unable to convert between these coordinate systems automatically"
  return(result)
}

#' Create a grid based on reprojected data
#'
#' @param data Reprojected spatial data (sf or terra object)
#' @param resolution Cell size in the units of data's CRS c(x, y)
#' @return An sf object with the grid
#'
#' @noRd
#'
create_grid_from_reprojected_data <- function(data, resolution) {

  # Check for unreasonable grid size
  bbox <- sf::st_bbox(data)
  nx <- ceiling((bbox["xmax"] - bbox["xmin"]) / resolution[1])
  ny <- ceiling((bbox["ymax"] - bbox["ymin"]) / resolution[2])

  total_cells <- nx * ny
  if (total_cells > 1e7) {  # Arbitrary limit of 10 million cells
    stop(paste("Grid would create", total_cells, "cells, which is too many. Check your resolution units."))
  }

  # Get the bounding box of the data
  if (inherits(data, "sf")) {
    bbox <- sf::st_bbox(data)
  } else if (inherits(data, "SpatRaster")) {
    ext <- terra::ext(data)
    bbox <- c(
      xmin = ext[1], ymin = ext[3],
      xmax = ext[2], ymax = ext[4]
    )
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  } else {
    stop("Data must be an sf or terra object")
  }

  # Calculate number of cells in each dimension
  nx <- ceiling((bbox["xmax"] - bbox["xmin"]) / resolution[1])
  ny <- ceiling((bbox["ymax"] - bbox["ymin"]) / resolution[2])

  # # Create grid
  # grid <- st_make_grid(
  #   bbox,
  #   cellsize = resolution,
  #   n = c(nx, ny),
  #   crs = st_crs(data)
  # )
  #
  # # Convert to sf data frame
  # grid_sf <- st_sf(id = 1:length(grid), geometry = grid)

  # Make a grid across the cube
  grid <- data %>%
    sf::st_make_grid(cellsize = resolution,
                     offset = c(nx, ny),
                     crs = sf::st_crs(data)) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  return(grid)
}

#' Example workflow function to demonstrate the entire process
#'
#' @param input_data Input spatial data (sf or terra object)
#' @param input_resolution Resolution of input data c(x, y)
#' @param target_crs Target CRS as EPSG code or proj4string
#' @param input_units Units of the input data (degrees or km)
#' @param target_units Units of the output (degrees or km)
#' @param target_resolution Optional target resolution in target CRS units
#' @return A list with the reprojected data and grid
#'
#' @noRd
#'
reproject_and_create_grid <- function(input_data,
                                      input_resolution,
                                      target_crs,
                                      input_units,
                                      target_units,
                                      target_resolution = NULL) {

  # 1. Get input CRS
  input_crs <- sf::st_crs(input_data)

  # 2. Determine appropriate cell size
  cell_size_result <- determine_cell_size(
    input_data,
    input_resolution,
    input_crs,
    target_crs,
    target_resolution,
    input_units,
    target_units
  )

  print(paste("Input resolution:", input_resolution))
  print(paste("Calculated target resolution:", cell_size_result$recommended_cell_size))
 # print(paste("Input CRS units:", input_units))
 # print(paste("Target CRS units:", target_units))

  # 3. Check if the cell size is valid
  if (!cell_size_result$is_valid) {
    warning(cell_size_result$message)
    # You could either stop here or use the recommended cell size
  }

  # 4. Reproject the data to target CRS
  reprojected_data <- sf::st_transform(input_data, target_crs)

  # 5. Create a grid with the appropriate cell size
  grid <- create_grid_from_reprojected_data(
    reprojected_data,
    cell_size_result$recommended_cell_size
  )

  # # 6. Return results
  # return(list(
  #   reprojected_data = reprojected_data,
  #   grid = grid,
  #   cell_size = cell_size_result$recommended_cell_size
  # ))
  return(grid)
}
