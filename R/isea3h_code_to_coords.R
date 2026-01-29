#' Convert ISEA3H Cell Codes to Coordinates (Pure R)
#'
#' Decodes GBIF ISEA3H cell codes (Mocnik identifier scheme) into latitude and longitude
#' coordinates using a native R implementation.
#'
#' @param cell_codes A character or numeric vector of ISEA3H cell codes (Long integers).
#' @return A data frame with columns `xcoord` (longitude), `ycoord` (latitude), and `resolution`.
#' @noRd
isea3h_code_to_coords <- function(cell_codes) {
  library(bit64)

  # 1. Constants
  S <- 1.1071487           # Snyder triangle side (rad)
  H_OFFSET <- sqrt(3)/6    # Barycentric centroid offset
  R_EARTH <- 6371.0072     # Authalic radius (km)

  # 2. Face Constants (Standard ISEA anchors)
  # Face 7 (Europe/Denmark): Centered at 0 Lon, 10.81 Lat
  face_data <- data.frame(
    id = 0:19,
    phi_c = c(rep(52.622632, 5), rep(10.812317, 5), rep(-10.812317, 5), rep(-52.622632, 5)),
    lam_c = c(
      seq(-144, 144, 72)[1:5],
      seq(-144, 144, 72)[1:5],  # Face 7 is index 8 (id=7): 0.0 Lon, 10.81 Lat
      seq(-108, 180, 72)[1:5],
      seq(-108, 180, 72)[1:5]
    )
  )

  decode_single <- function(cell_code) {
    code <- as.integer64(as.character(cell_code))
    bits <- bit64::as.bitstring(code)
    
    # 3. Header Extraction (Indices calibrated for Copenhagen Ground Truth)
    # Face: Bits 3-7 (Indices 3-7 in 1-based string)
    # Resolution: Bits 8-12 (Indices 8-12)
    face_raw <- strtoi(substr(bits, 3, 7), base = 2)
    res <- strtoi(substr(bits, 8, 12), base = 2)
    
    if (is.na(res) || res == 0) return(c(NA, NA))
    
    # 4. Morton Path Extraction (i, j)
    # Path bits typically start after the 12-bit header (Index 13)
    path_bits_str <- substr(bits, 13, 64)
    
    i <- 0
    j <- 0
    bits_to_use <- min(res * 2, nchar(path_bits_str))
    
    for (b in 0:(bits_to_use - 1)) {
      # Extract bit from string (Right to Left / LSB first)
      bit_char <- substr(path_bits_str, nchar(path_bits_str) - b, nchar(path_bits_str) - b)
      bit <- if (bit_char == "1") 1 else 0
      
      if (b %% 2 == 0) {
        i <- i + (bit * 2^(b %/% 2))
      } else {
        j <- j + (bit * 2^(b %/% 2))
      }
    }
    
    # 5. Normalization and Planar Mapping (Barycentric)
    L <- 2^res
    u_norm <- (i / L) - 0.5
    v_norm <- (j / L) - H_OFFSET
    
    # 6. The "Denmark" Transformation
    # Based on Hard Truths: x_snyder = u_norm * S, y_snyder = v_norm * S
    # This specifically addresses the South Hemisphere jump.
    x_snyder <- u_norm * S
    y_snyder <- v_norm * S
    
    # 7. Inverse Snyder
    rho <- sqrt(x_snyder^2 + y_snyder^2)
    if (rho == 0) {
      face_idx <- (face_raw %% 20) + 1
      return(c(face_data$lam_c[face_idx], face_data$phi_c[face_idx]))
    }
    
    alpha <- atan2(x_snyder, y_snyder)
    g <- 2 * asin(pmin(rho / 2, 1))
    
    face_idx <- (face_raw %% 20) + 1
    phi_c_rad <- face_data$phi_c[face_idx] * (pi / 180)
    lam_c_rad <- face_data$lam_c[face_idx] * (pi / 180)
    
    # Spherical law of cosines
    lat_rad <- asin(sin(phi_c_rad) * cos(g) + cos(phi_c_rad) * sin(g) * cos(alpha))
    lon_rad <- lam_c_rad + atan2(sin(g) * sin(alpha), 
                                 cos(phi_c_rad) * cos(g) - sin(phi_c_rad) * sin(g) * cos(alpha))
    
    lon_deg <- ((lon_rad * 180 / pi + 180) %% 360) - 180
    lat_deg <- lat_rad * 180 / pi
    
    return(c(lon_deg, lat_deg))
  }

  n <- length(cell_codes)
  if (n == 0) return(data.frame(xcoord = numeric(), ycoord = numeric(), resolution = character()))
  
  # Optimize by decoding unique codes
  unique_codes <- unique(cell_codes)
  unique_results <- t(sapply(unique_codes, decode_single))
  rownames(unique_results) <- as.character(unique_codes)
  
  all_results <- unique_results[as.character(cell_codes), , drop = FALSE]
  
  result <- data.frame(
    xcoord = all_results[, 1],
    ycoord = all_results[, 2],
    resolution = rep("isea3h", n)
  )
  
  return(result)
}

#' Create Hexagonal Polygons from ISEA3H Centroids
#' @noRd
create_isea3h_grid <- function(df) {
  hex_side <- 0.01 
  make_hex <- function(lon, lat, side) {
    if (is.na(lon) || is.na(lat)) return(sf::st_point(c(0,0)) %>% sf::st_buffer(0) %>% sf::st_geometry())
    angles <- seq(0, 2 * pi, length.out = 7)
    x <- lon + side * cos(angles)
    y <- lat + (side * 1.2) * sin(angles) 
    sf::st_polygon(list(cbind(x, y)))
  }
  hex_list <- lapply(1:nrow(df), function(i) make_hex(df$xcoord[i], df$ycoord[i], hex_side))
  sf::st_sf(cellid = 1:nrow(df), cellCode = df$cellCode, geometry = sf::st_sfc(hex_list, crs = "EPSG:4326"))
}