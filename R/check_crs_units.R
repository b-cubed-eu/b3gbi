#' Check if a given CRS is in meters or degrees.
#'
#' @param crs_input The CRS, which can be an EPSG code, WKT string, or PROJ.4 string.
#'
#' @return "km" if it's in meters,
#' "degrees" if it's in degrees. Otherwise, throws an informative error.
#'
#' @examples
#' tryCatch({
#'   print(check_crs_units(4326)) # EPSG:4326 (degrees)
#'   print(check_crs_units(25832)) # EPSG:25832 (meters)
#'   print(check_crs_units("+proj=longlat +datum=WGS84 +no_defs"))
#'   print(check_crs_units("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
#'   print(check_crs_units("+proj=somerc +lat_0=0 +lon_0=0 +k=1 +x_0=0 +y_0=0
#'         +ellps=WGS84 +units=km +no_defs")) # Invalid CRS
#'   print(check_crs_units("not a crs")) # Invalid CRS
#'   print(check_crs_units("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96
#'         +x_0=0 +y_0=0 +datum=NAD83 +units=us-ft +no_defs")) # Invalid units
#' }, error = function(e) {
#'   print(e$message)
#' })
#'
check_crs_units <- function(crs_input) {

  # Get CRS info
  crs_info <- st_crs(crs_input)

  # Check if the CRS is valid
  if (is.na(crs_info)) {
    stop(paste("Error: Invalid CRS input. Please provide a valid EPSG code, ",
    "WKT string, or PROJ.4 string."))
  }

  # Check if the CRS is in meters or degrees
  if (crs_info$units_gdal == "degree") {
    return("degrees")
  } else if (crs_info$units_gdal == "metre") {
    return("km")
  } else {
    stop(paste("Error: CRS units are not in degrees or meters. ",
    "Please provide a CRS with valid units."))
  }
}
