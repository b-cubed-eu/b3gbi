#' Check if a given CRS is in meters or degrees.
#'
#' @param crs_input The CRS, which can be an EPSG code, WKT string, or
#'  PROJ.4 string.
#'
#' @return "km" if it's in meters,
#' "degrees" if it's in degrees. Otherwise, throws an informative error.
#'
#' @keywords internal
#'
#' @examples
#' tryCatch({
#'   print(b3gbi:::check_crs_units(4326)) # EPSG:4326 (degrees)
#'   print(b3gbi:::check_crs_units(25832)) # EPSG:25832 (meters)
#'   print(b3gbi:::check_crs_units("+proj=longlat +datum=WGS84 +no_defs"))
#'   print(b3gbi:::check_crs_units(
#'     "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
#'   # Valid CRS, but rejected because its units are not metres or degrees
#'   print(b3gbi:::check_crs_units("+proj=somerc +lat_0=0 +lon_0=0 +k=1 +x_0=0
#'         +y_0=0 +ellps=WGS84 +units=km +no_defs"))
#' }, error = function(e) {
#'   print(e$message)
#' })
#'
#' # Invalid CRS
#' try(b3gbi:::check_crs_units("not a crs"))
#'
#' # Invalid units (US survey feet)
#' try(b3gbi:::check_crs_units("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39
#'   +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"))
#'
check_crs_units <- function(crs_input) {

  # Use tryCatch to gracefully handle errors from st_crs
  crs_info <- tryCatch(
    suppressWarnings(sf::st_crs(crs_input)),
    error = function(e) {
      stop(paste0("Error: Invalid output_crs: ", crs_input, ". Please provide ",
      "a valid EPSG code, WKT string, or PROJ.4 string."))
    }
  )

  # Check if the returned CRS object is valid
  if (is.na(crs_info$proj4string)) {
    stop(paste0("Error: Invalid output_crs: ", crs_input, ". Please provide ",
    "a valid EPSG code, WKT string, or PROJ.4 string."))
  }

  # Check if the CRS is in meters or degrees
  if (crs_info$units_gdal == "degree") {
    return("degrees")
  } else if (crs_info$units_gdal == "metre") {
    return("km")
  } else {
    stop(paste0("Error: CRS units are not in degrees or meters. ",
                "Please provide a CRS with valid units."))
  }
}
