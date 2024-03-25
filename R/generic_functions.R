#' @title Calculate a Biodiversity Indicator Time Series
#'
#' @description A generic function for calculating biodiversity indicator
#'   time series data. Specific implementations for different indicator types
#'   or calculation methods should be provided using S3 methods.
#'
#' @param data An object containing the necessary data and information for the
#'   time series calculation.
#' @param ... Additional arguments (potentially used by specific methods).
#'
#' @return An object representing the calculated biodiversity indicator time
#'   series. The format of the output will depend on the specific S3 method used.
#'
#' @method calc_ts default
#' @usage calc_ts(data)
#'
#' @export
calc_ts <- function(x, ...) {
  UseMethod("calc_ts")
}

#' @title Calculate a Biodiversity Indicator Map
#'
#' @description A generic function for calculating spatial biodiversity
#'   indicator maps. Specific implementations for different indicator types
#'   or mapping approaches should be provided using S3 methods.
#'
#' @param data An object containing the geospatial data and other information
#'   required for the map calculation.
#' @param ... Additional arguments (potentially used by specific methods).
#'
#' @return An object representing the calculated biodiversity indicator map.
#'   The format of the output will depend on the specific S3 method used.
#'
#' @method calc_map default
#' @usage calc_map(data)
#'
#' @export
calc_map <- function(x, ...) {
  UseMethod("calc_map")
}
