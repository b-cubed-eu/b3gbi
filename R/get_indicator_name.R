#' Retrieve Biodiversity Indicator Name
#'
#' Returns the full name (human-readable) of a biodiversity indicator, provided
#' its corresponding code.
#'
#' @param x A character string representing the biodiversity indicator class
#'  (e.g., "obs_richness", "hill1", "cum_richness").
#' @return A character string containing the full descriptive name of the
#'  biodiversity indicator. Returns an error if the specified indicator class
#'  is not found.
#' @noRd
get_indicator_name <- function(x) {

  iname <- as.character(available_indicators[[x]]$indicator_name)

  if (!length(iname) > 0) {

    stop("Indicator class is not registered. Check that you typed it correctly")

  } else {

    iname

  }

  return(iname)

}
