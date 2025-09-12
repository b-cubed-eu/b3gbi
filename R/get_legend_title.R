#' Retrieve Legend Title for Biodiversity Indicator
#'
#' Provides an appropriate legend title for a biodiversity indicator,
#' suitable for use with time series plots ("ts") or maps.
#'
#' @param x A character string representing the biodiversity indicator class
#'  (e.g., "obs_richness", "hill1", "cum_richness").
#' @return A character string containing the legend title. Throws an error if
#'  the specified indicator class is not found.
#' @noRd
get_legend_title <- function(x) {

  ltitle <- as.character(available_indicators[[x]]$legend_label)

  if (!length(ltitle) > 0) {

    stop(
      "Indicator class is not registered. Check that you typed it correctly."
    )

  } else {

    ltitle

  }

  return(ltitle)

}
