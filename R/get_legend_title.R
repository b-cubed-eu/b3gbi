#' Retrieve Legend Title for Biodiversity Indicator
#'
#' Provides an appropriate legend title for a biodiversity indicator,
#' suitable for use with time series plots ("ts") or maps.
#'
#' @param x A character string representing the biodiversity indicator code
#'   (e.g., "obs_richness", "hill1", "tax_distinct").
#' @param type Specifies the visualization type: "ts" (time series) or "map".
#' @return A character string containing the legend title. Throws an error if
#'   the specified `div_type` is not found.
#'
#' @examples
#' # Get legend title for "tax_distinct" on a map
#' map_legend <- get_legend_title("tax_distinct", type = "map")
#'
#' # Get legend title for "hill2" on a time series plot
#' ts_legend <- get_legend_title("hill2", type = "ts")
#' @noRd
get_legend_title <- function(x, type) {

  type <- match.arg(type, c("ts", "map"))

  leg_df <- matrix(c("obs_richness", "Richness", "Richness",
                     "cum_richness", "Cumulative Richness", NA,
                     "total_occ", "Occurrences", "Occurrences",
                     "occ_by_type", "Occurrences", "Occurrences",
                     "occ_by_dataset", "Occurrences", "Occurrences",
                     "rarefied", "Estimated Richness", "Richness",
                     "hill0", "Estimated Richness", "Richness",
                     "hill1", "Shannon-Hill Diversity", "Shannon-Hill \nDiversity",
                     "hill2", "Simpson-Hill Diversity", "Simpson-Hill \nDiversity",
                     "e9_evenness", "Evenness", "Evenness",
                     "pielou_evenness", "Evenness", "Evenness",
                     "species_rarity", "Rarity", NA,
                     "newness", NA, "Mean Year of \nOccurrence",
                     "density", "Occurrences per km^2", "Occurrences \nper km^2",
                     "ab_rarity", "Rarity", "Rarity",
                     "area_rarity", "Rarity", "Rarity",
                     "spec_occ", "Occurrences", "Occurrences",
                     "tax_distinct", "Taxonomic Distinctness", "Taxonomic \nDistinctness"
  ),
  ncol = 3,
  byrow = TRUE) %>%
    as.data.frame

  names(leg_df) <- c("div_type", "axis_title_ts", "leg_title_map")

  if (type == "ts") {

    leg_title <- leg_df$axis_title_ts[leg_df$div_type %in% x]

  } else if (type == "map") {

    leg_title <- leg_df$leg_title_map[leg_df$div_type %in% x]

  }

  if (!length(leg_title) > 0) stop("Div_type not found")

  return(leg_title)

}
