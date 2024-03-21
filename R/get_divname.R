#' Retrieve Biodiversity Indicator Name
#'
#' Returns the full name (human-readable) of a biodiversity indicator, provided
#' its corresponding code.
#'
#' @param x A character string representing the biodiversity indicator code
#'   (e.g., "obs_richness", "hill1", "tax_distinct").
#' @return A character string containing the full descriptive name of the
#'   biodiversity indicator. If no match is found, returns an error.
#'
#' @examples
#' # Get the name for the code "obs_richness"
#' indicator_name <- get_divname("obs_richness")
#' indicator_name  # Output: "Observed Species Richness"
#' @noRd
get_divname <- function(x) {

  iname <- as.character(available_indicators[[x]]$indicator_name)

  # div_df <- matrix(c("obs_richness", "Observed Species Richness",
  #                    "cum_richness", "Cumulative Species Richness",
  #                    "total_occ", "Total Occurrences",
  #                    "occ_by_type", "Total Occurrences Segrerated by Type",
  #                    "occ_by_dataset", "Total Occurrences Segregated by Dataset",
  #                    "rarefied", "Estimated Species Richness",
  #                    "hill0", "Estimated Species Richness",
  #                    "hill1", "Estimated Shannon-Hill Diversity",
  #                    "hill2", "Estimated Simpson-Hill Diversity",
  #                    "e9_evenness", "E9 Evenness",
  #                    "pielou_evenness", "Pielou's Evenness",
  #                    "species_rarity", "Species Rarity",
  #                    "newness", "Mean Year of Occurrences",
  #                    "density", "Density of Occurrences",
  #                    "ab_rarity", "Abundance-Based Rarity",
  #                    "area_rarity", "Area-Based Rarity",
  #                    "spec_occ", "Species Occurrences",
  #                    "spec_range", "Species Ranges",
  #                    "tax_distinct", "Taxonomic Distinctness"
  #                 ),
  #                 ncol = 2,
  #                 byrow = TRUE) %>%
  #   as.data.frame

  # names(div_df) <- c("div_type", "div_name")
  #
  # div_name <- div_df$div_name[div_df$div_type %in% x]
  #
  # if(!length(div_name) > 0) stop("div_type not found")

  return(iname)

}
