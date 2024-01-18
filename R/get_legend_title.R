# Retrieves the legend title for the biodiversity indicator associated with a diversity type
#' @noRd
get_legend_title <- function(x, type) {

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

  return(leg_title)

}
