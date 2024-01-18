# Retrieves the name of the biodiversity indicator associated with a diversity type
#' @noRd
get_divname <- function(x) {

  div_df <- matrix(c("obs_richness", "Observed Species Richness",
                     "cum_richness", "Cumulative Species Richness",
                     "total_occ", "Total Occurrences",
                     "occ_by_type", "Total Occurrences Segrerated by Type",
                     "occ_by_dataset", "Total Occurrences Segregated by Dataset",
                     "rarefied", "Estimated Species Richness",
                     "hill0", "Estimated Species Richness",
                     "hill1", "Estimated Shannon-Hill Diversity",
                     "hill2", "Estimated Simpson-Hill Diversity",
                     "e9_evenness", "E9 Evenness",
                     "pielou_evenness", "Pielou's Evenness",
                     "species_rarity", "Species Rarity",
                     "newness", "Mean Year of Occurrences",
                     "density", "Density of Occurrences",
                     "ab_rarity", "Abundance-Based Rarity",
                     "area_rarity", "Area-Based Rarity",
                     "spec_occ", "Species Occurrences",
                     "spec_range", "Species Ranges",
                     "tax_distinct", "Taxonomic Distinctness"
                  ),
                  ncol = 2,
                  byrow = TRUE) %>%
    as.data.frame

  names(div_df) <- c("div_type", "div_name")

  div_name <- div_df$div_name[div_df$div_type %in% x]

  return(div_name)

}
