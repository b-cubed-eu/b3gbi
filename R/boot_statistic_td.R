# boot statistic function for taxonomic distinctness
#' @noRd
boot_statistic_td <- function(data, indices) {

  tax_hier <- readRDS("taxonomic_hierarchy.RDS")

  df <- data.frame(index = indices, scientificName = data[indices])

  diversity_val = compute_tax_distinct_formula(df, tax_hier)

}
