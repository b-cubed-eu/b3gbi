# boot statistic function for taxonomic distinctness
#' @noRd
boot_statistic_td <- function(data, indices) {

  if (any(indices > length(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }

  tax_hier <- my_readRDS("taxonomic_hierarchy.RDS")

  df <- data.frame(index = indices, scientificName = data[indices])

  diversity_val <- compute_tax_distinct_formula(df, tax_hier)

  return(diversity_val)

}
