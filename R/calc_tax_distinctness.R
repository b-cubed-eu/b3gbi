#' Calculate Taxonomic Distinctness
#'
#' Calculates taxonomic distinctness, a measure of the evolutionary
#' relationships among species in a community. This function requires a
#' taxonomic hierarchy.
#'
#' @param x A data frame with a column named 'scientificName' containing
#'   species names.
#' @param y A taxonomic hierarchy obtained using the classification function
#'   from the taxize package.
#' @return A numeric value representing the average taxonomic distinctness.
#'   Returns `NA` if there are fewer than three species for calculation.
#' @examples
#' # Choose some species names
#' species_names <- data.frame(
#'   scientificName = c("Felis catus", "Canis lupus", "Panthera leo"))
#' # Get taxonomic hierarchy from GBIF
#' tax_hierarchy <- taxize::classification(unique(species_names$scientificName),
#'   db = "gbif", return_id = TRUE, accepted = TRUE)
#' # Calculate taxonomic distinctness
#' tax_distinctness <- calc_tax_distinctness(species_data, tax_hierarchy)
#'
#' @noRd
calc_tax_distinctness <- function(x, y) {
  temp <- names(y) %in% x$scientificName
  tax_hier_temp <- y[c(temp)]
  print(length(tax_hier_temp))
  n_spec <- length(tax_hier_temp)
  if(length(tax_hier_temp) < 3) {
    tax_distinct <- NA
    return(tax_distinct)
  } else {
    tax_tree <- taxize::class2tree(tax_hier_temp, check=FALSE)
    tax_distance <- tax_tree$distmat
    tax_distinct <- sum(tax_distance) / ((n_spec * (n_spec - 1)) / 2)
    return(tax_distinct)
  }
}
