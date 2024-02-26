#' @noRd
# Define function to calculate taxonomic distinctness
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
