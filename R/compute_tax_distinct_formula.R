#'
#' @noRd
compute_tax_distinct_formula <- function(x, y) {

  temp <- names(y) %in% x$scientificName
  tax_hier_temp <- y[c(temp)]
  print(length(tax_hier_temp))
  n_spec <- length(tax_hier_temp)

  if (n_spec < 3) {

    tax_distinct <- NA
    return(tax_distinct)

  } else {

      # 1. Get the number of species
      n_spec <- length(tax_hier_temp)

      # 2. Get the maximum number of ranks
      L <- max(sapply(tax_hier_temp, nrow))

      # 3. Get all unique pairs of species
      species_names <- names(tax_hier_temp)
      species_pairs <- utils::combn(species_names, 2, simplify = FALSE)

      # 4. Calculate the sum of pairwise distances (numerator of TDI)
      sum_of_distances <- 0

      for (pair in species_pairs) {
        # Get the hierarchy data frames for the two species
        hier1 <- tax_hier_temp[[pair[1]]]
        hier2 <- tax_hier_temp[[pair[2]]]

        # Find the number of shared ranks (up to the highest common ancestor)
        shared_ranks <- sum(hier1$name %in% hier2$name)

        # The distance is the number of unshared ranks
        distance <- L - shared_ranks

        # Add this distance to the total sum
        sum_of_distances <- sum_of_distances + distance
      }

      # 5. Calculate the denominator for the TDI
      denominator <- L * (n_spec * (n_spec - 1) / 2)

      # 6. Calculate the final TDI
      tax_tdi <- sum_of_distances / denominator

      return(tax_tdi)

  }
}
