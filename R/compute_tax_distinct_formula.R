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

    if (requireNamespace("taxize", quietly = TRUE)) {

      tax_tree <- taxize::class2tree(tax_hier_temp, check = FALSE)
      tax_distance <- tax_tree$distmat
      tax_distinct <- sum(tax_distance) / ((n_spec * (n_spec - 1)) / 2)

      return(tax_distinct)

    } else {

      stop(
        "The taxize package is required to calculate taxonomic distinctness."
      )

    }
  }
}
