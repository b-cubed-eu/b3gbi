#' Calculate the taxonomic distinctness index for one grid cell or year
#'
#' Average taxonomic distance between all pairs of taxa present, divided by the
#' number of taxonomic levels L (Clarke & Warwick 1999, presence-absence form):
#' TDI = sum_{i<j} d_ij / (L * S * (S - 1) / 2), where d_ij is the number of
#' levels below the lowest rank shared by taxa i and j. L is fixed at the seven
#' main ranks (kingdom to species) so that values are comparable between cells
#' and years.
#'
#' @param x Data frame of occurrences in one cell or year, with a `taxonKey`
#'   column.
#' @param tax_hier Taxonomic hierarchy as returned by
#'   `get_taxonomic_hierarchy()`.
#'
#' @return A single numeric value between 0 and 1, or `NA` if fewer than three
#'   classified taxa are present.
#' @noRd
compute_tax_distinct_formula <- function(x, tax_hier) {

  keys <- unique(as.character(x$taxonKey))
  ids <- tax_hier[match(keys, tax_hier$taxonKey), tax_ranks, drop = FALSE]

  # Exclude taxa without a classification
  ids <- ids[!is.na(ids$kingdom), , drop = FALSE]
  n_spec <- nrow(ids)

  if (n_spec < 3) {
    return(NA_real_)
  }

  L <- length(tax_ranks)
  d <- tax_distance_matrix(ids)

  sum(d[upper.tri(d)]) / (L * n_spec * (n_spec - 1) / 2)
}
