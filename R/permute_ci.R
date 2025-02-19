# Calculate confidence intervals by permutation
#' @noRd
permute_ci <- function(x, num_bootstrap, cumsum = FALSE) {

  n <- nrow(x)
  n_seq <- seq_len(n)
  val_sample <- replicate(num_bootstrap,
                              resample(x$unique_by_year, n, replace = TRUE))

  if (cumsum==TRUE) {

    index <- t(apply(val_sample, 2, cumsum))

  } else {

    index <- t(val_sample)

  }

  # index_df <- do.call(rbind, index)
  lower.ci <- apply(index, 2, stats::quantile, 0.05)
  upper.ci <- apply(index, 2, stats::quantile, 0.95)
  df <- data.frame(year = x$year, ll = lower.ci, ul = upper.ci)

}
