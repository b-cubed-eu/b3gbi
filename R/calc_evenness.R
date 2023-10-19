# Calculate evenness from occurrence data frame, with species as columns and
# sites as rows.

calc_evenness <- function(x) {
  n <- colSums(x)
  N <- sum(n)
  p <- n / N
  p_squared <- p^2
  summed <- sum(p_squared)
  complement <- 1 - summed
  even <- complement * (N / (N - 1))
  if (is.nan(even)) {
    even <- NA
  }
  return(even)
}
