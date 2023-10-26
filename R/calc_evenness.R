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

calc_e9_evenness <- function(x) {
  n <- colSums(x)
  N <- sum(n)
  S <- sum(n > 0)
  p <- n / N
  p_squared <- p^2
  summed <- sum(p_squared) * S
  adjusted <- (summed - 1) / (S - 1)
  root <- adjusted^(1/2)
  even <- 1 - root
  if (is.nan(even)) {
    even <- NA
  }
  return(even)
}
