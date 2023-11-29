# Calculate evenness from a vector of summed species occurrences.

calc_pielou_evenness <- function(x) {
  S <- length(x)
  n <- x
  N <- sum(n)
  p <- n / N
  p_squared <- p^2
  summed <- sum(p_squared)
  rooted <- sqrt(summed)
  complement <- 1 - rooted
  even <- complement / (1 - sqrt(1 / S))
  if (is.nan(even)) {
    even <- NA
  }
  return(even)
}

calc_e9_evenness <- function(x) {
  n <- x
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
