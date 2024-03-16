compute_formula <- function(x, ...) {
  UseMethod("compute_formula")
}


#' Calculate Pielou's Evenness Index
#'
#' Calculates Pielou's evenness index, a measure of how evenly species
#' are distributed in a community, from a vector of species abundances.
#'
#' @param x A numeric vector containing the total occurrences for each species.
#' @return A numeric value representing Pielou's evenness index. Values range
#'   between 0 (no evenness) and 1 (complete evenness). Returns `NA`
#'   if the calculation results in a `NaN`.
#' @examples
#' species_counts <- c(20, 55, 12, 13)
#' my_evenness <- compute_formula.pielou_evenness(species_counts)
#' my_evenness
#' @noRd
compute_formula.pielou_evenness <- function(x) {
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


#' Calculate Williams' Evenness Index
#'
#' Calculates Williams' evenness index, a measure of how evenly species are
#' distributed within a community, from a vector of species abundances.
#'
#' @param x A numeric vector containing the total occurrences of each species.
#' @return A numeric value representing Williams' evenness.  The interpretation of
#'   values is similar to Pielou's evenness: 0 indicates no evenness, and 1
#'   indicates complete evenness. Returns `NA` if the calculation results in a `NaN`.
#' @examples
#' species_counts <- c(15, 48, 27)
#' my_evenness <- compute_formula.williams_evenness(species_counts)
#' my_evenness
#' @noRd
compute_formula.williams_evenness <- function(x) {
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
