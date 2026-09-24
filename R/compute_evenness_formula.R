#' @noRd
compute_evenness_formula <- function(x, type) {

  type <- match.arg(type, names(available_indicators))

  # Only species actually observed in this cell/year count towards S.
  # (Absent species are passed in as zeros and must not inflate S.)
  n <- x[!is.na(x) & x > 0]
  S <- length(n)
  # Evenness is undefined with fewer than two species
  if (S < 2) return(NA)
  N <- sum(n)
  p <- n / N


  if (type == "pielou_evenness") {

    H_prime <- -sum(p * log(p))
    even <- H_prime / log(S)
    if (is.nan(even)) {
      even <- NA
    }

    return(even)

  } else if (type == "williams_evenness") {

    p_squared <- p^2
    summed <- S * sum(p_squared) - 1
    adjusted <- summed / (S - 1)
    root <- adjusted^(1 / 2)
    even <- 1 - root
    if (is.nan(even)) {
      even <- NA
    }

    return(even)

  }
}
