#' @noRd
compute_evenness_formula <- function(x, type) {

  type <- match.arg(type, names(available_indicators))

  if (type == "pielou_evenness") {

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

  } else if (type == "williams_evenness") {

    n <- x
    N <- sum(n)
    S <- sum(n > 0)
    p <- n / N
    p_squared <- p^2
    summed <- sum(p_squared) * S
    adjusted <- (summed - 1) / (S - 1)
    root <- adjusted^(1 / 2)
    even <- 1 - root
    if (is.nan(even)) {
      even <- NA
    }

    return(even)

  }
}

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
