# boot statistic function to calculate evenness
#' @noRd
boot_statistic_evenness <- function(data, indices, type) {
  if (any(indices > length(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }
  d <- data[indices]
  return(compute_evenness_formula(d, type))
}
