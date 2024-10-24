# boot statistic function to calculate evenness
#' @noRd
boot_statistic_evenness <- function(data, indices, type) {
  d <- data[indices]
  return(compute_evenness_formula(d, type))
}
