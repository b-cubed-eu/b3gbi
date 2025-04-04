# boot statistic function to calculate newness
#' @noRd
boot_statistic_newness <- function(data, indices) {
  if (any(indices > length(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }
  d <- data[indices]
  return(as.numeric(round(mean(d, na.rm = TRUE))))
}
