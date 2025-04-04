# boot statistic function to take the mean
#' @noRd
boot_statistic_mean <- function(data, indices) {
  if (any(indices > length(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }
  d <- data[indices]
  return(as.numeric(mean(d, na.rm = TRUE)))
}
