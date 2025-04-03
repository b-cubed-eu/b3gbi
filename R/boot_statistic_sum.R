# boot statistic function to sum the data
#' @noRd
boot_statistic_sum <- function(data, indices) {
  if (any(indices > length(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }
  d <- data[indices]
  return(as.numeric(sum(d, na.rm = TRUE)))
}
