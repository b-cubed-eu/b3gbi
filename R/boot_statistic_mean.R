# boot statistic function to take the mean
#' @noRd
boot_statistic_mean <- function(data, indices) {
  d <- data[indices]
  return(as.numeric(mean(d)))
}
