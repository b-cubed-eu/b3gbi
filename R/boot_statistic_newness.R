# boot statistic function to calculate newness
#' @noRd
boot_statistic_newness <- function(data, indices) {
  d <- data[indices]
  return(as.numeric(round(mean(d))))
}
