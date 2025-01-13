# boot statistic function to sum the data
#' @noRd
boot_statistic_sum <- function(data, indices) {
  d <- data[indices]
  return(as.numeric(sum(d)))
}
