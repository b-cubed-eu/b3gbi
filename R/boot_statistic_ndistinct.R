# boot statistic function to sum the data
#' @noRd
boot_statistic_ndistinct <- function(data, indices) {
  d <- data[indices]
  return(as.numeric(dplyr::n_distinct(d)))
}
