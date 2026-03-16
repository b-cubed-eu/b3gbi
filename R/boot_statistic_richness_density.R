# boot statistic function to calculate species richness density
#' @noRd
boot_statistic_richness_density <- function(data, indices) {
  # 1. Validate indices against the number of ROWS
  if (any(indices > nrow(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }

  # 2. Resample the data
  d <- data[indices, ]

  # 3. Calculate the indicator: distinct species richness / total area
  total_resampled_richness <- length(unique(d$taxonKey))

  # The total area is constant across all rows for a given year (since it was added)
  # We just need to grab the value from any row, using the first row is standard.
  constant_area <- d$total_area[1]

  return(total_resampled_richness / constant_area)
}
