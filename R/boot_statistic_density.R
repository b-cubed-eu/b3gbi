# boot statistic function to calculate occurrence density
#' @noRd
boot_statistic_density <- function(data, indices) {
  # 1. Validate indices against the number of ROWS
  if (any(indices > nrow(data)) || any(indices < 1)) {
    stop("Indices contain out-of-bounds values.")
  }

  # 2. Resample the data (resampling full rows to keep 'obs' linked to cells)
  d <- data[indices, ]
  # 3. Calculate the indicator: sum of resampled observations / total area
  total_resampled_obs <- sum(d$obs)

  # The total area is constant across all rows for a given year (since it was added)
  # We just need to grab the value from any row, using the first row is standard.
  constant_area <- d$total_area[1]

  # Note: No rounding/casting necessary here, as this is a density calculation
  return(total_resampled_obs / constant_area)
}
