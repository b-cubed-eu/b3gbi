# Function to replace NA values to avoid errors when calculating confidence intervals
# Use lapply over a list returned by the boot::boot function (or boot.out)
#' @noRd
ci_error_prevent <- function(x) {

  if (all(is.na(x$t))) {
    # when all values are NA, replace with 1.2 (the actual value is not
    # important as it will be ignored). No confidence intervals will be
    # calculated but this avoids an error.
    x$t <- matrix(rep(1.2, length(x$t)))
    x$t0 <- 1.2
    return(x)
  } else if (any(is.na(x$t))){
    # when any values are NA, replace with the mean of the other values
    # this avoids an error without affecting the CI calculation
    x$t[is.na(x$t)] <- mean(x$t, na.rm = TRUE)
    return(x)
  } else {
    return(x)
  }
}
