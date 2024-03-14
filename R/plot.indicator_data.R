plot.indicator_data <- function(x, ...) {
  warning("Plotting directly the data frame of an indicator_map or indicator_ts
          object ignores metadata and may not give the intended output.
          Consider plotting the indicator_map or indicator_ts object instead.")
  NextMethod()
}
