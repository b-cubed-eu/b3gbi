#' @export
get_observed_years <- function(x) {
  stopifnot(inherits(x, "indicator_map"))
  stopifnot("years_with_obs" %in% names(x))
  obs <- x$years_with_obs
  years <- min(x$years_with_obs):max(x$years_with_obs)
  observed <- ifelse(years %in% obs, TRUE, FALSE)
  data.frame("years" = years,
             "occurrences" = observed)

}
