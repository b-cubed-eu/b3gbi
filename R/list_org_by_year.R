# Put individual observations into a list organized by year
# col should be a column name in quotes, e.g. "obs"
#' @noRd
list_org_by_year <- function(df, col) {

  # create empty list
  a <- list()
  # set counter j to 1
  j <- 1
  # iterate over unique years
  for (i in unique(df$year)) {
    # add observations from year i to list
    a[[j]] <- df[[col]][df$year == i]
    # iterate counter
    j <- j + 1
  }
  # name list elements
  names(a) <- unique(df$year)

  return(a)
}
