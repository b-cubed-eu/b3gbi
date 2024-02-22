#' @export
aggregatevirtualcube <- function(dt) {

  cat("\nAggregating occurrences in grid cells.\n")

  tic("Total time")

  dt[, n := .N, by = .(year, taxonKey, eea_cell_code)]
  dt <- unique(dt, by = c("year", "taxonKey", "eea_cell_code"))
  dt[, obs:=NULL]
  setnames(dt, old = "n", new = "obs")

  #  cat("\nTransforming to sf object.")

  # Set projection and then transform to EPSG:3035
  #  sp::coordinates(dt) <- ~xcoord+ycoord
  #  sp::proj4string(dt) <- CRS("epsg:3035")

  #  setDT(dt@data)

  toc()

  return(dt)
}
