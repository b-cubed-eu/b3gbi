# Function to create virtual cube ----
#' @export
createvirtualcube <- function(vslist) {

  cat("\nAssigning names and taxon keys to species.")

  tic("Total time")

  counter <- 0
  vslist <- lapply(vslist, function (x) {
    counter <<- counter + 1
    setDT(x)
    x[, taxonKey:=..counter][, scientificName:=paste("Species ", ..counter, sep="")]
    return(x)})

  cat("\nConverting list to data table.")

  dt <- data.table::rbindlist(vslist)

  cat("\nAdding additional columns to data table.")

  names(dt) <- c("xcoord", "ycoord", "obs", "taxonKey", "scientificName")

  dt[,year:=as.numeric("2020")
  ][,resolution:="1km"
  ][, n:="N"
  ][, e:="E"]

  cat("\nAssigning occurrences to grid cells.\n")

  dt[,xcoord := floor(xcoord / 1000)][, ycoord := floor(ycoord / 1000)]

  cc <- c("resolution", "e", "xcoord", "n", "ycoord")

  tic("Time to create grid cells")
  setkey(dt, "resolution", "e", "xcoord", "n", "ycoord")
  dtunique <- unique(dt[,cc, with = FALSE], by = key(dt))
  dtunique[, eea_cell_code := do.call(paste, c(dtunique, sep = ""))]
  dt[dtunique, eea_cell_code := eea_cell_code, on = cc]
  dt[, c("resolution", "n","e"):=NULL]
  toc()

  toc()

  return(dt)

}
