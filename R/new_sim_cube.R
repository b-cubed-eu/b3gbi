#' @title 'sim_cube' S3 Constructor
#'
#' @description This function constructs a 'sim_cube' S3 object, a specialized
#' data structure designed for analysis of simulated biodiversity data (e.g.
#' from the gcube package) that lacks spatial information. It validates the
#' input data cube, calculates essential summary information, and prepares the
#' object for further use.
#'
#' @param x A tibble data cube containing simulated occurrence data.
#'
#' @return  A 'gcube' S3 object containing:
#'   * **Summary statistics:** First and last year of data, number of species,
#'     number of observations.
#'   * **Original data:** The input data cube.
#'
#' @noRd
new_sim_cube <- function(x, grid_type) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "taxonKey",
                  "obs") %in% names(x)))
  structure(list(
    first_year = min(x$year),
    last_year = max(x$year),
    coord_range = ifelse(
      ("xcoord" %in% colnames(x) & "ycoord" %in% colnames(x)),
      list(c("xmin" = min(x$xcoord),
             "xmax" = max(x$xcoord),
             "ymin" = min(x$ycoord),
             "ymax" = max(x$ycoord))),
      "Coordinates not provided"
    ),
    num_cells = ifelse(
      "cellCode" %in% colnames(x),
      length(unique(x$cellCode)),
      "No cell codes provided"
    ),
    num_species = length(unique(x$taxonKey)),
    num_obs = sum(x$obs),
    kingdoms = ifelse(
      "kingdom" %in% colnames(x),
      c(unique(x$kingdom)),
      "Data not present"
    ),
    num_families = ifelse(
      "family" %in% colnames(x),
      length(unique(x$family)),
      "Data not present"
    ),
    grid_type = grid_type,
    resolutions = ifelse(
      "resolution" %in% colnames(x),
      unique(x$resolution),
      NA
    ),
    data = x
  ),
  class = "sim_cube")
}
