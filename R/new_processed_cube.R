#' @title 'processed_cube' S3 Constructor
#'
#' @description This function constructs a 'processed_cube' S3 object, a
#' specialized data structure designed for biodiversity analysis within this
#' package. It validates the input data cube, calculates essential summary
#' information, and prepares the object for further use.
#'
#' @param x A tibble data cube containing occurrence data.
#'
#' @return  A 'processed_cube' S3 object containing:
#'   * **Summary statistics:** First and last year of data, coordinate range,
#'     number of spatial cells, species, and observations.
#'   * **Diversity information:** Kingdoms represented, resolutions, indication
#'    of multiple resolutions and datasets, and types of occurrence records.
#'   * **Original data:** The input data cube.
#'
#' @note 'processed_cube' objects are used by various analysis functions within
#'  this package.
#'
#' @noRd
new_processed_cube <- function(x, grid_type) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "cellCode",
                  "taxonKey",
                  "obs",
                  "scientificName",
                  "xcoord",
                  "ycoord",
                  "resolution") %in% names(x)))
  res_num <- as.numeric(stringr::str_extract(x$resolution[1],
                                             "^[+-]?\\d*\\.?\\d+"))
  if (grid_type == "eqdgc") {
    coord_range = list("xmin" = min(x$xcoord) - (res_num / 2),
                       "xmax" = max(x$xcoord) + (res_num / 2),
                       "ymin" = min(x$ycoord) - (res_num / 2),
                       "ymax" = max(x$ycoord) + (res_num / 2))
  } else if (grid_type == "eea") {
    coord_range = list("xmin" = min(x$xcoord),
                       "xmax" = max(x$xcoord) + res_num,
                       "ymin" = min(x$ycoord),
                       "ymax" = max(x$ycoord) + res_num)
  } else {
    coord_range = list("xmin" = min(x$xcoord),
                       "xmax" = max(x$xcoord),
                       "ymin" = min(x$ycoord),
                       "ymax" = max(x$ycoord))
  }
  if (all(c("datasetKey", "basisOfRecord") %in% names(x))) {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = coord_range,
                   num_cells = length(unique(x$cellCode)),
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
                   resolutions = unique(x$resolution),
                   num_datasets = length(unique(x$datasetKey)),
                   record_types = unique(x$basisOfRecord),
                   data = x),
              class = "processed_cube_dsinfo")
  } else {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = coord_range,
                   num_cells = length(unique(x$cellCode)),
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
                   resolutions = unique(x$resolution),
                   data = x),
              class = "processed_cube")
  }
}
