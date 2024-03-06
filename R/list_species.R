#' Extract Species Names.
#'
#' Retrieves a list of species names from a biodiversity data object
#'   ('processed_cube', 'indicator_map' or 'indicator_ts').
#'
#' @param object A biodiversity data object containing species names, either as
#'   a separate vector called species_names or as a column called scientificName.
#' @return  Either a character vector of species names (if directly available)
#'   or a data frame with columns:
#'   * `taxonKey`: A unique identifier for each species.
#'   * `scientificName`:  The scientific name of each species.
#'
#' @examples
#' # Assuming you have a biodiversity data cube named 'mammals_cube'
#' species_list <- list_species(mammals_cube)
#' @export
list_species <- function(object) {

  if(length(object$species_names) > 0) {

    return(object$species_names)

  } else {

    species_df <-
      object$data %>%
      dplyr::select(taxonKey, scientificName) %>%
      dplyr::distinct(taxonKey, scientificName)

    return(species_df)

  }
}
