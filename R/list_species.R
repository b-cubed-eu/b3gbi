#' Extract Species Names.
#'
#' Retrieves a standardized list of species names from a biodiversity data object
#' ('processed_cube', 'indicator_map' or 'indicator_ts').
#'
#' @param object A biodiversity data object containing species names (either in
#'  a \code{data} slot or a \code{species_names} vector).
#' @return A tibble with a single column:
#'  * `scientificName`: The unique scientific names found in the object.
#'
#' @examples
#' list_species(example_cube_1)
#' @export
list_species <- function(object) {

  scientificName <- NULL

  # Case 1: Object has a data element (e.g. processed_cube, indicator_map, indicator_ts)
  if (!is.null(object$data) && "scientificName" %in% names(object$data)) {
    species_df <- object$data %>%
      dplyr::select(scientificName) %>%
      dplyr::distinct(scientificName) %>%
      dplyr::arrange(scientificName)
    return(tibble::as_tibble(species_df))
  }

  # Case 2: Object has species_names vector (e.g. some indicator variants)
  if (length(object$species_names) > 0) {
    return(tibble::tibble(
      scientificName = sort(unique(as.character(object$species_names)))
    ))
  }

  # Fallback for data frames/tibbles directly
  if (is.data.frame(object) && "scientificName" %in% names(object)) {
    species_df <- object %>%
      dplyr::select(scientificName) %>%
      dplyr::distinct(scientificName) %>%
      dplyr::arrange(scientificName)
    return(tibble::as_tibble(species_df))
  }

  warning("Could not find species list in the provided object. Returning empty tibble.")
  return(tibble::tibble(scientificName = character(0)))
}
