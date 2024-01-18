#' @export
list_species <- function(data) {

  species_df <-
    data %>%
    dplyr::select(taxonKey, scientificName) %>%
    dplyr::distinct(taxonKey, scientificName)

  return(species_df)

}
