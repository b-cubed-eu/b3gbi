#' Get occurrences for selected species and split by species
#' @noRd
get_occurrences_and_split <- function(data, filter_type, species) {

  errormsg <- paste0(
    "No matching ", filter_type, "s. Please check that you have entered ",
    "them correctly."
  )

 if (filter_type == "taxonKey") {

   # Get occurrences for selected species
   species_occurrences <- data %>%
     dplyr::filter(taxonKey %in% species) %>%
     {
       if (nrow(.) < 1) stop(errormsg) else (.)
     } %>%
     dplyr::mutate(taxonKey = factor(taxonKey, levels = unique(taxonKey)))

   split_so <-
     species_occurrences %>%
     dplyr::group_split(taxonKey)

 } else if (filter_type == "scientificName") {

   # Get occurrences for selected species
   species_occurrences <- data %>%
     dplyr::filter(
       grepl(paste("^", species, collapse = "|", sep = ""), scientificName)) %>%
     {
       if (nrow(.) < 1) stop(errormsg) else (.)
     } %>%
     dplyr::arrange(
       scientificName,
       grepl(paste("^", species, collapse = "|", sep = ""), scientificName)
     )

   split_so <-
     species_occurrences %>%
     dplyr::group_split(scientificName)

 } else {

   stop("filter_type must be either taxonKey or species.")

 }

  return(split_so)

}
