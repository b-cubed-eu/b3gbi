#' Get occurrences for selected species and split by species
#' @noRd
get_occurrences_and_split <- function(data, filter_type, species) {

  taxonKey <- scientificName <- NULL

  errormsg <- paste0(
    "No matching ", filter_type, "s. Please check that you have entered ",
    "them correctly."
  )

  check_for_empty <- function(data, errormsg) {
    if (nrow(data) < 1) stop(errormsg)
    return(data)
  }

  starts_with <- function(string, patterns) {
    regex <- paste("^", patterns, collapse = "|", sep = "")
    grepl(regex, string)
  }

  split_by_column <- function(data, col) {
    data %>% dplyr::group_split({{ col }})
  }

  if (filter_type == "taxonKey") {

    # Get occurrences for selected species and split by taxonKey
    split_so <- data %>%
      dplyr::filter(taxonKey %in% species) %>%
      check_for_empty(errormsg) %>%
      dplyr::mutate(taxonKey = factor(taxonKey, levels = unique(taxonKey))) %>%
      split_by_column(taxonKey)

  } else if (filter_type == "scientificName") {

    # Get occurrences for selected species and split by scientificName
    split_so <- data %>%
      dplyr::filter(starts_with(scientificName, species)) %>%
      check_for_empty(errormsg) %>%
      dplyr::arrange(scientificName, starts_with(scientificName, species)) %>%
      split_by_column(scientificName)

  } else {

    stop("filter_type must be either taxonKey or species.")

  }

  return(split_so)

}
