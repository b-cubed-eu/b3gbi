#' Core function to calculate completeness (Sample Coverage) for map output
#'
#' This function prepares the data into a format suitable for the `iNEXT`
#' package and then calculates the sample coverage for each grid cell.
#'
#' @param x A processed data cube (data.frame or sf).
#' @param ... Additional arguments passed from `calc_map`.
#'
#' @return A data frame with cell IDs and completeness (sample coverage) values.
#' @noRd
calc_map_completeness_core <- function(x, ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    inherits(x, "completeness"))

  n <- S.obs <- SC <- cellid <- cellCode <- cellid_char <- diversity_val <-
    taxonKey <- obs <- scientificName <- year <- incidence <- Y_i <- NULL

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  data_type <- temp_opts$data_type
  data_type <- if (data_type == "incidence") "incidence_freq" else data_type
  assume_freq <- temp_opts$assume_freq %||% FALSE

  # Create cellid <-> cellCode mapping table
  cell_map <- x %>%
    dplyr::distinct(cellid, cellCode) %>%
    dplyr::mutate(cellid_char = as.character(cellid)) %>%
    dplyr::select(cellid, cellCode, cellid_char)

  if (data_type == "abundance") {
    incidence_list <- x %>%
      dplyr::select(cellid, cellCode, taxonKey, obs, scientificName) %>%
      dplyr::group_by(cellid) %>%
      {
        list_data <- dplyr::group_split(., .keep = FALSE)
        list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
          as.character()
        names(list_data) <- list_names
        list_data
      } %>%
      purrr::map(function(cell_data) {
        abundance_summary <- cell_data %>%
          dplyr::group_by(taxonKey, scientificName) %>%
          dplyr::summarise(count = sum(obs, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(scientificName)
        return(abundance_summary$count)
      })
  } else {
    if (assume_freq == FALSE) {
      incidence_list <- x %>%
        dplyr::select(taxonKey, obs, cellid, cellCode, year) %>%
        dplyr::mutate(incidence = as.numeric(obs > 0)) %>%
        dplyr::filter(incidence > 0) %>%
        dplyr::group_by(cellid) %>%
        {
          list_data <- dplyr::group_split(., .keep = FALSE)
          list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
            as.character()
          names(list_data) <- list_names
          list_data
        } %>%
        purrr::map(function(cell_data) {
          T_units <- dplyr::n_distinct(cell_data$year)
          freq_data <- cell_data %>%
            dplyr::group_by(taxonKey) %>%
            dplyr::summarise(Y_i = dplyr::n_distinct(year), .groups = "drop") %>%
            dplyr::pull(Y_i)
          return(c(T_units, freq_data))
        })
    } else {
      incidence_list <- x %>%
        dplyr::select(cellid, cellCode, taxonKey, obs, scientificName) %>%
        dplyr::group_by(cellid) %>%
        {
          list_data <- dplyr::group_split(., .keep = FALSE)
          list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
            as.character()
          names(list_data) <- list_names
          list_data
        } %>%
        purrr::map(function(cell_data) {
          freq_summary <- cell_data %>%
            dplyr::group_by(taxonKey, scientificName) %>%
            dplyr::summarise(Y_i = sum(obs, na.rm = TRUE), .groups = "drop")
          T_units <- max(freq_summary$Y_i, na.rm = TRUE)
          return(c(T_units, freq_summary$Y_i))
        })
    }
  }

  if (length(incidence_list) == 0) {
    stop("No grid cells to process. Something went wrong.")
  }

  # Filter based on cutoff_length
  incidence_list_processed <- purrr::keep(incidence_list, function(vec) {
    if (!is.null(vec) && is.vector(vec) && length(vec) > (if(data_type == "abundance") 0 else 1)) {
      species_count <- if (data_type == "abundance") length(vec) else length(vec) - 1
      return(species_count >= cutoff_length)
    }
    return(FALSE)
  })

  if (length(incidence_list_processed) == 0) {
    stop("There are no grid cells left to process after filtering.")
  }

  # Calculate completeness using iNEXT::DataInfo
  info <- my_DataInfo(incidence_list_processed, datatype = data_type)

  # Extract result
  indicator <- info %>%
    dplyr::select(Assemblage, SC) %>%
    dplyr::rename(cellid_char = Assemblage, diversity_val = SC) %>%
    dplyr::left_join(cell_map, by = "cellid_char") %>%
    dplyr::select(cellid, cellCode, diversity_val) %>%
    dplyr::mutate(cellid = as.integer(cellid))

  return(indicator)
}
