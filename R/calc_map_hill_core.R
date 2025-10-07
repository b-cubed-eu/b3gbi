#' @param type Which Hill diversity to calculate ("hill0", "hill1", "hill2")
#' @noRd
calc_map_hill_core <- function(x, type = c("hill0", "hill1", "hill2"), ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  qD <- cellid <- Assemblage <- taxonKey <- m <- SC <- Order.q <- NULL
  Method <- obs <- NULL

  type <- match.arg(type)

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  coverage <- temp_opts$coverage

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  incidence_list <- x %>%
    dplyr::select(taxonKey, obs, cellid, year) %>%
    dplyr::mutate(incidence = as.numeric(obs > 0)) %>%
    dplyr::filter(incidence > 0) %>% # Keep only detections
    dplyr::group_by(cellid) %>%
    dplyr::group_split() %>%

    purrr::map(function(cell_data) {
      T_units <- dplyr::n_distinct(cell_data$year) # Total unique years (T)

      freq_data <- cell_data %>%
        dplyr::group_by(taxonKey) %>%
        dplyr::summarise(Y_i = dplyr::n_distinct(year)) %>%
        dplyr::ungroup() %>%
        dplyr::pull(Y_i)

      # Combine T and the frequencies (Y_i)
      incidence_vector <- c(T_units, freq_data)

      return(incidence_vector)
    })

  if (length(incidence_list) == 0) {
    stop(paste0("No grid cells to process. Something went wrong."))
  }

  # Name the list elements by cellid
  names(incidence_list) <- unique(x$cellid)

  incidence_list_processed <-
    purrr::keep(incidence_list,
                function(vec) {
                  # Check if the vector exists and has more than 1 element
                  # (T plus at least one species)
                  if (!is.null(vec) && is.vector(vec) && length(vec) > 1) {
                    # The number of species is the length of the vector minus the T value
                    species_count <- length(vec) - 1
                    return(species_count >= cutoff_length)
                  }
                  # Return FALSE for empty or invalid vectors
                  return(FALSE)
                })

  if (length(incidence_list_processed) == 0) {
    stop(paste0("There are no grid cells left to process after filtering. ",
                "Try setting the cutoff_length lower."))

  }

  # Compute Hill diversity using a wrapper for iNEXT::estimateD
  diversity_estimates <- incidence_list_processed %>%
    my_estimateD(datatype = "incidence_freq",
                 base = "coverage",
                 level = coverage,
                 q = qval,
                 conf = 0.95,
                 nboot = 0)

  # Extract and format the results
  indicator <- diversity_estimates %>%
    dplyr::select(Assemblage, qD, m, SC, Order.q, Method) %>%
    dplyr::rename(cellid = Assemblage,
                  diversity_val = qD,
                  samp_size_est = m,
                  coverage = SC,
                  diversity_type = Order.q,
                  method = Method) %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")

  return(indicator)

}
