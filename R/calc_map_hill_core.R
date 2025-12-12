#' @param type Which Hill diversity to calculate ("hill0", "hill1", "hill2")
#' @noRd
calc_map_hill_core <- function(x, type = c("hill0", "hill1", "hill2"), ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  qD <- cellid <- Assemblage <- taxonKey <- m <- SC <- Order.q <- . <-
    scientificName <- count <- year <- Y_i <- incidence <- everything <-
  Method <- obs <- cellCode <- cellid_char <- diversity_val <- NULL

  type <- match.arg(type)

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  coverage <- temp_opts$coverage
  assume_freq <- temp_opts$assume_freq
  data_type <- temp_opts$data_type
  data_type <- if (data_type == "incidence") "incidence_freq" else data_type

  # Create cellid <-> cellCode mapping table
  cell_map <- x %>%
    dplyr::distinct(cellid, cellCode) %>%
    # Convert cellid to character/string as it's used for list names
    dplyr::mutate(cellid_char = as.character(cellid)) %>%
    dplyr::select(cellid, cellCode, cellid_char)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  if (data_type == "abundance") {

    incidence_list <- x %>%
      # 1. Select relevant columns
      dplyr::select(cellid, cellCode, taxonKey, obs, scientificName) %>%

      # 2. Group by the spatial unit
      dplyr::group_by(cellid) %>%

      # 3. Split the data and name the list elements
      {
        list_data <- dplyr::group_split(., .keep = FALSE)
        list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
          as.character()
        names(list_data) <- list_names
        list_data
      } %>%

      # 4. Process each cell's data frame to calculate total abundance
      purrr::map(function(cell_data) {

        # Calculate Total Abundance (sum of obs) for each species
        abundance_summary <- cell_data %>%
          # Group by both keys to maintain the species name for sorting
          dplyr::group_by(taxonKey, scientificName) %>%
          # Sum 'obs' to get the total abundance (Y_i)
          dplyr::summarise(count = sum(obs, na.rm = TRUE), .groups = "drop") %>%

          # Sort by 'species' alphabetically for consistency (optional but recommended)
          dplyr::arrange(scientificName)

        # Extract the final abundance counts vector
        abundance_vector <- abundance_summary %>%
          dplyr::pull(count)

        return(abundance_vector)
      })

  } else {

    if (assume_freq == FALSE) {

      incidence_list <- x %>%
        dplyr::select(taxonKey, obs, cellid, cellCode, year) %>%

        # Filter and setup the data structure
        dplyr::mutate(incidence = as.numeric(obs > 0)) %>%
        dplyr::filter(incidence > 0) %>% # Keep only detections

        # Group the data
        dplyr::group_by(cellid) %>%

        # Split the data and name the list elements
        {
          list_data <- dplyr::group_split(., .keep = FALSE)
          list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
            as.character()
          names(list_data) <- list_names
          list_data
        } %>%

        # Process each cell's data frame to calculate T and Y_i
        purrr::map(function(cell_data) {

          # T_units: Total unique years (T)
          T_units <- dplyr::n_distinct(cell_data$year)

          # freq_data: Incidence frequency (Y_i) - number of unique years each species was detected
          freq_data <- cell_data %>%
            dplyr::group_by(taxonKey) %>%
            dplyr::summarise(Y_i = dplyr::n_distinct(year), .groups = "drop") %>%
            dplyr::ungroup() %>%
            dplyr::pull(Y_i)

          # Combine T and the frequencies (Y_i)
          incidence_vector <- c(T_units, freq_data)

          return(incidence_vector)
        })

    } else {

      incidence_list <- x %>%
        # 1. Select relevant columns
        dplyr::select(cellid, cellCode, taxonKey, obs, scientificName) %>%

        # 2. Group by the spatial unit
        dplyr::group_by(cellid) %>%

        # 3. Split the data frame into a list of data frames (one per cell)
        #  Use group_keys() and pull the cellid to name the list
        {
          list_data <- dplyr::group_split(., .keep = FALSE)
          list_names <- dplyr::group_keys(.) %>% dplyr::pull(cellid) %>%
            as.character()
          names(list_data) <- list_names
          list_data
        } %>%

        # 4. Process each cell's data frame
        purrr::map(function(cell_data) {

          # Calculate Incidence Frequencies (Y_i)
          freq_summary <- cell_data %>%
            dplyr::group_by(taxonKey, scientificName) %>%
            dplyr::summarise(Y_i = sum(obs, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(scientificName)

          # --- CALCULATE T ---
          T_units <- freq_summary %>%
            dplyr::pull(Y_i) %>%
            max(na.rm = TRUE)

          # Extract the Y_i counts
          freq_data <- freq_summary %>%
            dplyr::pull(Y_i)

          # Combine T and the frequencies (Y_i)
          incidence_vector <- c(T_units, freq_data)

          return(incidence_vector)
        })
    }
  }

  if (length(incidence_list) == 0) {
    stop(paste0("No grid cells to process. Something went wrong."))
  }

  incidence_list_processed <-
    purrr::keep(incidence_list,
                function(vec) {
                  # Check if the vector exists and has more than 1 element
                  # (T plus at least one species)
                  if (!is.null(vec) && is.vector(vec) && length(vec) > 1) {
                    # The number of species is the length of the vector minus the T value
                    if (data_type == "abundance") {
                      species_count <- length(vec)
                    } else {
                      species_count <- length(vec) - 1
                    }
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
    my_estimateD(datatype = data_type,
                 base = "coverage",
                 level = coverage,
                 q = qval,
                 conf = 0.95,
                 nboot = 0)

  # Extract and format the results
  indicator <- diversity_estimates %>%
    dplyr::select(Assemblage, qD, m, SC, Order.q, Method) %>%
    dplyr::rename(cellid_char = Assemblage,
                  diversity_val = qD,
                  samp_size_est = m,
                  coverage = SC,
                  diversity_type = Order.q,
                  method = Method) %>%
    # Join the cellCode back using the cellid string
    dplyr::left_join(cell_map, by = "cellid_char") %>%
    # Reorder and finalize the data frame
    dplyr::select(cellid, cellCode, diversity_val, everything(), -cellid_char) %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")


  return(indicator)

}
