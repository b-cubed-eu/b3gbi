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

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  # Create list of incidence matrices by cell
  incidence_list <- x %>%
    dplyr::select(taxonKey, obs, cellid) %>%
    dplyr::group_by(cellid) %>%
    dplyr::group_split() %>%
    # Process each cellid's data frame
    purrr::map(function(cell_data) {
      df <- cell_data %>%
        dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
        dplyr::select(taxonKey, obs)

      # Convert to matrix, ensuring dimnames are preserved
      # This is a more robust way to handle single-row data
      mat <- as.matrix(df[, "obs", drop = FALSE])
      rownames(mat) <- df$taxonKey

      return(mat)
    })

  if (length(incidence_list) == 0) {
    stop(paste0("No grid cells to process. Something went wrong."))
  }

  # Name the list elements by cellid
  names(incidence_list) <- unique(x$cellid)

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  coverage <- temp_opts$coverage

  # Filter cells based on cutoff length (number of species >= cutoff)
  incidence_list_filtered <- purrr::keep(incidence_list,
                                         function(matrix) {
                                           if (!is.null(matrix) && is.matrix(matrix)) {
                                             return(nrow(matrix) >= cutoff_length)
                                           }
                                           return(FALSE)
                                         })

  if (length(incidence_list_filtered) == 0) {
    stop(paste0("There are no grid cells left to process after filtering. ",
                "Try setting the cutoff_length lower."))
  }

  incidence_list_processed <- lapply(incidence_list_filtered,
                                     function(matrix) {
                                       numeric_matrix <- matrix(as.numeric(matrix),
                                                                nrow = nrow(matrix),
                                                                dimnames = dimnames(matrix))
                                       return(numeric_matrix)
                                     })

  # Compute Hill diversity using a wrapper for iNEXT::estimateD
  diversity_estimates <- incidence_list_processed %>%
    my_estimateD(datatype = "abundance",
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
