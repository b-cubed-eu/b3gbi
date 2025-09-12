#' @noRd
calc_ts_evenness_core <- function(x, type, ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

  num_occ <- obs <- year <- taxonKey <- NULL

  type <- match.arg(type, names(available_indicators))

  # Check if the data is empty
  if (nrow(x) == 0) {
    return(tibble::tibble(year = integer(), diversity_val = numeric()))
  }

  # Calculate number of records for each species by grid cell
  x <- x %>%
    dplyr::summarize(num_occ = sum(obs), .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ,
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  # Apply evenness formula and format result as a data frame
  indicator <- x %>%
    purrr::map(function(y) {
      compute_evenness_formula(y, type)
    }) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = as.integer(year), .keep = "unused")

  return(indicator)

}
