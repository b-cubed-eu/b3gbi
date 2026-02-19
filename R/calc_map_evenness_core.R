#' @noRd
calc_map_evenness_core <- function(x,
                                   type,
                                   ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

  num_occ <- obs <- cellid <- taxonKey <- cellCode <- diversity_val <- . <- NULL

  type <- match.arg(type, names(available_indicators))

  if (nrow(x) == 0) {
    return(tibble::tibble(cellid = integer(0),
                          cellCode = character(0),
                          diversity_val = numeric(0)))
  }

  # --- CRITICAL ADDITION 1: Create the cellid <-> cellCode mapping table ---
  # We need a clean, unique mapping to join back at the end.
  cell_map <- x %>%
    dplyr::distinct(cellid, .keep_all = TRUE) %>%
    dplyr::select(cellid, cellCode)

  # Calculate adjusted evenness for each grid cell
  indicator <- x %>%
    dplyr::summarize(num_occ = sum(obs, na.rm = TRUE),
                     .by = c(cellid, taxonKey)) %>%
    dplyr::arrange(cellid) %>%

    # We must explicitly drop cellCode before pivoting so the data is clean
    # for the calculation matrix, and rely only on the separate cell_map later.
    dplyr::select(-cellCode) %>% # <--- CRITICAL CHANGE 2

    tidyr::pivot_wider(names_from = cellid,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list() %>%
    purrr::map(~compute_evenness_formula(., type)) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "cellid") %>%

    # --- CRITICAL ADDITION 3: Join the original cellCode back ---
    # Convert cellid to the correct type for the join.
    dplyr::mutate(cellid = as.integer(cellid)) %>%

    # Join the mapping table to the calculated results
    dplyr::left_join(cell_map, by = "cellid") %>%

    # Reorder columns for final output
    dplyr::select(cellid, cellCode, diversity_val)

  return(indicator)
}
