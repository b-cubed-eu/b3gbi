#' @noRd
calc_map_evenness_core <- function(x,
                                   type,
                                   ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

  num_occ <- obs <- cellid <- taxonKey <- . <- NULL

  type <- match.arg(type,
                    names(available_indicators))

  if (nrow(x) == 0) {
    return(tibble::tibble(cellid = integer(0), diversity_val = numeric(0)))
  }

  # Calculate adjusted evenness fo r each grid cell
  indicator <- x %>%
    dplyr::summarize(num_occ = sum(obs, na.rm = TRUE),
                     .by = c(cellid, taxonKey)) %>%
    dplyr::arrange(cellid) %>%
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
    dplyr::mutate(cellid = as.integer(cellid),
                  .keep = "unused")

  return(indicator)

}
