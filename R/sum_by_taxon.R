#' Sum Observations by Taxonomic Rank
#'
#' This function takes a \code{processed_cube} object and aggregates the
#' observation counts by a specified taxonomic rank.
#'
#' @param object A \code{processed_cube} object containing taxonomic information
#'  in \code{object$data}.
#' @param rank A character string specifying the taxonomic rank to group by
#'  (e.g., "family", "kingdom"). This column must exist in \code{object$data}.
#'
#' @return A tibble with columns \code{total_observations} and the specified \code{rank}.
#'
#' @examples
#' \dontrun{
#' # Assuming 'cube' is a processed_cube object
#' family_sums <- sum_by_taxon(cube, "family")
#' }
#'
#' @export
sum_by_taxon <- function(object, rank) {

  if (!inherits(object, "processed_cube")) {
    stop("object must be of class 'processed_cube'")
  }

  if (!is.character(rank) || length(rank) != 1) {
    stop("rank must be a single character string")
  }

  data <- object$data

  if (!rank %in% names(data)) {
    stop(sprintf("rank '%s' not found in object$data", rank))
  }

  summed_data <- data %>%
    dplyr::group_by(.data[[rank]]) %>%
    dplyr::summarize(total_observations = sum(obs, na.rm = TRUE)) %>%
    dplyr::select(total_observations, .data[[rank]])

  return(summed_data)
}
