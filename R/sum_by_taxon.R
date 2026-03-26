sum_by_taxon <- function(object, rank) {
  
  if (!is.character(rank)) {
    stop("rank must be a character string")
  }
  
  data <- object$data
  
  summed_data <- data %>% 
    dplyr::group_by(.data[[rank]]) %>% 
    dplyr::summarize(total_observations = sum(obs, na.rm = TRUE)) %>% 
    dplyr::select(total_observations, .data[[rank]])
  
  return(summed_data)
}
