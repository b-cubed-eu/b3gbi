#' Calculate confidence intervals for list of `boot` objects
#'
#' This function calculates confidence intervals for a list of objects of class
#' `"boot"` per year into a dataframe containing all required summaries.
#'
#' @param bootstrap_list A list of objects of class `"boot"` per year.
#' @param ... Additional argument to be passed to the `boot::boot.ci()`
#' function.
#' @param temporal_list_name The temporal list names of `bootstrap_list`
#' (e.g., year, month ...) containing time point values. Default `year`.
#'
#' @returns The returned value is a dataframe containing the time point,
#' the type of interval (`int_type`), the lower limit of the confidence
#' interval (`ll`), the upper limit of the confidence interval (`ul`), and the
#' confidence level of the intervals (`conf_level`).

get_bootstrap_ci <- function(bootstrap_list, ..., temporal_list_name = "year") {
  require("dplyr")
  require("rlang")

  # Calculate nonparametric confidence intervals
  conf_ints <- lapply(bootstrap_list, boot::boot.ci, ...)

  # Remove null values
  conf_ints[sapply(conf_ints, is.null)] <- NULL

  # Exit if there are no values
  if (length(conf_ints) == 0) {
    return(conf_ints)
  }

  # Get interval names
  indices_to_remove <- match(c("R", "t0", "call"), names(conf_ints[[1]]))
  interval_types <- names(conf_ints[[1]])[-indices_to_remove]

  # Get confidence level
  conf_level <- conf_ints[[1]][[interval_types[1]]][1]

  # Summarise for each confidence interval upper and lower limits in dataframes
  out_list <- vector(mode = "list", length = length(interval_types))
  for (i in seq_along(interval_types)) {
    type <- interval_types[i]

    ll <- sapply(conf_ints, function(list) {
      vec <- list[[type]]
      vec[length(vec) - 1]
    })
    ul <- sapply(conf_ints, function(list) {
      vec <- list[[type]]
      vec[length(vec)]
    })

    out_list[[i]] <- data.frame(time_point = as.numeric(names(conf_ints)),
                                int_type = type,
                                ll = ll,
                                ul = ul)
  }

  # Create combined dataframe
  conf_df_out <- do.call(rbind.data.frame, out_list) %>%
    tidyr::complete("time_point" = as.numeric(names(bootstrap_list)),
                    .data$int_type) %>%
    dplyr::arrange(.data$time_point, .data$int_type) %>%
    dplyr::mutate(conf_level = conf_level) %>%
    dplyr::rename({{ temporal_list_name }} := "time_point")
  rownames(conf_df_out) <- NULL

  return(conf_df_out)
}
