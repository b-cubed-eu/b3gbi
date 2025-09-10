#' @title Calculate Confidence Intervals for a Biodiversity Indicator
#'
#' @description This function calculates bootstrap confidence intervals for a
#' biodiversity indicator. It is called automatically when calculating a
#' biodiversity indicator over time unless you choose 'none' for ci_type.
#'
#' @param x A data cube object
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param ... Additional arguments passed to specific calc_ci functions.
#' @export
calc_ci <- function(x,
                    indicator,
                    ...) {

  UseMethod("calc_ci")

}

#' @export
#' @rdname calc_ci
calc_ci.default <- function(x,
                            indicator,
                            ...) {

  warning(
    paste(
      "calc_ci does not know how to handle object of class ",
      class(x),
      ". Please ensure you are not calling calc_ci directly on an object."
    )
  )

}

#' Core function for handling the confidence interval calculations for different
#'  indicator types. This function is called by the calc_ci functions for each
#'  indicator type.
#' @param bootstraps Bootstrapped indicator values
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param ci_type Type of confidence interval to calculate
#' @param ... Additional arguments
#' @noRd
calc_ci_core <- function(bootstraps,
                         indicator,
                         ci_type,
                         ...) {

  year <- NULL

  # Calculate confidence intervals
  ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

  if (length(ci_df) > 0) {

    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                       by = dplyr::join_by(year),
                       relationship = "many-to-many")

  } else {

    warning(
      paste0(
        "Unable to calculate confidence intervals. There may be ",
        "insufficient data."
      )
    )
  }

  return(indicator)

}

#' @describeIn calc_ci Calculate confidence intervals for Hill richness
#' @export
calc_ci.hill0 <- function(x,
                          indicator,
                          ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill0"))

  indicator <- calc_ci_hill_core(x = x,
                                 indicator,
                                 type = "hill0",
                                 ...)

}

#' @describeIn calc_ci Calculate confidence intervals for Simpson-Hill diversity
#' @export
calc_ci.hill1 <- function(x,
                          indicator,
                          ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill1"))

  indicator <- calc_ci_hill_core(x = x,
                                 indicator,
                                 type = "hill1",
                                 ...)

}

#' @describeIn calc_ci Calculate confidence intervals for Shannon-Hill diversity
#' @export
calc_ci.hill2 <- function(x,
                          indicator,
                          ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill2"))

  indicator <- calc_ci_hill_core(x = x,
                                 indicator,
                                 type = "hill2",
                                 ...)

}

#' Core function for handling the confidence interval calculations for Hill
#' diversity. This function is called by the calc_ci.hill0, calc_ci.hill1,
#' calc_ci.hill2 functions.
#' @param x A data cube object
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param type (Optional) Type of Hill diversity function to calculate
#'  confidence intervals for. Options are "hill0" for richness, "hill1" for
#'  Simpson-Hill diversity, or "hill2" for Shannon-Hill diversity.
#' @param ... Additional arguments.
#' @noRd
calc_ci_hill_core <- function(x,
                              indicator,
                              type = c("hill0", "hill1", "hill2"),
                              ...) {

  stopifnot_error(
    "Please check the class and structure of your data.
     This is an internal function, not meant to be called directly.",
    inherits(x, c("data.frame", "sf")) &
      rlang::inherits_any(x, c("hill0", "hill1", "hill2"))
  )

  type <- match.arg(type)

  if ("ll" %in% colnames(indicator) && "ul" %in% colnames(indicator)) {

    return(indicator)

  } else {

    stop(
      paste0(
        "Confidence intervals for Hill numbers should be calculated by ",
        "the calc_ts function, but seem to be missing."
      )
    )
  }
}

#' @describeIn calc_ci Calculate confidence intervals for total occurrences
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.total_occ <- function(x,
                              indicator,
                              num_bootstrap = 1000,
                              ci_type = ci_type,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "total_occ"))

  year <- NULL

  x <-
    x %>%
    dplyr::arrange(year)

  # Put individual observations into a list organized by year
  ind_list <- list_org_by_year(x, "obs")

  # Bootstrap indicator value
  bootstraps <-
    ind_list %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_sum,
      R = num_bootstrap))

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for occurrence density
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.occ_density <- function(x,
                                indicator,
                                num_bootstrap = 1000,
                                ci_type = ci_type,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_density"))

  year <- cellid <- obs <- area <- NULL

  x <- x %>%
    dplyr::arrange(year, cellid) %>%
    dplyr::reframe(diversity_val = sum(obs) / area,
                   .by = c("year", "cellid"))

  # Put individual observations into a list organized by year
  ind_list <- list_org_by_year(x, "diversity_val")

  # Bootstrap indicator value
  bootstraps <-
    ind_list %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_mean,
      R = num_bootstrap))

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for newness
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.newness <- function(x,
                            indicator,
                            num_bootstrap = 1000,
                            ci_type = ci_type,
                            ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "newness"))

  year <- x$year

  # Put individual observations into a list organized by year
  ind_list <- lapply(unique(year), function(y) {
    a <- year[year <= y]
    return(a)
  })

  # Use years to name list elements
  names(ind_list) <- unique(year)

  # Bootstrap indicator value
  bootstraps <- ind_list %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_newness,
      R = num_bootstrap))

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for Williams' evenness
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.williams_evenness <- function(x,
                                      ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ci_evenness_core(x = x,
                                     type = "williams_evenness",
                                     ...)

  return(indicator)

}

#' @describeIn calc_ci Calculate confidence intervals for Pielou's evenness
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.pielou_evenness <- function(x,
                                    ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ci_evenness_core(x = x,
                                     type = "pielou_evenness",
                                     ...)

  return(indicator)

}

#' Core function to calculate confidence intervals for evenness. This is called
#'  by the calc_ci.pielou_evenness and calc_ci.williams_evenness functions.
#' @param x A data cube object.
#' @param type Evenness measure ("pielou_evenness" or "williams_evenness)
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @param ... Additional arguments
#' @noRd
calc_ci_evenness_core <- function(x,
                                  type,
                                  indicator,
                                  num_bootstrap = 1000,
                                  ci_type = ci_type,
                                  ...) {

  obs <- year <- taxonKey <- num_occ <- NULL

  stopifnot_error(
    "Please check the class and structure of your data.
    This is an internal function, not meant to be called directly.",
    inherits(x, c("data.frame", "sf"))
  )

  type <- match.arg(type,
                    names(available_indicators))

  # Calculate number of records for each species by grid cell
  x <- x %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ,
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  # Bootstrap evenness values
  bootstraps <- x %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_evenness,
      R = num_bootstrap,
      type = type))

  # Replace NA values to avoid errors when calculating confidence intervals
  bootstraps <- lapply(bootstraps, ci_error_prevent)

  names(bootstraps) <- unique(indicator$year)

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for abundance-based rarity
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.ab_rarity <- function(x,
                              indicator,
                              num_bootstrap = 1000,
                              ci_type = ci_type,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "ab_rarity"))

  obs <- taxonKey <- records_taxon <- year <- NULL

    # Calculate rarity for each cell each year
    x <- x %>%
      dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
      dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
      dplyr::arrange(year)

    ind_list <- list_org_by_year(x, "rarity")

    # Bootstrap indicator value
    bootstraps <- ind_list %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_sum,
        R = num_bootstrap))

    # Calculate confidence intervals and add to indicator values
    ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

    return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for area-based rarity
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.area_rarity <- function(x,
                                indicator,
                                num_bootstrap = 1000,
                                ci_type = ci_type,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "area_rarity"))

  year <- cellid <- taxonKey <- rec_tax_cell <- rarity <- diversity_val <- NULL

  # Calculate rarity for each cell each year
  x <- x %>%
    dplyr::arrange(year, cellid, taxonKey) %>%
    dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 /
                    (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
    dplyr::arrange(year)

  # Put cell-based rarity values into a list organized by year
  ind_list <- list_org_by_year(x, "diversity_val")

  # Bootstrap indicator value
  bootstraps <- ind_list %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_mean,
      R = num_bootstrap))

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}

#' @describeIn calc_ci Calculate confidence intervals for species occurrences
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.spec_occ <- function(x,
                             indicator,
                             num_bootstrap = 1000,
                             ci_type = ci_type,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_occ"))

  taxonKey <- totobs <- obs <- year <- cellCode <- scientificName <- NULL

  x <- x %>%
    dplyr::arrange(taxonKey)

  # Bootstrap species occurrence values
  bootstraps <- x %>%
    dplyr::summarize(totobs = sum(obs), .by = c(year, cellCode, taxonKey)) %>%
    dplyr::group_by(taxonKey) %>%
    dplyr::group_split() %>%
    purrr::map(function(taxon_df) {
      taxon_df %>%
        dplyr::select(year, totobs, cellCode) %>%
        dplyr::arrange(year) %>%
        tidyr::pivot_wider(names_from = year,
                           values_from = totobs,
                           values_fill = 0) %>%
        tibble::column_to_rownames("cellCode") %>%
        purrr::map(function(year_data) {
          boot::boot(
            data = year_data,
            statistic = boot_statistic_sum,
            R = num_bootstrap
          )
        })
    })

  taxkeys <- unique(x$taxonKey)
  scinames <- unique(x$scientificName)

  # Calculate confidence intervals
  ci_df_list <- list()
  for (i in seq_along(bootstraps)) {

    ci_df_list[[i]] <- get_bootstrap_ci(bootstraps[[i]], type = ci_type, ...)
    if (length(ci_df_list[[i]]) > 0) {
      ci_df_list[[i]]$taxonKey <- taxkeys[i]
      ci_df_list[[i]]$scientificName <- scinames[i]

    }
  }

  # Convert list to data frame
  ci_df <- do.call(rbind, ci_df_list)

  # Convert negative values to zero as rarity cannot be less than zero
  ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

  # Join confidence intervals to indicator values
  indicator <- indicator %>%
    dplyr::full_join(ci_df,
                     by = dplyr::join_by(year, taxonKey, scientificName),
                     relationship = "many-to-many")

  return(indicator)

}

#' @describeIn calc_ci Calculate confidence intervals for species range
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @export
calc_ci.spec_range <- function(x,
                               indicator,
                               num_bootstrap = 1000,
                               ci_type = ci_type,
                               ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_range"))

  taxonKey <- obs <- observed <- year <- cellCode <- scientificName <- NULL

  x <- x %>%
    dplyr::arrange(taxonKey, year, cellCode)

  # Bootstrap species range values
  bootstraps <- x %>%
    dplyr::summarize(observed = sum(obs >= 1),
                     .by = c(taxonKey, year, cellCode)) %>%
    dplyr::group_by(taxonKey) %>%
    dplyr::group_split() %>%
    purrr::map(function(taxon_df) {
      taxon_df %>%
        dplyr::select(year, observed, cellCode) %>%
        dplyr::arrange(year) %>%
        tidyr::pivot_wider(names_from = year,
                           values_from = observed,
                           values_fill = 0) %>%
        tibble::column_to_rownames("cellCode") %>%
        purrr::map(function(year_data) {
          boot::boot(
            data = year_data,
            statistic = boot_statistic_sum,
            R = num_bootstrap
          )
        })
    })

  taxkeys <- unique(x$taxonKey)
  scinames <- unique(x$scientificName)

  # Calculate confidence intervals
  ci_df_list <- list()
  for (i in seq_along(bootstraps)) {

    ci_df_list[[i]] <- get_bootstrap_ci(bootstraps[[i]], type = ci_type, ...)
    if (length(ci_df_list[[i]]) > 0) {
      ci_df_list[[i]]$taxonKey <- taxkeys[i]
      ci_df_list[[i]]$scientificName <- scinames[i]

    }
  }

  # Convert list to data frame
  ci_df <- do.call(rbind, ci_df_list)

  # Convert negative values to zero as rarity cannot be less than zero
  ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

  # Join confidence intervals to indicator values
  indicator <- indicator %>%
    dplyr::full_join(ci_df,
                     by = dplyr::join_by(year, taxonKey, scientificName),
                     relationship = "many-to-many")

  return(indicator)

}
