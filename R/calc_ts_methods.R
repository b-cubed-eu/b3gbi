#' @export
calc_ts.default <- function(x, ...){

  warning(paste("calc_ts does not know how to handle object of class ",
                class(x),
                ". Please ensure you are not calling calc_ts directly on an object."))

}

#' @noRd
calc_ts.hill0 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill0"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill0",
                                  ...)

  return(indicator)

}

#' @noRd
calc_ts.hill1 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill1"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill1",
                                  ...)

  return(indicator)

}

#' @noRd
calc_ts.hill2 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill2"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill2",
                                  ...)

  return(indicator)
}

#' @noRd
calc_ts.hill_core <- function(x,
                              type = c("hill0", "hill1", "hill2"),
                              ...)
{

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) & rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  type <- match.arg(type)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  richness_by_year <-
    x %>%
    dplyr::summarise(obs_richness = dplyr::n_distinct(scientificName),
                     .by = "year")

  # Create list of occurrence matrices by year, with species as rows
  species_records_raw <-
    x %>%
    dplyr::select(year, scientificName, obs, cellCode) %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::group_by(scientificName) %>%
                 tidyr::pivot_wider(names_from = "scientificName",
                                    values_from = "obs") %>%
                 dplyr::ungroup() %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 dplyr::select(-year, -cellCode) %>%
                 tibble::rownames_to_column() %>%
                 tidyr::gather(variable,
                               value,
                               -rowname) %>%
                 tidyr::spread(rowname, value) %>%
                 'row.names<-'(., NULL) %>%
                 tibble::column_to_rownames(var = "variable") %>%
                 as.matrix() %>%
                 ifelse(. > 1, 1, .))

  # name list elements
  names(species_records_raw) <- richness_by_year$year


  temp_opts <- list(...)

  cutoff_length <- temp_opts$cutoff_length

  coverage <- temp_opts$coverage

  # remove all years with too little data to avoid errors from iNEXT
  species_records_raw2 <- species_records_raw %>%
    purrr::keep(., function(x) length(x) > cutoff_length)


  coverage_rare <- species_records_raw2 %>%
    iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

  # Extract estimated relative species richness
  indicator <-
    coverage_rare %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q, qD.LCL, qD.UCL) %>%
    dplyr::rename(year = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q,
                  ll = qD.LCL,
                  ul = qD.UCL) %>%
    dplyr::mutate(year = as.numeric(year))

}

#' @export
#' @rdname calc_ts
calc_ts.obs_richness <- function(x,
                                 indicator = NULL,
                                 bootstrap = FALSE,
                                 num_bootstrap = 1000,
                                 ci_type = ci_type,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "obs_richness"))

  x <-
    x %>%
    dplyr::select(year, taxonKey) %>%
    dplyr::arrange(year)

  if (bootstrap==TRUE) {

    x <-
      x %>%
      dplyr::summarise(unique_by_year = dplyr::n_distinct(taxonKey),
                       .by = "year")

    # # Put individual observations into a list organized by year
    # ind_list <- list_org_by_year(x, "taxonKey")
    #
    # # Bootstrap indicator value
    # bootstraps <-
    #   ind_list %>%
    #   purrr::map(~boot::boot(
    #     data = .,
    #     statistic = boot_statistic_ndistinct,
    #     R = num_bootstrap))
    #
    # # Calculate confidence intervals
    # ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)
    #
    # # Convert negative values to zero as evenness cannot be below zero
    # ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)


    # Calculate confidence intervals by permutation
    ci_df <- permute_ci(x, num_bootstrap)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

  } else {

    # Calculate observed species richness by year
    indicator <-
      x %>%
      dplyr::summarise(diversity_val = dplyr::n_distinct(taxonKey),
                       .by = "year")

  }

}

#' @export
#' @rdname calc_ts
calc_ts.cum_richness <- function(x,
                                 indicator = NULL,
                                 bootstrap = FALSE,
                                 num_bootstrap = 1000,
                                 ci_type = ci_type,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "cum_richness"))


    # Calculate the cumulative number of unique species observed
    x <-
      x %>%
      dplyr::select(year, taxonKey) %>%
      dplyr::arrange(year) %>%
      dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
      dplyr::summarize(unique_by_year = length(taxonKey),
                       .by = year)

    if (bootstrap==TRUE) {

    # Calculate confidence intervals by permutation
    ci_df <- permute_ci(x, num_bootstrap, cumsum = TRUE)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")


  } else {

    # Calculate the cumulative number of unique species observed
    indicator <-
      x %>%
      dplyr::reframe(year = year,
                     diversity_val = cumsum(unique_by_year))

  }

}

#' @export
#' @rdname calc_ts
calc_ts.total_occ <- function(x,
                              indicator = NULL,
                              bootstrap = FALSE,
                              num_bootstrap = 1000,
                              ci_type = ci_type,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "total_occ"))


  if (bootstrap==TRUE) {

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

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as evenness cannot be below zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

  } else {

  # Calculate total number of occurrences over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(obs),
                     .by = "year")

  }

}

#' @export
#' @rdname calc_ts
calc_ts.occ_density <- function(x,
                                indicator = NULL,
                                bootstrap = FALSE,
                                num_bootstrap = 1000,
                                ci_type = ci_type,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_density"))


  if (bootstrap==TRUE) {

    x <-
      x %>%
      arrange(year, cellid) %>%
      dplyr::reframe(diversity_val = sum(obs) / area_km2,
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

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as evenness cannot be below zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

  } else {

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    x %>%
    arrange(year, cellid) %>%
    dplyr::reframe(diversity_val = sum(obs) / area_km2,
                   .by = c("year", "cellid")) %>%
    dplyr::reframe(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val)) %>%
    dplyr::arrange(year)

  }

}

#' @export
#' @rdname calc_ts
calc_ts.newness <- function(x,
                            indicator = NULL,
                            bootstrap = FALSE,
                            num_bootstrap = 1000,
                            ci_type = ci_type,
                            ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "newness"))

  if (bootstrap==TRUE) {

    # Put individual observations into a list organized by year
    ind_list <- lapply(unique(x$year), function(y){
      a <- x$year[x$year<=y]
      return(a)
    })

    # Use years to name list elements
    names(ind_list) <- unique(x$year)

    # Bootstrap indicator value
    bootstraps <-
      ind_list %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_newness,
        R = num_bootstrap))

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as evenness cannot be below zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

  } else {

    yearvals <- vector()
    counter <- 1
    for (i in unique(x$year)) {
      yearvals[counter]  <- round(mean(x$year[x$year <= i]))
      counter <- counter + 1
    }

    indicator <- data.frame("year" = unique(x$year),
                            "diversity_val" = yearvals)

  }

}

#' @export
#' @rdname calc_ts
calc_ts.williams_evenness <- function(x,
                                      ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts.evenness_core(x = x,
                                      type = "williams_evenness",
                                      ...)

}

#' @export
#' @rdname calc_ts
calc_ts.pielou_evenness <- function(x,
                                    ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts.evenness_core(x = x,
                                      type = "pielou_evenness",
                                      ...)

}

#' @noRd
calc_ts.evenness_core <- function(x,
                                  type,
                                  indicator = NULL,
                                  bootstrap = FALSE,
                                  num_bootstrap = 1000,
                                  ci_type = ci_type,
                                  ...) {

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))


  type <- match.arg(type,
                    names(available_indicators))

  # Calculate number of records for each species by grid cell
  x <-
    x %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  if (bootstrap==TRUE) {

    # Bootstrap evenness values
    bootstraps <-
      x %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_evenness,
        R = num_bootstrap,
        type = type))

    # Replace NA values to avoid errors when calculating confidence intervals
    bootstraps <- lapply(bootstraps, ci_error_prevent)

    names(bootstraps) <- unique(indicator$year)

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as evenness cannot be below zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

  } else {

    indicator <-
      x %>%
      purrr::map(~compute_evenness_formula(. ,type)) %>%
      unlist() %>%
      as.data.frame() %>%
      dplyr::rename(diversity_val = ".") %>%
      tibble::rownames_to_column(var = "year") %>%
      dplyr::mutate(year = as.integer(year),
                    .keep = "unused")

  }

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.ab_rarity <- function(x,
                              indicator = NULL,
                              bootstrap = FALSE,
                              num_bootstrap = 1000,
                              ci_type = ci_type,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "ab_rarity"))

  if (bootstrap==TRUE) {

    # Calculate rarity for each cell each year
    x <-
      x %>%
      dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
      dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
      dplyr::arrange(year)

    ind_list <- list_org_by_year(x, "rarity")

    # Bootstrap indicator value
    bootstraps <-
    ind_list %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_sum,
        R = num_bootstrap))

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

    return(indicator)

  } else {

    # Calculate total summed rarity (in terms of abundance) for each grid cell
    indicator <-
      x %>%
      dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
      dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
      dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
      dplyr::summarise(diversity_val = sum(diversity_val), .by = "year") %>%
      dplyr::arrange(year)

  }

}

#' @export
#' @rdname calc_ts
calc_ts.area_rarity <- function(x,
                                indicator = NULL,
                                bootstrap = FALSE,
                                num_bootstrap = 1000,
                                ci_type = ci_type,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "area_rarity"))

  if (bootstrap==TRUE) {

    # Calculate rarity for each cell each year
    x <-
      x %>%
      dplyr::arrange(year, cellid, taxonKey) %>%
      dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                    .by = c(taxonKey)) %>%
      dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
      dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
      dplyr::arrange(year)

    # Put cell-based rarity values into a list organized by year
    ind_list <- list_org_by_year(x, "diversity_val")

    # Bootstrap indicator value
    bootstraps <-
      ind_list %>%
      purrr::map(~boot::boot(
        data = .,
        statistic = boot_statistic_mean,
        R = num_bootstrap))

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

    return(indicator)

  } else {

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
    indicator <-
      x %>%
      dplyr::arrange(year, cellid, taxonKey) %>%
      dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                    .by = c(taxonKey)) %>%
      dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
      dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
      dplyr::summarise(diversity_val = mean(diversity_val), .by = "year") %>%
      dplyr::arrange(year)

  }

}

#' @export
#' @rdname calc_ts
calc_ts.spec_occ <- function(x,
                             indicator=indicator,
                             bootstrap=FALSE,
                             num_bootstrap=1000,
                             ci_type = ci_type,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_occ"))

  if (bootstrap==TRUE) {

  x <-
    x %>%
    dplyr::arrange(taxonKey)

  # Bootstrap species occurrence values
  bootstraps <-
    x %>%
    dplyr::summarize(totobs = sum(obs), .by = c(year,cellCode,taxonKey)) %>%
    dplyr::group_by(taxonKey) %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::select(year,totobs,cellCode) %>%
                 dplyr::arrange(year) %>%
                 tidyr::pivot_wider(names_from = year, values_from = totobs) %>%
                 replace(is.na(.),0) %>%
                 tibble::column_to_rownames("cellCode") %>%
                 as.list() %>%
                 purrr::map(. %>%
                              boot::boot(
                                data = .,
                                statistic = boot_statistic_sum,
                                R = num_bootstrap
                              )
                 )
               )

  taxkeys <- unique(x$taxonKey)
  scinames <- unique(x$scientificName)

  # Calculate confidence intervals
  ci_df_list <- list()
  for (i in 1:length(bootstraps)) {

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
              by = join_by(year, taxonKey, scientificName),
              relationship = "many-to-many")

  return(indicator)

  } else {

    # Calculate total occurrences for each species by grid cell
    indicator <-
      x %>%
      dplyr::mutate(diversity_val = sum(obs), .by = c(taxonKey, year)) %>%
      dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
      dplyr::arrange(year) %>%
      dplyr::select(year, taxonKey, scientificName, diversity_val)

  }

}

#' @export
#' @rdname calc_ts
calc_ts.spec_range <- function(x,
                               indicator=indicator,
                               bootstrap=FALSE,
                               num_bootstrap=1000,
                               ci_type = ci_type,
                               ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_range"))

  x <-
    x %>%
    dplyr::arrange(taxonKey, year, cellCode)

  if (bootstrap==TRUE) {

    # Bootstrap species range values
    bootstraps <-
      x %>%
      dplyr::summarize(observed = sum(obs >= 1), .by = c(taxonKey, year, cellCode)) %>%
      dplyr::group_by(taxonKey) %>%
      dplyr::group_split() %>%
      purrr::map(. %>%
                   dplyr::select(year,observed,cellCode) %>%
                   dplyr::arrange(year) %>%
                   tidyr::pivot_wider(names_from = year, values_from = observed) %>%
                   replace(is.na(.),0) %>%
                   tibble::column_to_rownames("cellCode") %>%
                   as.list() %>%
                   purrr::map(. %>%
                                boot::boot(
                                  data = .,
                                  statistic = boot_statistic_sum,
                                  R = num_bootstrap
                                )
                   )
      )

    taxkeys <- unique(x$taxonKey)
    scinames <- unique(x$scientificName)

    # Calculate confidence intervals
    ci_df_list <- list()
    for (i in 1:length(bootstraps)) {

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
                by = join_by(year, taxonKey, scientificName),
                relationship = "many-to-many")

    return(indicator)

  } else {

  # Flatten occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = sum(obs >= 1), .by = c(taxonKey, year)) %>%
    dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(taxonKey) %>%
    dplyr::select(year, taxonKey, scientificName, diversity_val)

  }

}

#' @param set_rows Automatically select which taxonomic information to keep when
#'    there are multiple options. Default value of 1 keeps the first option, which
#'    is usually the best.
#' @export
#' @rdname calc_ts
calc_ts.tax_distinct <- function(x,
                                 set_rows = 1,
                                 indicator=indicator,
                                 bootstrap=FALSE,
                                 num_bootstrap=1000,
                                 ci_type = ci_type,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "tax_distinct"))


  if (bootstrap==TRUE) {

    # read data saved during the initial indicator calculation
    tax_hier <- readRDS("taxonomic_hierarchy.RDS")

    x <-
      x %>%
      dplyr::arrange(year)

    # organize data
    x2 <-
      x %>%
      tibble::add_column(diversity_val = NA) %>%
      dplyr::group_split(year)

    x3 <- lapply(x2, function(y) {
      a <- y$scientificName
    })

    names(x3) <- lapply(x2, function(y) {
      a <- y$year[1]
    })


    # Bootstrap indicator value
    bootstraps <-
      x3 %>%
      purrr::map(. %>%
                 boot::boot(
                   data = .,
                   statistic = boot_statistic_td,
                   R = num_bootstrap
                 ))

    # Replace NA values to avoid errors when calculating confidence intervals
    bootstraps <- lapply(bootstraps, ci_error_prevent)

    names(bootstraps) <- unique(x$year)

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    if (length(ci_df) > 0) {

      # Convert negative values to zero as rarity cannot be less than zero
      ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

      # Join confidence intervals to indicator values by year
      indicator <- indicator %>%
        dplyr::full_join(ci_df,
                  by = join_by(year),
                  relationship = "many-to-many")

    } else {

      warning("Unable to calculate confidence intervals. There may be insufficient data.")

    }

    return(indicator)

  } else {

    # Retrieve taxonomic data from GBIF
    tax_hier <- taxize::classification(unique(x$scientificName),
                                       db = "gbif",
                                       ...)

    # Save data for use when calculating bootstraps
    saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

    # tax_hier <- readRDS("taxonomic_hierarchy.RDS")

    # Calculate taxonomic distinctness
    indicator <-
      x %>%
      tibble::add_column(diversity_val = NA) %>%
      dplyr::group_split(year) %>%
      purrr::map(. %>%
                   dplyr::mutate(diversity_val =
                                   compute_tax_distinct_formula(.,
                                                                tax_hier))) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(year, diversity_val, .keep_all = TRUE) %>%
      dplyr::select(year, diversity_val)

    return(indicator)

  }

}


#' @export
#' @rdname calc_ts
calc_ts.occ_turnover <- function(x,
                                 indicator = NULL,
                                 bootstrap = FALSE,
                                 num_bootstrap = 1000,
                                 ci_type = ci_type,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_turnover"))

  x <- x %>%
    dplyr::arrange(year)

  # Determine the species present each year
  ind_list <- list_org_by_year(x, "taxonKey")

  if (bootstrap==TRUE) {


    # Calculate bootstraps from tax_list
    bootstraps <- multiyear_bootstrap(tax_list,
                                      indicator = indicator,
                                      num_bootstrap = num_bootstrap)

    # Replace NA values to avoid errors when calculating confidence intervals
    bootstraps <- lapply(bootstraps, ci_error_prevent)

    # Name bootstrap output list items by year
    bootstraps <- add_yearvals_to_boot(bootstraps, x)

    # Calculate confidence intervals
    ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                by = join_by(year),
                relationship = "many-to-many")

    return(indicator)

  } else {

    # Determine the new species added each year
    tax_added <- list()
    tax_added[[1]] <- tax_list[[1]]
    tax_added[2:length(tax_list)] <-
      lapply(2:length(unique(x$year)), function(y){
        a <- setdiff(tax_list[[y]], tax_list[[y-1]])
        return(a)
      })

    # Determine the species lost each year
    tax_lost <- list()
    tax_lost[[1]] <- NULL
    tax_lost[2:length(tax_list)] <-
      lapply(2:length(unique(x$year)), function(y){
        a <- setdiff(tax_list[[y-1]], tax_list[[y]])
      })

    # Combine the species present in the current with those present in the previous year
    tax_present <- list()
    tax_present[[1]] <- tax_list[[1]]
    tax_present[2:length(tax_list)] <-
      lapply(2:length(unique(x$year)), function(y){
        a <- intersect(tax_list[[y-1]], tax_list[[y]])
      })

    # Calculate occupancy turnover as the sum of the number of species added and the
    # number of species lost divided by the total number of species present in the current
    # and previous year combined
    occ_turnover <- sapply(1:length(unique(x$year)), function(y){
      a <- (length(tax_added[[y]]) + length(tax_lost[[y]])) /
        (length(tax_present[[y]]) + length(tax_added[[y]]) + length(tax_lost[[y]]))
    })
    occ_turnover[[1]] <- NA

    indicator <- tibble::tibble(year = unique(x$year), diversity_val = occ_turnover)

    return(indicator)

  }

}
