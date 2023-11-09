#' Calculate species richness trend
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param method A character vector.
#'   "observed" calculates the observed species richness trend from the cube data,
#'   "total_records" uses the total number of occurrences for each year as a
#'   proxy for sampling effort.
#'   "rarefaction" calculates rarefaction curves to approximate sampling effort,
#'   "coverage" estimates relative richness trends by standardizing by coverage,
#'   "evenness" calculates evenness
#'
#' @return A tibble
#' @export
#'
#' @examples
#' calc_srt(processed_cube, "rarefaction")
calc_ts <- function(data, method = "observed", qval = 0, inext_sampsize = 150, coverage = 0.9) {

  # Put year names into a vector
  year_names <- unique(data$year)

  # Calculate species richness and total records by year
  richness_by_year <-
    data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total_records = sum(obs),
                     obs_richness = n_distinct(scientificName),
                     .groups = "drop")

  if (method == "observed") {

    richness_by_year_obs <-
      richness_by_year %>%
      dplyr::rename(diversity_val = obs_richness) %>%
      tibble::add_column(diversity_type = c("obs_richness"))

    return(richness_by_year_obs)

   } else if (method == "cumulative") {

      cumulative_richness <-
        data %>%
        dplyr::select(year, taxonKey) %>%
        dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
        dplyr::summarize(unique_by_year = length(unique(taxonKey)),
                         .by = year) %>%
        dplyr::reframe(year = year,
                       diversity_val = cumsum(unique_by_year)) %>%
        tibble::add_column(diversity_type = c("cum_richness"))

      return(cumulative_richness)

  } else if (method == "total_records") {

    # Adjust species richness using total records as proxy for sampling effort
    total_records_df <-
      richness_by_year %>%
      dplyr::mutate(diversity_val = (obs_richness / total_records * 500),
                        .after = "obs_richness") %>%
      tibble::add_column(diversity_type = c("total_records"))

  } else if (method == "total_obs") {

    # Calculate total number of observations over the grid
    obs_year <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = "year") %>%
      tibble::add_column(diversity_type = c("total_obs"))

  } else if (method == "occ_by_type") {

    # Calculate total number of observations over the grid
    occ_by_type <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "dataType")) %>%
      dplyr::rename(type = dataType) %>%
      tibble::add_column(diversity_type = c("occ_by_type"))

  } else if (method == "occ_by_dataset") {

    # Calculate total number of observations over the grid
    occ_by_dataset <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "datasetName")) %>%
      dplyr::rename(type = datasetName) %>%
      tibble::add_column(diversity_type = c("occ_by_dataset"))

  } else if (method == "rarefaction") {

    # Calculate number of records for each species by year
    species_records <-
      data %>%
      dplyr::group_by(year) %>%
      dplyr::group_split() %>%
      purrr::map(. %>%
                   dplyr::group_by(eea_cell_code, scientificName) %>%
                   dplyr::summarise(spec_rec = sum(obs), .groups = "drop") %>%
                   tidyr::pivot_wider(names_from = scientificName, values_from = spec_rec) %>%
                   dplyr::select(-eea_cell_code) %>%
                   replace(is.na(.), 0)
      )

    # Calculate rarefaction curves for each year
    future::plan(multisession)
    spec_rare <- species_records %>%
      furrr::future_map(vegan::specaccum, method = "rarefaction")

    # Get the sampling effort level at which to interpolate
    sampling_effort <-
      spec_rare %>%
      purrr::map(~max(.$individuals)) %>%
      unlist() %>%
      min()

    # Interpolate richness at different sampling effort levels
    rarefied_richness <-
      spec_rare %>%
      purrr::map(~stats::approx(.$sites, .$richness, xout = sampling_effort)$y)

    # Calculate adjusted species richness by year
    rarefied_df <-
      richness_by_year %>%
      tibble::add_column(diversity_val = unlist(rarefied_richness),
                         .after = "obs_richness") %>%
      tibble::add_column(diversity_type = c("rarefied"))

  }

  # else if (method == "gam") {
  #
  #   df_richness_gam <- cube_species_richness %>%
  #     tibble::add_column(sampling_effort = unlist(sampling_effort_levels)) %>%
  #     dplyr::select(-adjusted_richness_gam, -adjusted_richness, -mean_rarefied_richness)
  #
  #   # Fit a GAM to species richness
  #   gam_model <- mgcv::gam(species_richness ~ s(sampling_effort, by = year),
  #                    data = df_richness_gam)
  #
  #   # Predicted adjusted richness from the GAM
  #   adjusted_richness_gam = mgcv::predict(gam_model, newdata = df_richness_gam)
  #
  #   # Add adjusted richness to species richness tibble
  #   df_richness_gam <-
  #     df_richness_gam %>%
  #     tibble::add_column(adjusted_richness_gam = adjusted_richness_gam)
  #
  # }

  else if (method == "coverage") {

    # Create list of occurrence matrices by year, with species as rows
    species_records_raw <-
      data %>%
      dplyr::group_by(year) %>%
      dplyr::group_split() %>%
      purrr::map(. %>%
                   dplyr::group_by(eea_cell_code,
                                   scientificName) %>%
                   tidyr::pivot_wider(names_from = scientificName,
                                      values_from = obs) %>%
                   ungroup() %>%
                   dplyr::select(-eea_cell_code,
                                 -taxonKey,
                                 -kingdom,
                                 -rank,
                                 -xcoord,
                                 -ycoord,
                                 -resolution) %>%
                   replace(is.na(.), 0) %>%
                   mutate_if(is.numeric,
                             as.integer) %>%
                   select(-year) %>%
                   rownames_to_column %>%
                   gather(variable,
                          value,
                          -rowname) %>%
                   spread(rowname, value) %>%
                   'row.names<-'(., NULL) %>%
                   column_to_rownames(var = "variable") %>%
                   as.matrix() %>%
                   ifelse(. > 1, 1, .))

    # name list elements
    names(species_records_raw) <- richness_by_year$year

    # Calculate diversity estimates
  #  coverage_rare <- species_records_raw %>%
   #   iNEXT(endpoint=inext_sampsize, datatype="incidence_raw", q=qval)

    coverage_rare <- species_records_raw %>%
      estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

    # Extract estimated relative species richness
    est_richness <-
      coverage_rare %>%
    #  coverage_rare$iNextEst$coverage_based %>%
    #  dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
    #                .by = Assemblage) %>%
      dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
      dplyr::rename(year = Assemblage,
                    est_relative_richness = qD,
                    samp_size_est = t,
                    coverage = SC,
                    diversity_type = Order.q)

    # Calculate estimated relative richness as an index
    est_richness <-
      est_richness %>%
      mutate(index = est_relative_richness/lag(est_relative_richness)) %>%
      replace(is.na(.), 1) %>%
      mutate(index = cumprod(index))

    # Add observed richness and total records to df
    coverage_df <-
      richness_by_year %>%
      add_column(est_relative_richness = est_richness$est_relative_richness,
                 .after = "obs_richness") %>%
      add_column(diversity_val = est_richness$index,
                 .after = "est_relative_richness") %>%
      tibble::add_column(diversity_type = est_richness$diversity_type) %>%
      as_tibble

  } else if (method=="evenness") {


    # Calculate number of records for each species by grid cell
    evenness <-
      data %>%
      dplyr::summarize(num_occ = sum(obs),
                       .by = c(year, taxonKey)) %>%
      dplyr::arrange(year) %>%
      tidyr::pivot_wider(names_from = year,
                         values_from = num_occ) %>%
      replace(is.na(.), 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      purrr::map(~calc_evenness(.)) %>%
      unlist() %>%
      as.data.frame() %>%
      dplyr::rename(diversity_val = ".") %>%
      tibble::rownames_to_column(var = "year") %>%
      dplyr::mutate(year = as.integer(year),
                    .keep = "unused") %>%
      tibble::add_column(diversity_type = c("evenness"),
                         .after = "year")

    }

}



