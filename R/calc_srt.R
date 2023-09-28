#' Calculate species richness trend
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param method A character vector, "raw", "total_records", or "rarefaction".
#'   "raw" calculates an unadjusted richness trend from the cube data,
#'   "total_records" uses the total number of occurrences for each year as a
#'   proxy for sampling effort.
#'   "rarefaction" calculates rarefaction curves to approximate sampling effort
#'
#' @return A tibble
#' @export
#'
#' @examples
#' calc_srt(processed_cube, "rarefaction")
calc_srt <- function(data, method) {

  # Calculate species richness by year
  richness_by_year <-
    merged_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(species_richness = n_distinct(scientificName), .groups = "drop")

  if (method == "raw") {

    return(richness_by_year)

  } else if (method == "total_records") {

    # Adjust species richness using total records as proxy for sampling effort
    richness_by_year <-
      richness_by_year %>%
      dplyr::left_join(merged_data %>%
                  dplyr::group_by(year) %>%
                  dplyr::summarise(total_records = sum(obs), .groups = "drop"),
                by = "year") %>%
      dplyr::mutate(adjusted_richness = species_richness / total_records * 500)

  } else if (method == "rarefaction") {

    # Calculate number of records for each species by year
    species_records <-
      merged_data %>%
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
    interpolated_richness <-
      spec_rare %>%
      purrr::map(~stats::approx(.$sites, .$richness, xout = sampling_effort)$y)

    # Calculate mean rarefied richness for each year
    mean_rarefied_richness <-
      interpolated_richness %>%
      purrr::map(mean, na.rm=TRUE)

    # Calculate adjusted species richness by year
    richness_by_year <-
      richness_by_year %>%
      tibble::add_column(mean_rarefied_richness = unlist(mean_rarefied_richness)) %>%
      dplyr::mutate(adjusted_richness = species_richness - mean_rarefied_richness)

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
      merged_data %>%
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

    # Calculate diversity estimates
    coverage_rare <- species_records_raw %>%
      iNEXT(endpoint=max_sampsize, datatype="incidence_raw")

    # Split observed richness values into a list
    richness_by_year_list <- split(richness_by_year,
                                   seq_len(nrow(richness_by_year)))

    # Extract species richness values standardized to equal coverage
    inext_results_df <-
      map(1:72, function(iteration_value) {
        inext_rich_vals <-
          coverage_rare$iNextEst$coverage_based %>%
          filter(Assemblage == paste0("assemblage", iteration_value)) %>%
          {. ->> test1 } %>%
          add_column(diff = abs(.$t - (
            richness_by_year_list[[iteration_value]]$"species_richness"
            ))) %>%
          slice_min(diff) %>%
          slice_head(n = 1) %>%
        {. ->> test2 }
        if (nrow(inext_rich_vals) > 0 & !is.na(inext_rich_vals$qD)) {
          qD_closest <- inext_rich_vals$qD
        } else {
          qD_closest <- NA
        }
        data.frame(Year = richness_by_year_list[[iteration_value]]$"year",
                   qD_Closest = qD_closest)
      }) %>%
      bind_rows


  }

}



