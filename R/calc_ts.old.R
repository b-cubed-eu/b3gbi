


#' @title Calculate a Biodiversity Indicator Time Series
#'
#' @description  Calculates various biodiversity indicators over time, based on
#'   species occurrence data provided in a 'processed_cube', 'processed_cube_dsinfo',
#'   or 'virtual_cube' object.
#'
#' @param x An object of class 'processed_cube', 'processed_cube_dsinfo', or
#'   'virtual_cube' containing species occurrence data.
#' @param type  The type of biodiversity indicator to calculate. Supported types:
#'   * 'obs_richness': Observed species richness
#'   * 'cum_richness': Cumulative species richness
#'   * 'total_occ': Total number of occurrences
#'   * 'occ_by_type': Occurrences segregated by observation type
#'   * 'occ_by_dataset': Occurrences segregated by dataset
#'   * 'rarefied': Rarefied species richness
#'   * 'hill0', 'hill1', 'hill2': Hill numbers (order 0, 1, 2)
#'   * 'e9_evenness', 'pielou_evenness': Evenness indices
#' @param inext_sampsize (For 'hill0', 'hill1', 'hill2') Sample size
#'   for rarefaction/extrapolation.
#' @param coverage (For 'hill0', 'hill1', 'hill2') Coverage level for extrapolation.
#' @param level (Optional) Geographic level for spatial calculations
#'    ("country", "continent", or "world").
#' @param region (Optional) Geographic region for spatial calculations
#'    (e.g., "Germany" or "Europe").
#' @param cutoff_length (For rarefaction and Hill number calculations) Minimum
#'    number of records for a year to be included.
#'
#' @return An object of class 'indicator_ts' containing the calculated
#'   time series data.
#'
#' @examples
#' # Assuming you have a 'processed_cube' object named 'my_data'
#' diversity_timeseries <- calc_ts(my_data, type = "obs_richness")
#'
#' @export
calc_ts.default <- function(x,
                    type = "obs_richness",
                    inext_sampsize = 150,
                    coverage = 0.95,
                    level = NULL,
                    region = NULL,
                    cutoff_length = 100) {

  stopifnot_error("Object class not recognized.",
                  inherits(x, "processed_cube") |
                    inherits(x, "processed_cube_dsinfo") |
                    inherits(x, "virtual_cube"))

  type <- match.arg(type,
                    c(available_indicators$indicator_class))

  data <- x$data

  # Collect information to add to final object
  num_species <- x$num_species
  first_year <- x$first_year
  last_year <- x$last_year
  num_years <- length(unique(data$year))
  year_names <- unique(data$year)
  map_lims <- unlist(list("xmin" = min(data$xcoord),
                          "xmax" = max(data$xcoord),
                          "ymin" = min(data$ycoord),
                          "ymax" = max(data$ycoord)))

  if(!inherits(x, "virtual_cube")) {

    kingdoms <- x$kingdoms
    species_names <- unique(data$scientificName)

  }

  if (!is.null(level) & !is.null(region)) {

    # Download Natural Earth data
    map_data <- get_NE_data(level, region)

    # Create grid from Natural Earth data
    grid <- create_grid(map_data, cs1, cs2)

    # Format spatial data and merge with grid
    data <- prepare_spatial_data(data, grid)

  } else {

    level <- "unknown"
    region <- "unknown"

  }

  if (type == "occ_by_type") {

    # Calculate total number of observations over the grid
    indicator <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "basisOfRecord")) %>%
      dplyr::rename(type = basisOfRecord)


  } else if (type == "occ_by_dataset") {

    # Calculate total number of observations over the grid
    indicator <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "datasetKey")) %>%
      dplyr::rename(type = datasetKey)

  } else if (type == "rarefied") {

    # Calculate species richness and total records by year
    richness_by_year <-
      data %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(total_records = sum(obs),
                       obs_richness = n_distinct(scientificName),
                       .groups = "drop")

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
      furrr::future_map(specaccum_int, type = "rarefaction")

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
    diversity_ts <-
      richness_by_year %>%
      tibble::add_column(rarefied_richness = unlist(rarefied_richness),
                         .after = "obs_richness")

    # Calculate estimated relative richness as an index
    diversity_ts <-
      diversity_ts %>%
      dplyr::mutate(diversity_val = rarefied_richness/lag(rarefied_richness)) %>%
      replace(is.na(.), 1) %>%
      dplyr::mutate(diversity_val = cumprod(diversity_val))

    # remove rows which are not present in estimated richness
    indicator <- diversity_ts[diversity_ts$year %in% diversity_ts$year,]

  } else if (type == "species_rarity") {

    # Calculate rarity for each species by year
    indicator <-
      data %>%
      dplyr::mutate(records_year = sum(obs), .by = year) %>%
      dplyr::mutate(records_taxon = sum(obs), .by = c(year, taxonKey)) %>%
      dplyr::mutate(diversity_val = 1 / (records_taxon / records_year)) %>%
      dplyr::distinct(year, taxonKey, .keep_all = TRUE) %>%
      dplyr::select(year, taxonKey, scientificName, diversity_val)

  } else {

    indicator <- calc_ts(data, ...)
  }



  diversity_obj <- new_indicator_ts(as_tibble(indicator),
                                div_type = type,
                                kingdoms = kingdoms,
                                num_species = num_species,
                                num_years = num_years,
                                species_names = species_names,
                                map_level = level,
                                map_region = region,
                                coord_range = map_lims)

  return(diversity_obj)

}




#' @noRd
compute_indicator_workflow.ts <- function(x,
                                          type,
                                          cell_size = NULL,
                                          level = c("continent", "country", "world"),
                                          region = "Europe",
                                          ...) {

  stopifnot_error("Object class not recognized.",
                  inherits(x, "processed_cube") |
                    inherits(x, "processed_cube_dsinfo") |
                    inherits(x, "virtual_cube"))

  level <- match.arg(level)
  type <- match.arg(type,
                    names(available_indicators))

  data <- x$data

  # Collect information to add to final object
  num_species <- x$num_species
  first_year <- x$first_year
  last_year <- x$last_year
  num_years <- length(unique(data$year))

  if (!inherits(x, "virtual_cube")) {

    kingdoms <- x$kingdoms
    species_names <- unique(data$scientificName)
    years_with_obs <- unique(data$year)

  }

  if (!is.null(level) & !is.null(region)) {

    # Download Natural Earth data
    map_data <- get_NE_data(level, region)

    # Create grid from Natural Earth data
    grid <- create_grid(map_data, level, cell_size)

    # Format spatial data and merge with grid
    data <- prepare_spatial_data(data, grid)

  } else {
    level <- "unknown"
    region <- "unknown"
  }

  # Assign class to send data to correct calculator function
  class(data) <- append(type, class(data))

  # Calculate indicator
  indicator <- calc_ts(data, ...)


  # Create indicator object
  if (!inherits(x, "virtual_cube")) {

    diversity_obj <- new_indicator_ts(indicator,
                                      div_type = type,
                                      map_level = level,
                                      map_region = region,
                                      kingdoms = kingdoms,
                                      num_species = num_species,
                                      first_year = first_year,
                                      last_year = last_year,
                                      num_years = num_years,
                                      species_names = species_names,
                                      years_with_obs = years_with_obs)

  } else {

    diversity_obj <- new_virtual_indicator_ts(indicator,
                                              div_type = type,
                                              cell_size = cell_size,
                                              map_level = level,
                                              map_region = region,
                                              num_species = num_species,
                                              first_year = first_year,
                                              last_year = last_year,
                                              num_years = num_years)

  }

  return(diversity_obj)

}
