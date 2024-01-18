#' Calculate species richness trend
#'
#' @param data A tibble created from a GBIF cube using the process_cube function.
#' @param type A character vector.
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
calc_ts.default <- function(x,
                    type = "obs_richness",
                    inext_sampsize = 150,
                    coverage = 0.95,
                    level = NULL,
                    region = NULL,
                    cutoff_length = 100) {

  stopifnot_error("Object class not recognized.",
                  inherits(x, "processed_cube") |
                    inherits(x, "processed_cube_dsinfo"))

  data <- x$data

  # Collect information to add to final object
  kingdoms <- x$kingdoms
  species_names <- unique(data$scientificName)
  num_species <- x$num_species
  first_year <- x$first_year
  last_year <- x$last_year
  num_years <- length(unique(data$year))
  year_names <- unique(data$year)
  map_lims <- unlist(list("xmin" = min(data$xcoord),
                          "xmax" = max(data$xcoord),
                          "ymin" = min(data$ycoord),
                          "ymax" = max(data$ycoord)))

  if (type == "e9_evenness") {
    calc_evenness <- calc_e9_evenness
  } else if (type == "pielou_evenness") {
    calc_evenness <- calc_pielou_evenness
  }

  if (!is.null(level) & !is.null(region)) {


  # Download and prepare Natural Earth map data
  if (level == "country") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            country = region) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  } else if (level == "continent") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            continent = region) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  } else if (level == "world") {

    map_data <- rnaturalearth::ne_countries(scale = "medium") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  }

  # Make a grid across the map area
  grid <- map_data %>%
    sf::st_make_grid(cellsize = c(cs1 * 1000, cs2 * 1000)) %>%
    sf::st_intersection(map_data) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = row_number())

  # Add area column to grid
  grid$area_km2 <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  # Set map limits
  map_lims <- sf::st_buffer(grid, dist = 1000) %>%
    sf::st_bbox()

  # Scale coordinates of occurrences so the number of digits matches map
  data_scaled <-
    data %>%
    dplyr::mutate(xcoord = xcoord * 1000,
                  ycoord = ycoord * 1000)

  # Convert the x and y columns to the correct format for plotting with sf
  occ_sf <- sf::st_as_sf(data_scaled,
                         coords = c("xcoord", "ycoord"),
                         crs = "EPSG:3035")

  # Set attributes as spatially constant to avoid warnings
  sf::st_agr(grid) <- "constant"
  sf::st_agr(occ_sf) <- "constant"

  # Calculate intersection between occurrences and grid cells
  occ_grid_int <- sf::st_intersection(occ_sf, grid, left = TRUE)

  # Add cell numbers to occurrence data
  data_cell <-
    data_scaled %>%
    dplyr::inner_join(occ_grid_int) %>%
    suppressMessages() %>%
    dplyr::arrange(cellid)

  data <- data_cell

  } else {
    level <- "unknown"
    region <- "unknown"
  }

  # Calculate species richness and total records by year
  richness_by_year <-
    data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total_records = sum(obs),
                     obs_richness = n_distinct(scientificName),
                     .groups = "drop")

  if (type == "obs_richness") {

    diversity_ts <-
      richness_by_year %>%
      dplyr::rename(diversity_val = obs_richness)

   } else if (type == "cum_richness") {

      diversity_ts <-
        data %>%
        dplyr::select(year, taxonKey) %>%
        dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
        dplyr::summarize(unique_by_year = length(unique(taxonKey)),
                         .by = year) %>%
        dplyr::reframe(year = year,
                       diversity_val = cumsum(unique_by_year))

  } else if (type == "total_occ") {

    # Calculate total number of occurrences over the grid
    diversity_ts <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = "year")

  } else if (type == "occ_by_type") {

    # Calculate total number of observations over the grid
    diversity_ts <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "basisOfRecord")) %>%
      dplyr::rename(type = basisOfRecord)


  } else if (type == "occ_by_dataset") {

    # Calculate total number of observations over the grid
    diversity_ts <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = c("year", "datasetKey")) %>%
      dplyr::rename(type = datasetKey)

  } else if (type == "rarefied") {

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
    diversity_ts <- diversity_ts[diversity_ts$year %in% diversity_ts$year,]

  }

  else if (type %in% c("hill0", "hill1", "hill2")) {

    # Extract qvalue from hill diversity type
    qval <- as.numeric(gsub("hill", "", type))

    # Create list of occurrence matrices by year, with species as rows
    species_records_raw <-
      data %>%
      dplyr::group_by(year) %>%
      dplyr::group_split() %>%
      purrr::map(. %>%
                   dplyr::group_by(eea_cell_code,
                                   scientificName) %>% {. ->> temp1 } %>%
                   tidyr::pivot_wider(names_from = "scientificName",
                                      values_from = "obs") %>%
                   dplyr::ungroup() %>%
                   dplyr::select(-eea_cell_code,
                                 -taxonKey,
                                 -kingdom,
                                 -rank,
                                 -resolution,
                                 -geometry) %>%
                   dplyr::select(-any_of(c("area_km2",
                                           "xcoord",
                                           "ycoord",
                                           "basisOfRecord",
                                           "datasetKey"))) %>%{. ->> temp2 } %>%
                   replace(is.na(.), 0) %>%
                   dplyr::mutate_if(is.numeric,
                                    as.integer) %>%
                   dplyr::select(-year,
                                 -cellid) %>%
                   tibble::rownames_to_column() %>%
                   tidyr::gather(variable,
                                 value,
                                 -rowname) %>%
                   tidyr::spread(rowname, value) %>%{. ->> temp4 } %>%
                   'row.names<-'(., NULL) %>%
                   tibble::column_to_rownames(var = "variable") %>%
                   as.matrix() %>%
                   ifelse(. > 1, 1, .))

    # name list elements
    names(species_records_raw) <- richness_by_year$year

    # remove all years with too little data to avoid errors from iNEXT
    species_records_raw2 <- species_records_raw %>%
      keep(., function(x) length(x) > cutoff_length)

    # Calculate diversity estimates
    #  coverage_rare <- species_records_raw %>%
    #   iNEXT::iNEXT(endpoint=inext_sampsize, datatype="incidence_raw", q=qval)

    coverage_rare <- species_records_raw2 %>%
      iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

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
      dplyr::mutate(index = est_relative_richness/lag(est_relative_richness)) %>%
      replace(is.na(.), 1) %>%
      dplyr::mutate(index = cumprod(index))

    # remove rows which are not present in estimated richness
    richness_by_year <- richness_by_year[richness_by_year$year %in% est_richness$year,]


    # Add observed richness and total records to df
    diversity_ts <-
      richness_by_year %>%
      tibble::add_column(est_relative_richness = est_richness$est_relative_richness,
                 .after = "obs_richness") %>%
      tibble::add_column(diversity_val = est_richness$index,
                 .after = "est_relative_richness") %>%
      as_tibble

  } else if (type == "e9_evenness" | type == "pielou_evenness") {


    # Calculate number of records for each species by grid cell
    diversity_ts <-
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
                    .keep = "unused")


  } else if (type == "species_rarity") {

    # Calculate rarity for each species by year
    diversity_ts <-
      data %>%
      dplyr::mutate(records_year = sum(obs), .by = year) %>%
      dplyr::mutate(records_taxon = sum(obs), .by = c(year, taxonKey)) %>%
      dplyr::mutate(diversity_val = 1 / (records_taxon / records_year)) %>%
      dplyr::distinct(year, taxonKey, .keep_all = TRUE) %>%
      dplyr::select(year, taxonKey, scientificName, diversity_val)

  }

  diversity_obj <- indicator_ts(as_tibble(diversity_ts),
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
