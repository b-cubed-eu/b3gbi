#' @title Calculate a Biodiversity Indicator Map
#'
#' @description Calculates various spatial biodiversity indicators based on the
#' provided data. Supported spatial indicators include species diversity, evenness,
#' rarity, and more.
#'
#' @param data  An 'sf' object containing gridded species occurrences.
#' @param type  The type of biodiversity indicator to calculate. Supported types:
#'   * 'hill0', 'hill1', 'hill2': Hill numbers (order 0, 1, 2)
#'   * 'obs_richness': Observed species richness
#'   * 'total_occ': Total number of occurrences
#'   * 'newness': Mean year of occurrence
#'   * 'density': Density of occurrences (occurrences per square kilometer)
#'   * 'e9_evenness', 'pielou_evenness': Evenness indices
#'   * 'ab_rarity': Abundance-based rarity
#'   * 'area_rarity': Area-based rarity
#'   * 'spec_occ': Total occurrences per species in each grid cell
#'   * 'spec_range': Range size of each species (number of occupied grid cells)
#'   * 'tax_distinct': Taxonomic distinctness
#' @param ... Additional arguments (potentially used for specific indicator types).
#'
#' @return A data frame containing calculated indicator values for each grid cell,
#'   along with cell identifiers.
#'
#' @examples
#' # Assuming you have a data frame 'biodiversity_data' with spatial information
#' richness_map <- calc_map(biodiversity_data, type = "obs_richness")
#'
#' @export
calc_map.default <- function(data,
                     type,
                     ...) {

  type <- match.arg(type,
                    c(available_indicators$indicator_class))


  if (type == "williams_evenness") {
    compute_formula_evenness <- compute_formula_williams_evenness
  } else if (type == "pielou_evenness") {
    compute_formula_evenness <- compute_formula_pielou_evenness
  }


  if (type %in% c("hill0", "hill1", "hill2")) {

    # Extract qvalue from hill diversity type
    qval <- as.numeric(gsub("hill", "", type))

    # Create list of occurrence matrices by grid cell, with species as rows
    spec_rec_raw_cell <-
      data %>%
      dplyr::group_split(cellid) %>%
      purrr::map(. %>%
                   dplyr::group_by(eea_cell_code,
                                   taxonKey) %>%
                   tidyr::pivot_wider(names_from = taxonKey,
                                      values_from = obs) %>%
                   dplyr::ungroup() %>%
                   dplyr::select(-eea_cell_code,
                                 -scientificName,
                                 -kingdom,
                                 -rank,
                                 -geometry,
                                 -resolution,
                                 -xcoord,
                                 -ycoord,
                                 -year,
                                 -area_km2) %>%
                   dplyr::select(-any_of(c("basisOfRecord",
                                           "datasetKey"))) %>%
                   replace(is.na(.), 0) %>%
                   dplyr::mutate_if(is.numeric,
                                    as.integer) %>%
                   dplyr::select(-cellid) %>%
                   tibble::rownames_to_column() %>%
                   tidyr::gather(variable,
                                 value,
                                 -rowname) %>%
                   tidyr::spread(rowname, value) %>%
                   'row.names<-'(., NULL) %>%
                   tibble::column_to_rownames(var = "variable") %>%
                   as.matrix() %>%
                   replace(. > 1, as.integer(1))
      )


    # name list elements
    names(spec_rec_raw_cell) <- unique(data$cellid)

    # remove all cells with too little data to avoid errors from iNEXT
    spec_rec_raw_cell2 <- spec_rec_raw_cell %>%
      keep(., function(x) length(x) > cutoff_length)

    # Calculate diversity estimates
    # coverage_rare_cell <- spec_rec_raw_cell2 %>%
    #   iNEXT::iNEXT(endpoint=inext_sampsize, knots=knots, datatype="incidence_raw", q=qval)

    coverage_rare_cell <- spec_rec_raw_cell2 %>%
      iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

    # Extract estimated relative diversity
    diversity_cell <-
      coverage_rare_cell %>%
      #coverage_rare_cell$iNextEst$coverage_based %>%
      #dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
      #              .by = Assemblage) %>%
      dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
      dplyr::rename(cellid = Assemblage,
                    diversity_val = qD,
                    samp_size_est = t,
                    coverage = SC,
                    diversity_type = Order.q) %>%
      dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")

  } else if (type == "obs_richness") {

    # Calculate species richness over the grid
    diversity_cell <-
      data %>%
      dplyr::summarize(diversity_val = sum(dplyr::n_distinct(taxonKey)),
                       .by = "cellid")

  } else if (type == "total_occ") {

    # Calculate total number of occurrences over the grid
    diversity_cell <-
      data %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = "cellid")

  } else if (type == "newness") {

    # Calculate mean year of occurrence over the grid
    diversity_cell <-
      data %>%
      dplyr::summarize(diversity_val = round(mean(year)),
                       .by = "cellid")

    if (!is.null(newness_min_year)) {
      diversity_cell$diversity_val <- ifelse(diversity_cell$diversity_val > newness_min_year,
                                             diversity_cell$diversity_val,
                                             NA)
    }

  } else if (type == "density") {

    # Calculate density of occurrences over the grid (per square km)
    diversity_cell <-
      data %>%
      dplyr::reframe(diversity_val = sum(obs) / area_km2,
                     .by = "cellid") %>%
      dplyr::distinct(cellid, diversity_val) %>%
      dplyr::mutate(diversity_val = as.numeric(diversity_val))

  } else if (type == "williams_evenness" | type == "pielou_evenness") {

    # Calculate adjusted evenness fo r each grid cell
    diversity_cell <-
      data %>%
      dplyr::summarize(num_occ = sum(obs),
                       .by = c(cellid, taxonKey)) %>%
      dplyr::arrange(cellid) %>%
      tidyr::pivot_wider(names_from = cellid,
                         values_from = num_occ) %>%
      replace(is.na(.), 0) %>%
      tibble::column_to_rownames("taxonKey") %>%
      purrr::map(~calc_evenness(.)) %>%
      unlist() %>%
      as.data.frame() %>%
      dplyr::rename(diversity_val = ".") %>%
      tibble::rownames_to_column(var = "cellid") %>%
      dplyr::mutate(cellid = as.integer(cellid),
                    .keep = "unused")

  } else if (type == "ab_rarity") {

    # Calculate total summed rarity (in terms of abundance) for each grid cell
    diversity_cell <-
      data %>%
      dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
      dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
      dplyr::summarise(diversity_val = sum(rarity), .by = "cellid") %>%
      dplyr::arrange(cellid)

  } else if (type == "area_rarity") {

    # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
    # frequency for each species
    diversity_cell <-
      data %>%
      dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                    .by = c(taxonKey)) %>%
      dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
      dplyr::summarise(diversity_val = sum(rarity), .by = cellid)

  } else if (type == "spec_occ") {

    # Calculate total occurrences for each species by grid cell
    diversity_cell <-
      data %>%
      dplyr::mutate(num_records = sum(obs), .by = c(taxonKey, cellid)) %>%
      dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
      dplyr::arrange(cellid) %>%
      dplyr::select(cellid, taxonKey, scientificName, num_records)

  } else if (type == "spec_range") {

    # Flatten occurrences for each species by grid cell
    diversity_cell <-
      data %>%
      dplyr::mutate(obs = 1) %>%
      dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
      dplyr::arrange(cellid) %>%
      dplyr::select(cellid, taxonKey, scientificName, obs)

  } else if (type == "tax_distinct") {

    # Retrieve taxonomic data from GBIF
    tax_hier <- taxize::classification(unique(data$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

    # Save data
    #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

    #  tax_hier <- readRDS("taxonomic_hierarchy.RDS")

    # Calculate taxonomic distinctness
    diversity_cell <-
      data %>%
      tibble::add_column(diversity_val = NA) %>%
      dplyr::group_split(cellid) %>%
      purrr::map(. %>%
                   dplyr::mutate(diversity_val =
                                   calc_tax_distinctness(.,
                                                         tax_hier))) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
      dplyr::select(cellid, diversity_val)

  } else {

    stop("Invalid type argument.")

  }

  # Add map information
  # diversity_cell <-
  #   diversity_cell %>%
  #   tibble::add_column(diversity_type = type) %>%
  #   tibble::add_column(map_level = level) %>%
  #   tibble::add_column(map_region = paste(region, collapse = ","))


  return(diversity_cell)

}
