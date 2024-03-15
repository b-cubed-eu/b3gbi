calc_map.hill0 <- function(data, ...) {
  indicator <- calc_map.hill_core(data = data,
                                  type = "hill0",
                                  ...)
}

calc_map.hill1 <- function(data, ...) {
  indicator <- calc_map.hill_core(data = data,
                                  type = "hill1",
                                  ...)
}

calc_map.hill2 <- function(data, ...) {
  indicator <- calc_map.hill_core(data = data,
                                  type = "hill2",
                                  ...)
}

calc_map.hill_core <- function(data,
                               type = c("hill0, hill1, hill2"),
                               cutoff_length = 100,
                               coverage = 0.95,
                               ...)
{

  stopifnot_error("This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

  type <- match.arg(type)

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

  # Compute hill diversity
  coverage_rare_cell <- spec_rec_raw_cell2 %>%
    iNEXT::estimateD(datatype="incidence_raw",
                     base = "coverage",
                     level = coverage,
                     q=qval,
                     ...)

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

}


