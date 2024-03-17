# #' @export
# calc_map <- function(x, ...) {
#
#   useMethod(calc_map)
#
# }

#' @export
hill0 <- function(x,
                  coverage = 0.95,
                  cutoff_length = 5,
                  inext_sampsize = 100,
                  knots = 10,
                  ...) {

  x <- calculate_indicator(x,
                           type = hill0,
                           coverage = coverage,
                           cutoff_length = cutoff_length,
                           inext_sampsize = inext_sampsize,
                           knots = knots,
                           ...)

  return(x)

}

#' @export
hill1 <- function(x,
                  coverage = 0.95,
                  cutoff_length = 5,
                  inext_sampsize = 100,
                  knots = 10,
                  ...) {

  x <- calculate_indicator(x,
                           type = hill1,
                           coverage = coverage,
                           cutoff_length = cutoff_length,
                           inext_sampsize = inext_sampsize,
                           knots = knots,
                           ...)

  return(x)

}

#' @export
hill2 <- function(x,
                  coverage = 0.95,
                  cutoff_length = 5,
                  inext_sampsize = 100,
                  knots = 10,
                  ...) {

  x <- calculate_indicator(x,
                           type = hill2,
                           coverage = coverage,
                           cutoff_length = cutoff_length,
                           inext_sampsize = inext_sampsize,
                           knots = knots,
                           ...)

  return(x)

}

#' @export
obs_richness <- function(x,
                         ...) {

  x <- calculate_indicator(x,
                           type = obs_richness,
                           ...)

  return(x)

}

# calc_map.hill <- function(data,
#                           hill_type,
#                           coverage,
#                           cutoff_length,
#                           inext_sampsize,
#                           knots,
#                           ...) {
#
#   # Extract qvalue from hill diversity type
#   qval <- as.numeric(gsub("hill", "", hill_type))
#
#   # Create list of occurrence matrices by grid cell, with species as rows
#   spec_rec_raw_cell <-
#     data %>%
#     dplyr::group_split(cellid) %>%
#     purrr::map(. %>%
#                  dplyr::group_by(eea_cell_code,
#                                  taxonKey) %>%
#                  tidyr::pivot_wider(names_from = taxonKey,
#                                     values_from = obs) %>%
#                  dplyr::ungroup() %>%
#                  dplyr::select(-eea_cell_code,
#                                -scientificName,
#                                -kingdom,
#                                -rank,
#                                -geometry,
#                                -resolution,
#                                -xcoord,
#                                -ycoord,
#                                -year,
#                                -area_km2) %>%
#                  dplyr::select(-any_of(c("basisOfRecord",
#                                          "datasetKey"))) %>%
#                  replace(is.na(.), 0) %>%
#                  dplyr::mutate_if(is.numeric,
#                                   as.integer) %>%
#                  dplyr::select(-cellid) %>%
#                  tibble::rownames_to_column() %>%
#                  tidyr::gather(variable,
#                                value,
#                                -rowname) %>%
#                  tidyr::spread(rowname, value) %>%
#                  'row.names<-'(., NULL) %>%
#                  tibble::column_to_rownames(var = "variable") %>%
#                  as.matrix() %>%
#                  replace(. > 1, as.integer(1))
#     )
#
#
#   # name list elements
#   names(spec_rec_raw_cell) <- unique(data$cellid)
#
#   # remove all cells with too little data to avoid errors from iNEXT
#   spec_rec_raw_cell2 <- spec_rec_raw_cell %>%
#     keep(., function(x) length(x) > cutoff_length)
#
#   # Calculate diversity estimates
#   # coverage_rare_cell <- spec_rec_raw_cell2 %>%
#   #   iNEXT::iNEXT(endpoint=inext_sampsize, knots=knots, datatype="incidence_raw", q=qval)
#
#   coverage_rare_cell <- spec_rec_raw_cell2 %>%
#     iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval, ...)
#
#   # Extract estimated relative diversity
#   indicator <-
#     coverage_rare_cell %>%
#     #coverage_rare_cell$iNextEst$coverage_based %>%
#     #dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
#     #              .by = Assemblage) %>%
#     dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
#     dplyr::rename(cellid = Assemblage,
#                   diversity_val = qD,
#                   samp_size_est = t,
#                   coverage = SC,
#                   diversity_type = Order.q) %>%
#     dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")
#
#   return(diversity_cell)
# }

# #' @export
# calc_map.obs_richness <- function(data,
#                                   ...) {
#
#   # Calculate species richness over the grid
#   indicator <-
#     data %>%
#     dplyr::summarize(diversity_val = sum(n_distinct(taxonKey)),
#                      .by = "cellid")
#
#   return(indicator)
#
# }
