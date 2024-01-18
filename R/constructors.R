#' @export
processed_cube <- function(x) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "eea_cell_code",
                  "taxonKey",
                  "obs",
                  "scientificName",
                  "xcoord",
                  "ycoord",
                  "resolution") %in% names(x)))
  if (all(c("datasetKey", "basisOfRecord") %in% names(x))) {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = list("xmin" = min(x$xcoord),
                                      "xmax" = max(x$xcoord),
                                      "ymin" = min(x$ycoord),
                                      "ymax" = max(x$ycoord)),
                   num_cells = length(unique(x$eea_cell_code)),
                   num_species = length(unique(x$taxonKey)),
                   num_obs = sum(x$obs),
                   kingdoms = unique(x$kingdom),
                   resolutions = unique(x$resolution),
                   multi_res = ifelse(length(unique(x$resolution)) > 1, TRUE, FALSE),
                   num_datasets = length(unique(x$datasetKey)),
                   record_types = unique(x$basisOfRecord),
                   data = x),
              class = "processed_cube_dsinfo")
  } else {
    structure(list(first_year = min(x$year),
                   last_year = max(x$year),
                   coord_range = list("xmin" = min(x$xcoord),
                                      "xmax" = max(x$xcoord),
                                      "ymin" = min(x$ycoord),
                                      "ymax" = max(x$ycoord)),
                   num_cells = length(unique(x$eea_cell_code)),
                   num_species = length(unique(x$taxonKey)),
                   num_obs = sum(x$obs),
                   kingdoms = unique(x$kingdom),
                   resolutions = unique(x$resolution),
                   multi_res = ifelse(length(unique(x$resolution)) > 1, TRUE, FALSE),
                   data = x),
              class = "processed_cube")
  }
}

#' @export
indicator_ts <- function(x,
                         div_type,
                         kingdoms,
                         num_species,
                         num_years,
                         species_names,
                         map_level,
                         map_region,
                         coord_range) {
  # check that x is a tibble and all necessary columns are present
  stopifnot(tibble::is_tibble(x),
            all(c("year",
                  "diversity_val") %in% names(x)))
  #names(coord_range) <- c("xmin", "ymin", "xmax", "ymax")
  if(div_type == "pielou_evenness" | div_type == "e9_evenness") {
    id = div_type
    div_type = "evenness"
  } else {
    id = div_type
  }
  structure(list(div_name = get_divname(id),
                 first_year = min(x$year),
                 last_year = max(x$year),
                 num_years = num_years,
                 num_species = num_species,
                 map_level = map_level,
                 map_region = map_region,
                 kingdoms = kingdoms,
                 coord_range = coord_range,
                 species_names = species_names,
                 data = x),
            class = c("indicator_ts", div_type),
            indicator_id = id,
            type = "ts")
}

#' @export
indicator_map <- function(x,
                          div_type,
                          cs1,
                          cs2,
                          map_level,
                          map_region,
                          kingdoms,
                          num_species,
                          first_year,
                          last_year,
                          num_years,
                          species_names,
                          years_with_obs) {
  # check that x is both a data frame and sf object
  # and all necessary columns are present
  stopifnot(inherits(x, c("sf", "data.frame")),
            all(c("cellid",
                  "geometry") %in% names(x)))
  coord_range = sf::st_bbox(x)
 # names(coord_range) = c("xmin", "ymin", "xmax", "ymax")
  if (cs1 == cs2) {
    cell_size = paste(cs1, "km^2")
  } else {
    cell_size = paste(cs1, "kmx", cs2, "km", sep = "")
  }
  if(div_type == "pielou_evenness" | div_type == "e9_evenness") {
    id = div_type
    div_type = "evenness"
  } else {
    id = div_type
  }
  structure(list(div_name = get_divname(id),
                 num_cells = length(x$cellid),
                 cell_size = cell_size,
                 map_level = map_level,
                 map_region = map_region,
                 projection = sf:::crs_parameters(st_crs(x$geometry))$Name,
                 coord_range = coord_range,
                 first_year = first_year,
                 last_year = last_year,
                 num_years = num_years,
                 num_species = num_species,
                 kingdoms = kingdoms,
                 species_names = species_names,
                 years_with_obs = years_with_obs,
                 data = x),
            class = c("indicator_map", div_type),
            indicator_id = id,
            type = "map")
}


