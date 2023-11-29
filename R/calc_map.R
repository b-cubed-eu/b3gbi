# This function calculates and maps species richness across grid cells
# The user can choose the cell sizes and the region to map
# Input is ?, level should be "world", "continent", or "country"
# region is the specific continent, country, etc., or "world" for global.
# level and region therefore must match.
# Arguments cs1 and cs2 are width and height (in km) of the grid.
# coverage determines the coverage percentage to standardize at (0.9 = 90%)
# cutoff_length determines the smallest number of occurrences per grid cell
# for diversity estimation. Anything smaller becomes NA.
# inext_sampsize determines the maximum sample size to use for diversity estimation
# knots determines the number of different sample sizes to estimate at (between
# 0 and inext_sampsize)
# sub-sampling will be used for larger samples and extrapolation for smaller ones
# type can be "hill" for hill diversity measure, "even" for evenness,
# "obs_rich" for observed species richness, "total_obs" for total occurrences,
# "newness" for newness of data (mean occurrence year), "density" for the
# number of occurrences per square km
# qval determines which type of hill diversity measure to estimate,
# 0 for species richness, 1 for shannon hill diversity, 2 for simpson hill diversity

calc_map <- function(data,
                     cs1 = 100,
                     cs2 = 100,
                     level = "country",
                     region = "Denmark",
                     type = "hill0",
                     coverage = 0.95,
                     cutoff_length = 5,
                     inext_sampsize = 100,
                     knots = 10,
                     newness_min_year = NULL,
                     even_type = "pielou",
                     ...) {


  if (type == "e9_evenness") {
    calc_evenness <- calc_e9_evenness
  } else if (type == "pielou_evenness") {
    calc_evenness <- calc_pielou_evenness
  }

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

  #
  # # Remove grid cells with areas smaller than 20% of the largest one
  # grid_filtered <-
  #   grid %>%
  #   filter(area_km2 > 0.2 * max(area_km2))
  #
  # # Remove same grid cells from data
  # merged_data_cells_filtered <-
  #   merged_data_cells %>%
  #   filter(cellid %in% grid_filtered$cellid)


  if (type %in% c("hill0", "hill1", "hill2")) {

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  # Create list of occurrence matrices by grid cell, with species as rows
  spec_rec_raw_cell <-
    data_cell %>%
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
                               -area_km2,
                               -basisOfRecord,
                               -datasetKey) %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 #dplyr::mutate_if(is.character,
                 #                 as.integer) %>%
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
  names(spec_rec_raw_cell) <- unique(data_cell$cellid)

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
      data_cell %>%
      dplyr::summarize(diversity_val = sum(n_distinct(taxonKey)),
                       .by = "cellid")

  } else if (type == "total_occ") {

    # Calculate total number of occurrences over the grid
    diversity_cell <-
      data_cell %>%
      dplyr::summarize(diversity_val = sum(obs),
                       .by = "cellid")

  } else if (type == "newness") {

    # Calculate mean year of occurrence over the grid
    diversity_cell <-
      data_cell %>%
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
      data_cell %>%
      dplyr::reframe(diversity_val = sum(obs) / area_km2,
                     .by = "cellid") %>%
      dplyr::distinct(cellid, diversity_val) %>%
      dplyr::mutate(diversity_val = as.numeric(diversity_val))

  } else if (type == "e9_evenness" | type == "pielou_evenness") {

    # Calculate adjusted evenness for each grid cell
    diversity_cell <-
      data_cell %>%
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
        data_cell %>%
        dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
        dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
        dplyr::summarise(diversity_val = sum(rarity), .by = "cellid") %>%
        dplyr::arrange(cellid)

    } else if (type == "area_rarity") {

      # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
      # frequency for each species
      diversity_cell <-
        data_cell %>%
        dplyr::mutate(rec_tax_cell = sum(n_distinct(cellid)),
                      .by = c(taxonKey)) %>%
        dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(n_distinct(cellid)))) %>%
        dplyr::summarise(diversity_val = sum(rarity), .by = cellid)

    } else if (type == "spec_occ") {

      # Calculate total occurrences for each species by grid cell
      diversity_cell <-
        data_cell %>%
        dplyr::mutate(num_records = sum(obs), .by = c(taxonKey, cellid)) %>%
        dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
        dplyr::arrange(cellid) %>%
        dplyr::select(cellid, taxonKey, scientificName, num_records)

    } else if (type == "tax_distinct") {

      # Retrieve taxonomic data from GBIF
      tax_hier <- taxize::classification(unique(data$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

      # Save data
      #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

      #  tax_hier <- readRDS("taxonomic_hierarchy.RDS")

      # Define function to calculate taxonomic distinctness
      tax_distinct_fn <- function(x, y) {

        temp <- names(y) %in% x$scientificName

        tax_hier_temp <- y[c(temp)]

        print(length(tax_hier_temp))

        n_spec <- length(tax_hier_temp)

        if(length(tax_hier_temp) < 3) {

          tax_distinct <- NA

          return(tax_distinct)

        } else {

          tax_tree <- taxize::class2tree(tax_hier_temp, check=FALSE)

          tax_distance <- tax_tree$distmat

          tax_distinct <- sum(tax_distance) / ((n_spec * (n_spec - 1)) / 2)

          return(tax_distinct)

        }

      }

      # Calculate taxonomic distinctness
      diversity_cell <-
        data_cell %>%
        tibble::add_column(diversity_val = NA) %>%
        dplyr::group_split(cellid) %>%
        purrr::map(. %>%
                     dplyr::mutate(diversity_val = tax_distinct_fn(.,
                                                                   tax_hier))) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
        dplyr::select(cellid, diversity_val)

    } else {

    stop("Invalid type argument.")

    }

  # Add map information
  diversity_cell <-
    diversity_cell %>%
    tibble::add_column(diversity_type = type) %>%
    tibble::add_column(map_level = level) %>%
    tibble::add_column(map_region = paste(region, collapse = ","))

  # Add grid-based rarity to grid
  diversity_grid <-
    grid %>%
    dplyr::left_join(diversity_cell, by = "cellid")

}
