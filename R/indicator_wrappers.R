#' @title Calculate Observed Species Richness Over Space
#'
#' @description This function calculates observed species richness over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'obs_richness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' or_map <- obs_richness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(or_map)
#'
#' @export
obs_richness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "obs_richness",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Observed Species Richness Over Time
#'
#' @description This function calculates observed species richness as a time series.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'obs_richness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' or_ts <- obs_richness_ts(example_cube_1, first_year = 1985)
#' plot(or_ts)
#'
#' @export
obs_richness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "obs_richness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Total Occurrences Over Space
#'
#' @description This function calculates the total number of species occurrence
#' records over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'total_occ' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' to_map <- total_occ_map(example_cube_1, level = "country", region = "Denmark")
#' plot(to_map)
#'
#' @export
total_occ_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "total_occ",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Total Occurrences Over Time
#'
#' @description This function calculates the total number of species occurrence records
#' as a time series.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'total_occ' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' to_ts <- total_occ_ts(example_cube_1, first_year = 1985)
#' plot(to_ts)
#'
#' @export
total_occ_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "total_occ",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Pielou's Evenness Over Space
#'
#' @description This function calculates Pielou's evenness over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'pielou_evenness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' pe_map <- pielou_evenness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(pe_map)
#'
#' @export
pielou_evenness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "pielou_evenness",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Pielou's Evenness Over Time
#'
#' @description This function calculates Pielou's evenness over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'pielou_evenness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' pe_ts <- pielou_evenness_ts(example_cube_1, first_year = 1985)
#' plot(pe_ts)
#'
#' @export
pielou_evenness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "pielou_evenness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Williams' Evenness Over Space
#'
#' @description This function calculates Williams' evenness over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'williams_evenness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' we_map <- williams_evenness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(we_map)
#'
#' @export
williams_evenness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "williams_evenness",
                             dim_type = "map",
                             ...)
}

#' @title Calculate Williams' Evenness Over Time
#'
#' @description This function calculates Williams' evenness over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'williams_evenness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' we_ts <- williams_evenness_ts(example_cube_1, first_year = 1985)
#' plot(we_ts)
#'
#' @export
williams_evenness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "williams_evenness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Area-Based Rarity Over Space
#'
#' @description This function calculates area-based rarity over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'area_rarity' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' arr_map <- area_rarity_map(example_cube_1, level = "country", region = "Denmark")
#' plot(arr_map)
#'
#' @export
area_rarity_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "area_rarity",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Area-Based Rarity Over Time
#'
#' @description This function calculates area-based rarity over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'area_rarity' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' arr_ts <- area_rarity_ts(example_cube_1, first_year = 1985)
#' plot(arr_ts)
#'
#' @export
area_rarity_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "area_rarity",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Abundance-Based Rarity Over Space
#'
#' @description This function calculates abundance-based rarity over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'ab_rarity' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' abr_map <- ab_rarity_map(example_cube_1, level = "country", region = "Denmark")
#' plot(abr_map)
#'
#' @export
ab_rarity_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "ab_rarity",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Abundance-Based Rarity Over Time
#'
#' @description This function calculates abundance-based rarity over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'ab_rarity' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' abr_ts <- ab_rarity_ts(example_cube_1, first_year = 1985)
#' plot(abr_ts)
#'
#' @export
ab_rarity_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "ab_rarity",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Estimated Species Richness Over Space
#'
#' @description This function uses coverage-based methods to estimate species
#' richness over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill0' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h0_map <- hill0_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h0_map)
#'
#' @noRd
hill0_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill0",
                             dim_type = "map",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}


#' @title Calculate Estimated Species Richness Over Time
#'
#' @description This function uses coverage-based methods to estimate species
#' richness over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill0' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h0_ts <- hill0_ts(example_cube_1, first_year = 1985)
#' plot(h0_ts)
#'
#' @noRd
hil0_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill0",
                             dim_type = "ts",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}


#' @title Calculate Hill-Shannon Diversity Over Space
#'
#' @description This function uses coverage-based methods to estimate Hill-Shannon Diversity
#' over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill1' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h1_map <- hill1_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h1_map)
#'
#' @noRd
hill1_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill1",
                             dim_type = "map",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}


#' @title Calculate Hill-Shannon Diversity Over Time
#'
#' @description This function uses coverage-based methods to estimate Hill-Shannon Diversity
#' over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill1' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h1_ts <- hill1_ts(example_cube_1, first_year = 1985)
#' plot(h1_ts)
#'
#' @noRd
hill1_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill1",
                             dim_type = "ts",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}


#' @title Calculate Hill-Simpson Diversity Over Space
#'
#' @description This function uses coverage-based methods to estimate Hill-Simpson Diversity
#' over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill2' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h2_map <- hill2_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h2_map)
#'
#' @noRd
hill2_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill2",
                             dim_type = "map",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}

#' @title Calculate Hill-Simpson Diversity Over Time
#'
#' @description This function uses coverage-based methods to estimate Hill-Simpson Diversity
#' over time.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill2' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' h2_ts <- hill2_ts(example_cube_1, first_year = 1985)
#' plot(h2_ts)
#'
#' @noRd
hill2_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "hill2",
                             dim_type = "ts",
                             cutoff_length = 5,
                             inext_sampsize = 100,
                             knots = 10,
                             ...)
}


#' @title Calculate Cumulative Species Richness
#'
#' @description This function calculates cumulative species richness as a time series.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'cum_richness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' cr_ts <- cum_richness_ts(example_cube_1, first_year = 1985)
#' plot(cr_ts)
#'
#' @export
cum_richness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "cum_richness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Mean Year of Occurrence Over Space
#'
#' @description This function estimates the relative newness of records in a data cube
#' by calculating the mean year of occurrence over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'newness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' n_map <- newness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(n_map)
#'
#' @export
newness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "newness",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Mean Year of Occurrence Over Time
#'
#' @description This function estimates the change in relative newness of records
#' in a data cube over time by calculating the mean year of occurrence as a time series.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'newness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' n_ts <- newness_ts(example_cube_1, first_year = 1985)
#' plot(n_ts)
#'
#' @export
newness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "newness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Occurrence Density Over Space
#'
#' @description This function calculates the density of records over a gridded map.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_map' and 'occ_density' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' od_map <- occ_density_map(example_cube_1, level = "country", region = "Denmark")
#' plot(od_map)
#'
#' @export
occ_density_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "occ_density",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Occurrence Density Over Time
#'
#' @description This function calculates density of records as a time series.
#'
#' @param x A data cube object (class 'processed_cube').
#' @param cell_size Length of grid cell sides, in km. (Default: 10 for country, 100 for continent or world)
#' @param level Spatial level: 'continent', 'country', or 'world'. (Default: 'continent')
#' @param region The region of interest (e.g., "Europe"). (Default: "Europe")
#' @param cube_crs The projection of the cube. (Default: "EPSG:3035")
#' @param first_year Exclude data before this year. (Uses all data in the cube by default.)
#' @param last_year Exclude data after this year. (Uses all data in the cube by default.)
#'
#' @return An S3 object with the classes 'indicator_ts' and 'occ_density' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' od_ts <- occ_density_ts(example_cube_1, first_year = 1985)
#' plot(od_ts)
#'
#' @export
occ_density_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "occ_density",
                             dim_type = "ts",
                             ...)
}

#' @noRd
spec_occ_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_occ",
                             dim_type = "map",
                             ...)
}

#' @noRd
spec_occ_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_occ",
                             dim_type = "ts",
                             ...)
}

#' @noRd
spec_range_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_range",
                             dim_type = "map",
                             ...)
}

#' @noRd
spec_range_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_range",
                             dim_type = "map",
                             ...)
}

#' @noRd
tax_distinct_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "tax_distinct",
                             dim_type = "map",
                             ...)
}

#' @noRd
tax_distinct_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "tax_distinct",
                             dim_type = "ts",
                             ...)
}
