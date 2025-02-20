#' @title Calculate Observed Species Richness Over Space
#'
#' @description This function calculates observed species richness over a
#'  gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'obs_richness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' or_map <- obs_richness_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(or_map)
#' }
#' @export
obs_richness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "obs_richness",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Observed Species Richness Over Time
#'
#' @description This function calculates observed species richness as a time
#'  series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'obs_richness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' or_ts <- obs_richness_ts(example_cube_1, first_year = 1985)
#' plot(or_ts)
#' }
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
#'  records over a gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'total_occ'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' to_map <- total_occ_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(to_map)
#' }
#' @export
total_occ_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "total_occ",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Total Occurrences Over Time
#'
#' @description This function calculates the total number of species occurrence
#'  records as a time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'total_occ'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' to_ts <- total_occ_ts(example_cube_1, first_year = 1985)
#' plot(to_ts)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'pielou_evenness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' pe_map <- pielou_evenness_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(pe_map)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'pielou_evenness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' pe_ts <- pielou_evenness_ts(example_cube_1, first_year = 1985)
#' plot(pe_ts)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'williams_evenness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' we_map <- williams_evenness_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(we_map)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'williams_evenness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' we_ts <- williams_evenness_ts(example_cube_1, first_year = 1985)
#' plot(we_ts)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'area_rarity'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' arr_map <- area_rarity_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(arr_map)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'area_rarity'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' arr_ts <- area_rarity_ts(example_cube_1, first_year = 1985)
#' plot(arr_ts)
#' }
#' @export
area_rarity_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "area_rarity",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Abundance-Based Rarity Over Space
#'
#' @description This function calculates abundance-based rarity over a gridded
#'  map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'ab_rarity'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' abr_map <- ab_rarity_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(abr_map)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'ab_rarity'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' abr_ts <- ab_rarity_ts(example_cube_1, first_year = 1985)
#' plot(abr_ts)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#' @param coverage The sample coverage value for the estimator. Default is 0.95.
#' @param cutoff_length The minimum number of data points for each grid cell.
#'  Grid cells with fewer data points will be removed before calculations to
#'  avoid errors. Default is 5.
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill0' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h0_map <- hill0_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h0_map)
#' }
#' @export
hill0_map <- function(data,
                      coverage = 0.95,
                      cutoff_length = 5,
                      ...) {
  compute_indicator_workflow(data,
                             type = "hill0",
                             dim_type = "map",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}


#' @title Calculate Estimated Species Richness Over Time
#'
#' @description This function uses coverage-based methods to estimate species
#' richness over time.
#'
#' @param data A data cube object (class 'processed_cube').
#' @param coverage The sample coverage value for the estimator. Default is 0.95.
#' @param cutoff_length The minimum number of data points for each year.
#'  Years with fewer data points will be removed before calculations to avoid
#'  errors. Default is 5.
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill0' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h0_ts <- hill0_ts(example_cube_1, first_year = 1985)
#' plot(h0_ts)
#' }
#' @export
hill0_ts <- function(data,
                     coverage = 0.95,
                     cutoff_length = 5,
                     ...) {
  compute_indicator_workflow(data,
                             type = "hill0",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}


#' @title Calculate Hill-Shannon Diversity Over Space
#'
#' @description This function uses coverage-based methods to estimate
#'  Hill-Shannon Diversity over a gridded map.
#'
#' @inheritParams hill0_map
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill1' containing
#'  the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h1_map <- hill1_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h1_map)
#' }
#' @export
hill1_map <- function(data,
                      cutoff_length = 5,
                      coverage = 0.95,
                      ...) {
  compute_indicator_workflow(data,
                             type = "hill1",
                             dim_type = "map",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}


#' @title Calculate Hill-Shannon Diversity Over Time
#'
#' @description This function uses coverage-based methods to estimate
#'  Hill-Shannon Diversity over time.
#'
#' @inheritParams hill0_ts
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill1' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h1_ts <- hill1_ts(example_cube_1, first_year = 1985)
#' plot(h1_ts)
#' }
#' @export
hill1_ts <- function(data,
                     cutoff_length = 5,
                     coverage = 0.95,
                     ...) {
  compute_indicator_workflow(data,
                             type = "hill1",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}


#' @title Calculate Hill-Simpson Diversity Over Space
#'
#' @description This function uses coverage-based methods to estimate
#'  Hill-Simpson Diversity over a gridded map.
#'
#' @inheritParams hill0_map
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'hill2' containing
#'  the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h2_map <- hill2_map(example_cube_1, level = "country", region = "Denmark")
#' plot(h2_map)
#' }
#' @export
hill2_map <- function(data,
                      cutoff_length = 5,
                      coverage = 0.95,
                      ...) {
  compute_indicator_workflow(data,
                             type = "hill2",
                             dim_type = "map",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}

#' @title Calculate Hill-Simpson Diversity Over Time
#'
#' @description This function uses coverage-based methods to estimate
#'  Hill-Simpson Diversity over time.
#'
#' @inheritParams hill0_ts
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'hill2' containing
#'  the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' h2_ts <- hill2_ts(example_cube_1, first_year = 1985)
#' plot(h2_ts)
#' }
#' @export
hill2_ts <- function(data,
                     cutoff_length = 5,
                     coverage = 0.95,
                     ...) {
  compute_indicator_workflow(data,
                             type = "hill2",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             ...)
}


#' @title Calculate Cumulative Species Richness
#'
#' @description This function calculates cumulative species richness as a time
#'  series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'cum_richness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' cr_ts <- cum_richness_ts(example_cube_1, first_year = 1985)
#' plot(cr_ts)
#' }
#' @export
cum_richness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "cum_richness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Mean Year of Occurrence Over Space
#'
#' @description This function estimates the relative newness of records in a
#'  data cube by calculating the mean year of occurrence over a gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'newness'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' n_map <- newness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(n_map)
#' }
#' @export
newness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "newness",
                             dim_type = "map",
                             ...)
}


#' @title Calculate Mean Year of Occurrence Over Time
#'
#' @description This function estimates the change in relative newness of
#'  records in a data cube over time by calculating the mean year of occurrence
#'  as a time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'newness' containing
#' the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' n_ts <- newness_ts(example_cube_1, first_year = 1985)
#' plot(n_ts)
#' }
#' @export
newness_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "newness",
                             dim_type = "ts",
                             ...)
}


#' @title Calculate Occurrence Density Over Space
#'
#' @description This function calculates the density of records over a gridded
#'  map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'occ_density'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' od_map <- occ_density_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(od_map)
#' }
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
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'occ_density'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' od_ts <- occ_density_ts(example_cube_1, first_year = 1985)
#' plot(od_ts)
#' }
#' @export
occ_density_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "occ_density",
                             dim_type = "ts",
                             ...)
}

#' @title Calculate Species Occurrences Over Space
#'
#' @description This function calculates the number of occurrences for
#'  individual species over a gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'spec_occ'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' so_map <- spec_occ_map(example_cube_1, level = "country", region = "Denmark")
#' plot(so_map, c(2440728, 4265185))
#' }
#' @export
spec_occ_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_occ",
                             dim_type = "map",
                             ...)
}

#' @title Calculate Number of Occurrences for One or More Species Over Time
#'
#' @description This function calculates number of occurrences for individual
#' species as time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'spec_occ'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' so_ts <- spec_occ_ts(example_cube_1, first_year = 1985)
#' plot(so_ts, c(2440728, 4265185))
#'
#' @export
spec_occ_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_occ",
                             dim_type = "ts",
                             ...)
}

#' @title Plot Species Ranges Over Space
#'
#' @description This function plots the cells occupied for individual
#' species over a gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'spec_range'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' sr_map <- spec_range_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(sr_map, c(2440728, 4265185))
#'
#' @export
spec_range_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_range",
                             dim_type = "map",
                             ...)
}

#' @title Calculate Range Size (Number of Cells Occupied) for One or More
#'  Species Over Time
#'
#' @description This function calculates number of cells occupied for individual
#' species as time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'spec_range'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' sr_ts <- spec_range_ts(example_cube_1, first_year = 1985)
#' plot(sr_ts, c(2440728, 4265185))
#' }
#' @export
spec_range_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_range",
                             dim_type = "ts",
                             ...)
}

#' @title Calculate Taxonomic Distinctness Over Space
#'
#' @description This function calculates the taxonomic distinctness index over
#'  a gridded map.
#'
#' @param data A data cube object (class 'processed_cube').
#' @param rows Choose which row to select if there are multiple matches when
#'  retrieving taxonomic information from GBIF. (Default is 1. Use NA for
#'  interactive mode.)
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' and 'tax_distinct'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' td_map <- tax_distinct_map(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(td_map)
#' }
#'
#' @export
tax_distinct_map <- function(data, rows = 1, ...) {
  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("The package {taxize} is required for this action")
  }
  compute_indicator_workflow(data,
                             type = "tax_distinct",
                             dim_type = "map",
                             rows = rows,
                             ...)
}

#' @title Calculate Taxonomic Distinctness Over Time
#'
#' @description This function calculates the taxonomic distinctness index as a
#'  time series.
#'
#' @param data A data cube object (class 'processed_cube').
#' @param rows Choose which row to select if there are multiple matches when
#'  retrieving taxonomic information from GBIF. (Default is 1. Use NA for
#'  interactive mode.)
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'tax_distinct'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' \dontrun{
#' td_ts <- tax_distinct_ts(example_cube_1, level = "country",
#'  region = "Denmark")
#' plot(td_ts)
#' }
#' @export
tax_distinct_ts <- function(data, rows = 1, ...) {
  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("The package {taxize} is required for this action")
  }
  compute_indicator_workflow(data,
                             type = "tax_distinct",
                             dim_type = "ts",
                             rows = rows,
                             ...)
}

#' @title Calculate Occupancy Turnover Over Time
#'
#' @description This function calculates occupancy turnover
#' as a time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'occ_turnover'
#'  containing the calculated indicator values and metadata.
#'
#' @examples
#' ot_ts <- occ_turnover_ts(example_cube_1, first_year = 1985)
#' plot(ot_ts)
#'
#' @export
occ_turnover_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "occ_turnover",
                             dim_type = "ts",
                             ...)
}
