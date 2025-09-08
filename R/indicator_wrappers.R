#' @title Calculate Observed Species Richness Over Space or Time
#'
#' @description This function calculates observed species richness over a
#' gridded map or as a time series (see 'Details' for more information).
#'
#' @details
#' ## Species richness
#' Species richness is the total number of species present in a
#' sample (Magurran, 1988). It is a fundamental and commonly used
#' measure of biodiversity, providing a simple and intuitive
#' overview of the status of biodiversity. However, richness is not
#' well suited to measuring biodiversity change over time, as it
#' only decreases when local extinctions occur and thus lags behind
#' abundance for negative trends. While it may act as a leading
#' indicator of alien species invasions, it will not indicate
#' establishment because it ignores abundance. Nor will it
#' necessarily indicate changes in local species composition, which
#' can occur without any change in richness. Although richness is
#' conceptually simple, it can be measured in different ways.
#'
#' ## Observed richness
#' Observed richness is calculated by summing the number of unique species
#' observed for each year or each cell. Observed richness is highly dependent
#' on the comprehensiveness of the dataset it is being applied to. If some
#' regions are more intensively, carefully or systematically sampled than
#' others, this will likely reflect as higher observed richness. Observed
#' richness also depends on the relative abundance and spatial aggregation of
#' each species, with less abundant and less aggregated species less likely to
#' be discovered during surveys (Hillebrand et al., 2018), as well as the
#' detectability of each species.
#'
#' @references
#' Hillebrand, H., Blasius, B., Borer, E. T., Chase, J. M., Downing, J. A.,
#' Eriksson, B. K., Filstrup, C. T., Harpole, W. S., Hodapp, D., Larsen, S.,
#' Lewandowska, A. M., Seabloom, E. W., Van de Waal, D. B., & Ryabov, A. B.
#' (2018). Biodiversity change is uncoupled from species richness trends:
#' Consequences for conservation and monitoring. *Journal of Applied Ecology*,
#' *55*(1), 169-184.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'obs_richness' containing the calculated indicator values and metadata.
#'
#' @describeIn obs_richness_map
#'
#' @examples
#' \dontrun{
#' or_map <- obs_richness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(or_map)
#' }
#' @export
obs_richness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "obs_richness",
                             dim_type = "map",
                             ...)
}


#' @describeIn obs_richness_map
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


#' @title Calculate Total Occurrences Over Space or Time
#'
#' @description This function calculates the total number of species occurrence
#' records over a gridded map or as a time series (see 'Details' for more '
#' information).
#'
#' @details
#' The total number of occurrences is calculated by summing the occurrences of
#' all species observed for each cell or year. While not itself an indicator,
#' this variable provides an overview of the comprehensiveness and distribution
#' of data in the cube being analysed, and may be helpful, or even vital, for
#' interpreting the results of calculated indicators.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'total_occ' containing the calculated indicator values and metadata.
#'
#' @describeIn total_occ_map
#'
#' @examples
#' \dontrun{
#' to_map <- total_occ_map(example_cube_1, level = "country",
#'                                         region = "Denmark")
#' plot(to_map)
#' }
#' @export
total_occ_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "total_occ",
                             dim_type = "map",
                             ...)
}


#' @describeIn total_occ_map
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


#' @title Calculate Evenness Over Time or Space
#'
#' @description Calculate evenness over a gridded map or as a time series (see
#' 'Details' for more information).
#'
#' @details
#' ## Evenness
#'
#' Species evenness is a commonly used indicator that measures how
#' uniformly individuals are distributed across species in a region
#' or over time. It provides a complement to richness by taking
#' relative abundance into account. Although GBIF provides
#' information about abundances as individual counts, the majority
#' of entries lack this information. Hence, evenness can only be
#' calculated using the proportions of observations rather than
#' proportions of individuals. Strictly speaking, the evenness
#' measures therefore indicate how uniformly species are
#' represented in the respective data set rather than the true
#' evenness of the ecological community.
#'
#' ## Pielou's evenness
#'
#' Pielou's evenness (1966) is a well-known and commonly
#' used evenness measure. It is calculated as:
#'
#' \deqn{
#'  E = \frac{-\sum_{i=1}^{S} p_i \ln(p_i)}{\ln(S)}
#' }{
#'  E = -Sum from i=1 to S of pi * ln(pi) / ln(S)
#' }
#' where S is the number of species and pi is the proportion of occurrences
#' represented by species i.
#'
#' ## Williams' evenness
#'
#' An analysis of evenness properties by Kv&aring;lseth (2015) showed
#' that an evenness index introduced by Williams in 1977 in an
#' unpublished manuscript has two important properties which
#' Pielou's does not. The properties in question are complex
#' mathematical properties known as the Schur-Concavity and
#' value validity, but we attempt to describe them here more
#' simply. If a measure of evenness is Schur-concave, it means
#' that when the distribution of individuals becomes more evenly
#' spread across species, the measure of evenness will stay the
#' same or increase, but never decrease. Value validity means
#' that an evenness index should provide sensible and meaningful
#' values across its range for any given distribution of species
#' abundances. Kv&aring;lseth referred to this evenness measure as E9
#' but we refer to it as Williams' evenness.
#'
#' Williams' evenness is calculated as:
#'
#' \deqn{
#'   1 - \sqrt{\frac{S\sum_{i=1}^{S} p_i^2 - 1}{S - 1}}
#' }{
#'   1 - sqrt((S * Sum from i=1 to S of pi^2 - 1) / (S - 1))
#' }
#'
#' where S is the number of species and pi is the proportion of occurrences
#' represented by species i.
#'
#' @references
#' Pielou, E. C. (1966). The measurement of diversity in
#' different types of biological collections. *Journal of theoretical biology*,
#' *13*, 131-144.
#'
#' Kv&aring;lseth, T. O. (2015). Evenness indices once again: critical analysis of
#' properties. *SpringerPlus*, *4*, 1-12.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'pielou_evenness' or 'williams_evenness' containing the calculated indicator
#' values and metadata.
#'
#' @describeIn pielou_evenness_map
#'
#' @examples
#' \dontrun{
#' pe_map <- pielou_evenness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(pe_map)
#' }
#' @export
pielou_evenness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "pielou_evenness",
                             dim_type = "map",
                             ...)
}


#' @describeIn pielou_evenness_map
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


#' @describeIn pielou_evenness_map
#'
#' @examples
#' \dontrun{
#' we_map <- williams_evenness_map(example_cube_1, level = "country", region = "Denmark")
#' plot(we_map)
#' }
#' @export
williams_evenness_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "williams_evenness",
                             dim_type = "map",
                             ...)
}

#' @describeIn pielou_evenness_map
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


#' @title Calculate Rarity Over Time or Space
#'
#' @description This function calculates area-based or abundance-based rarity
#' over a gridded map or as a time series (see 'Details' for more information).
#'
#' @details
#' ## Rarity
#' Rarity is the scarcity or infrequency of a particular species in
#' an area. A rare species might have a small population size, a
#' limited distribution, or a unique ecological niche (Maciel,
#' 2021; Rabinowitz, 1981). Rarity can also be a biodiversity
#' indicator when summed over multiple species in an area, and may
#' provide important insight for determining conservation
#' priorities. When measured over time, rarity may indicate
#' potential threats or changes in the environment.
#'
#' ## Abundance-Based Rarity
#' Abundance-based rarity is the inverse of the proportion of total occurrences
#' represented by a particular species. The total summed rarity for each grid
#' cell or year is calculated (sum the rarity values of each species present
#' there). It is calculated as:
#'
#' \deqn{
#'   \sum_{i=1}^{S} \frac{1}{p_i}
#' }{
#'   Sum from i=1 to S of 1 / pi
#' }
#'
#' where S is the number of species and pi is the proportion of occurrences
#' represented by species i.
#'
#' ## Area-Based Rarity
#' Area-based rarity is the inverse of occupancy frequency (proportion of grid
#' cells occupied) for a particular species. The total summed rarity for each
#' grid cell or year is calculated (sum the rarity values of each species
#' present there). It is calculated as:
#'
#' \deqn{
#'   \sum_{i=1}^{S} \frac{N}{n_i}
#' }{
#'   Sum from i=1 to S of N / ni
#' }
#'
#' where S is the number of species, N is the total number of occupied grid
#' cells, and ni is the number of grid cells occupied by species i.
#'
#' @references
#' Maciel, E. A. (2021). An index for assessing the rare species of a community.
#' *Ecological Indicators*, 124, 107424.
#'
#' Rabinowitz, D. (1981). Seven forms of rarity. *Biological aspects of rare *
#' *plant conservation*.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'area_rarity' or 'ab_rarity' containing the calculated indicator values and
#' metadata.
#'
#' @describeIn area_rarity_map
#'
#' @examples
#' \dontrun{
#' arr_map <- area_rarity_map(example_cube_1, level = "country", region = "Denmark")
#' plot(arr_map)
#' }
#' @export
area_rarity_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "area_rarity",
                             dim_type = "map",
                             ...)
}


#' @describeIn area_rarity_map
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
                             force_grid = TRUE,
                             ...)
}


#' @describeIn area_rarity_map
#'
#' @examples
#' \dontrun{
#' abr_map <- ab_rarity_map(example_cube_1, level = "country", region = "Denmark")
#' plot(abr_map)
#' }
#' @export
ab_rarity_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "ab_rarity",
                             dim_type = "map",
                             ...)
}


#' @describeIn area_rarity_map
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

#' @noRd
hill_diversity_details <- paste0(
  "<h3>Hill diversity</h3>",
  "\n\n",
  "Hill (1973) introduced the concept of Hill diversity, which assumes ",
  "that the number and relative abundance of species are inseparable ",
  "components of diversity. Hill diversity uses a single equation to ",
  "calculate multiple measures of diversity by varying a single ",
  "parameter \u2113, which changes the emphasis on rare vs common species ",
  "(Roswell et al., 2019). It represents the mean rarity of sampled ",
  "species, and is calculated as: ",
  "\\deqn{",
  "  D = \\left( \\sum_{i=1}^{S} p_i^\\ell \\right)^{1/(1-\\ell)}",
  "  }{",
  "  D = (Sum from i=1 to S of pi^l) ^ (1 / (1 - l))",
  "  }",
  "where D is diversity, S is the number of species, pi is the proportion ",
  "of individuals belonging to species i, ri is the rarity of species i, ",
  "and \u2113 determines the rarity scale for the mean. While \u2113 can ",
  "theoretically take almost any value, three common measures of diversity ",
  "are special cases: species richness, and modified versions of the ",
  "Shannon and Simpson diversity indices (Roswell et al., 2019). These ",
  "three measures occur when \u2113 takes the value of 1, 0 (or near-zero, ",
  "as \u2113 cannot actually take the value of 0), or -1, respectively. \n",
  "\n* **Species Richness (\u2113 = 1):**",
  "  \\deqn{",
  "    D = S",
  "  }{",
  "    D = S",
  "  }",
  "\n* **Hill-Shannon Diversity (\u2113 \u2248 0):**",
  "  \\deqn{",
  "    D = e^{-\\sum_{i=1}^{S} p_i \\ln(p_i)}",
  "  }{",
  "    D = e ^ (-Sum from i=1 to S of pi * ln(pi))",
  "  }",
  "\n* **Hill-Simpson Diversity (\u2113 = -1):**",
  "  \\deqn{",
  "    D = \\frac{1}{\\sum_{i=1}^{S} p_i^2}",
  "  }{",
  "    D = 1 / (Sum from i=1 to S of pi^2)",
  "  }",
  "\n\nRichness uses an arithmetic scale (the arithmetic mean), thus giving ",
  "rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
  "uses a logarithmic scale (the geometric mean), treating common and ",
  "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
  "scale (the harmonic mean), giving common species higher leverage.",
  "\n\n",
  "<h3>Coverage-based estimation</h3>",
  "\n\n",
  "Hill diversity values can be estimated through different ",
  "standardisation procedures as a way to mitigate the effects of sample size and ",
  "sampling biases. One way to do this is by equalising sample size by calculating ",
  "a species accumulation curve (a plot of cumulative species richness as a function ",
  "of sample size) for each year or grid cell. The smallest ",
  "sample size from among all the grid cells or years in the dataset is used as a reference ",
  "to select richness values from each curve. This is called rarefaction. It is also ",
  "possible to use a larger sample size as a reference, but this requires extrapolation ",
  "of smaller samples, which is more prone to error than rarefaction.\n\n",
  "However, results from sample-size based estimation can be ",
  "problematic as they depend on both richness and evenness. A sample from a ",
  "community with a more even distribution of individuals across species is likely ",
  "to show higher richness than a sample of the same size from a community where ",
  "many species are rare, as the rare species are less likely to appear in the ",
  "sample. Similarly, a community containing a lot of species will appear less ",
  "rich than it actually is if the sample size used for comparison is too small. ",
  "Detectability also plays an important part; hard to detect species are less ",
  "likely to appear in the sample, so communities in which rare species are more ",
  "easily detectable are likely to yield richer samples. \n\n",
  "Another way to estimate species richness is to standardise ",
  "by coverage. The iNEXT package (Chao et al., 2014; Hsieh et al., 2016) for R is ",
  "used to estimate species richness at an equal level of coverage (e.g. 0.95) for ",
  "each cell or year in a biodiversity data cube. Coverage is the proportion of ",
  "individuals in the community belonging to species in the sample. So, at a ",
  "coverage of 0.95, 95% of individuals in the community belong to species ",
  "detected in the sample while 5% belong to species that are not detected in the ",
  "sample. Coverage is estimated based on the frequencies of species already in ",
  "the sample. It can be illustrated using a species accumulation curve, the slope ",
  "of which represents the probability of detecting a new species with the next ",
  "individual you sample from a community. At a sample size of zero, the slope ",
  "would be one, meaning the next individual sampled has a 100% probability of ",
  "being a species not already in the sample. Therefore, a coverage value of one ",
  "corresponds to the asymptote of a species accumulation curve (slope of zero), ",
  "meaning no new species would be uncovered through further sampling."

)

#' @title Calculate Estimated Hill Diversity Over Space or Time
#'
#' @description Use coverage-based methods to estimate Hill diversity measures
#' over a gridded map or as a time series.
#' Three Hill diversity measures are covered:
#'
#' *Species richness* - <code>hill0_map()</code> and <code>hill0_ts()</code>
#'
#' *Hill-Shannon diversity* - <code>hill1_map()</code> and <code>hill1_ts()</code>
#'
#' *Hill-Simpson diversity* - <code>hill2_map()</code> and <code>hill2_ts()</code>
#'
#' (see 'Details' for more information).
#'
#' @details `r hill_diversity_details`
#'
#' @references
#' Hill, M. O. (1973). Diversity and evenness: a unifying notation and its
#' consequences. *Ecology*, *54*(2), 427-432.
#'
#' Roswell, M., Shipley, J., & Ewers, R. M. (2019). A conceptual guide to
#' measuring and interpreting functional diversity.
#' *Journal of Applied Ecology*, *56*(12), 2533-2543.
#'
#' Chao, A., Gotelli, N. J., Hsieh, T. C., Sander, E. L., Ma, K. H.,
#' Colwell, R. K., & Ellison, A. M. (2014). Rarefaction and extrapolation with
#' Hill numbers: a framework for sampling and estimation in species diversity
#' studies. *Ecological monographs*, *84*(1), 45-67.
#'
#' Hsieh, T. C., Ma, K. H., & Chao, A. (2016). iNEXT: an R package for
#' rarefaction and extrapolation of species diversity (Hill numbers).
#' *Methods in Ecology and Evolution*, *7*(12), 1451-1456.
#'
#' @param data A data cube object (class 'processed_cube').
#' @param coverage (Optional) The sample coverage value for the estimator. Default is 0.95.
#' @param cutoff_length (Optional) The minimum number of data points for each grid cell.
#'  Grid cells with fewer data points will be removed before calculations to
#'  avoid errors.  Default is 5.
#' @param conf_level (Optional) Confidence level for bootstrap confidence intervals. Only
#'  applies to temporal indicators. Default is 0.95.
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'hill0' or 'hill1' or 'hill2' containing the calculated indicator values and
#' metadata.
#'
#' @describeIn hill0_map
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
                             nboot = 0,
                             conf = 0.95,
                             ...)
}


#' @describeIn hill0_map
#'
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
                     conf_level = 0.95,
                     ...) {

  dot_params <- list(...)

  # Check if num_bootstrap was provided, otherwise set a default
  num_bootstrap <- dot_params$num_bootstrap
  if (is.null(num_bootstrap)) num_bootstrap <- 100

  # Check if ci_type is provided and set to "none". If so, set num_bootstrap = 0
  ci_type <- dot_params$ci_type
  if (!is.null(ci_type) && ci_type == "none") num_bootstrap <- 0

  compute_indicator_workflow(data,
                             type = "hill0",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             conf = conf_level,
                             nboot = num_bootstrap,
                             force_grid = TRUE,
                             ...)
}


#' @describeIn hill0_map
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
                             nboot = 0,
                             conf = 0.95,
                             ...)
}


#' @describeIn hill0_map
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
                     conf_level = 0.95,
                     ...) {

  dot_params <- list(...)

  # Check if num_bootstrap was provided, otherwise set a default
  num_bootstrap <- dot_params$num_bootstrap
  if (is.null(num_bootstrap)) num_bootstrap <- 100

  # Check if ci_type is provided and set to "none". If so, set num_bootstrap = 0
  ci_type <- dot_params$ci_type
  if (!is.null(ci_type) && ci_type == "none") num_bootstrap <- 0

  compute_indicator_workflow(data,
                             type = "hill1",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             conf = conf_level,
                             nboot = num_bootstrap,
                             force_grid = TRUE,
                             ...)
}


#' @describeIn hill0_map
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
                             nboot = 0,
                             conf = 0.95,
                             ...)
}

#' @describeIn hill0_map
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
                     conf_level = 0.95,
                     ...) {

  dot_params <- list(...)

  # Check if num_bootstrap was provided, otherwise set a default
  num_bootstrap <- dot_params$num_bootstrap
  if (is.null(num_bootstrap)) num_bootstrap <- 100

  # Check if ci_type is provided and set to "none". If so, set num_bootstrap = 0
  ci_type <- dot_params$ci_type
  if (!is.null(ci_type) && ci_type == "none") num_bootstrap <- 0

  compute_indicator_workflow(data,
                             type = "hill2",
                             dim_type = "ts",
                             cutoff_length = cutoff_length,
                             coverage = coverage,
                             conf = conf_level,
                             nboot = num_bootstrap,
                             force_grid = TRUE,
                             ...)
}


#' @title Calculate Cumulative Species Richness
#'
#' @description This function calculates cumulative species richness as a time
#' series (see 'Details' for more information).
#'
#' @details
#' ## Species richness
#' Species richness is the total number of species present in a
#' sample (Magurran, 1988). It is a fundamental and commonly used
#' measure of biodiversity, providing a simple and intuitive
#' overview of the status of biodiversity. However, richness is not
#' well suited to measuring biodiversity change over time, as it
#' only decreases when local extinctions occur and thus lags behind
#' abundance for negative trends. While it may act as a leading
#' indicator of alien species invasions, it will not indicate
#' establishment because it ignores abundance. Nor will it
#' necessarily indicate changes in local species composition, which
#' can occur without any change in richness. Although richness is
#' conceptually simple, it can be measured in different ways.
#'
#' ## Cumulative richness
#' Cumulative richness is calculated by adding the newly observed unique species
#' each year to a cumulative sum. This indicator provides an estimation of
#' whether and how many new species are still being discovered in a region.
#' While an influx of alien species could cause an increase in cumulative
#' richness, a fast-rising trend as shown in Fig. 2 is likely an indication
#' that the dataset is not comprehensive and therefore observed richness will
#' provide an underestimate of species richness.
#'
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'cum_richness' containing
#' the calculated indicator values and metadata.
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


#' @title Calculate Mean Year of Occurrence Over Time or Space
#'
#' @description This function estimates the relative newness of records in a
#' data cube by calculating the mean year of occurrence over a gridded map or as
#' a time series (see 'Details' for more information).
#'
#' @details The mean year of occurrence is calculated per cell, giving an
#' indication of how recent the data is for each cell. A recent
#' mean year is not necessarily an indication of quality, as some
#' countries or regions have been conducting comprehensive
#' biodiversity monitoring for many years and will therefore
#' reflect an older mean year of occurrence, while others may show
#' a recent mean year due to e.g., the sudden availability of large
#' amounts of citizen science data.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'newness' containing the calculated indicator values and metadata.
#'
#' @describeIn newness_map
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


#' @describeIn newness_map
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


#' @title Calculate Occurrence Density Over Space or Time
#'
#' @description This function calculates the density of records over a gridded
#' map or as a time series (see 'Details' for more information).
#'
#' @details
#' Density is calculated by summing the total number of occurrences per square
#' kilometre for each cell or year. This provides similar information to total
#' occurrences, but is adjusted for cell area.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'occ_density' containing the calculated indicator values and metadata.
#'
#' @describeIn occ_density_map
#'
#' @examples
#' \dontrun{
#' od_map <- occ_density_map(example_cube_1, level = "country",
#'                                           region = "Denmark")
#' plot(od_map)
#' }
#' @export
occ_density_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "occ_density",
                             dim_type = "map",
                             ...)
}


#' @describeIn occ_density_map
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

#' @title Calculate Species Occurrences Over Space or Time
#'
#' @description This function calculates the number of occurrences for individual
#' species over a gridded map or as a time series (see 'Details' for more
#' information).
#'
#' @details
#' Species occurrences are considered an essential biodiversity variable (EBV).
#' They are mapped by calculating the total number of occurrences
#' of a given species for each cell. This represents the occurrence frequency
#' distribution, and also indicates the observed species distribution. The
#' number of occurrences can act as a proxy for relative abundance of species
#' with a similar detectability, which is an important aspect of biodiversity
#' although not an indicator when calculated in isolation.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'spec_occ' containing the calculated indicator values and metadata.
#'
#' @describeIn spec_occ_map
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

#' @describeIn spec_occ_map
#'
#' @examples
#' so_ts <- spec_occ_ts(example_cube_1, first_year = 1985)
#' plot(so_ts, c(2435767, 2434793))
#'
#' @export
spec_occ_ts <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_occ",
                             dim_type = "ts",
                             ...)
}

#' @title Plot Species Ranges Over Space or Time
#'
#' @description Plot the cells occupied for individual species over a gridded
#' map or calculate the change in the number of cells occupied as a time series.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'spec_range' containing the calculated indicator values and metadata.
#'
#' @describeIn spec_range_map
#'
#' @examples
#' sr_map <- spec_range_map(example_cube_1, level = "country", region = "Denmark")
#' plot(sr_map, c(2440728, 4265185))
#'
#' @export
spec_range_map <- function(data, ...) {
  compute_indicator_workflow(data,
                             type = "spec_range",
                             dim_type = "map",
                             ...)
}

#' @describeIn spec_range_map
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

#' @title Calculate Taxonomic Distinctness Over Space or Time
#'
#' @description This function calculates the taxonomic distinctness index (TDI)
#' over a gridded map or as a time series (see 'Details' for more information).
#'
#' @details Taxonomic distinctness is an essential biodiversity variable (EBV)
#' that measures the taxonomic relatedness between species, providing a measure
#' of biodiversity that accounts for evolutionary relationships. A distance
#' matrix based on pairwise taxonomic relationships is calculated for each cell
#' using the taxize package (Chamberlain & Sz&ouml;cs, 2013; Chamberlain et al.,
#' 2020), then taxonomic distinctness is calculated as the Taxonomic
#' Distinctness Index (TDI; Clarke & Warwick, 1999):
#' \deqn{
#'  \frac{\sum\sum_{i<j} \frac{|R_i - R_j|}{L}}{\frac{S(S-1)}{2}}
#' }{
#'  (&sum;&sum; from i<j of (|R_i-R_j| / L) / (S * (S - 1) / 2)
#' }
#' where S is the number of species, Ri and Rj are the taxonomic ranks
#' of species i and j (from the GBIF Taxonomic Backbone), and L is the
#' maximum number of taxonomic ranks.
#' The TDI ranges from 0 to 1, with higher values indicating greater
#' taxonomic distinctness.
#'
#' @references
#' Chamberlain, S. A., & Sz&ouml;cs, E. (2013). taxize: taxonomic search and
#' retrieval in R. *F1000Research*, 2.
#'
#' Chamberlain, S., Szoecs, E., Foster, Z., Boettiger, C., Ram, K., Bartomeus,
#' I., Baumgartner, J., O'Donnell, J., Oksanen, J., Tzovaras, B. G., Marchand,
#' P., Tran, V., Salmon, M., Li, G., & Greni&eacute;, M. (2020). taxize: Taxonomic
#' Information from Around the Web. R package version 0.9.98.
#' https://github.com/ropensci/taxize.
#'
#' Clarke, K. R., & Warwick, R. M. (1999). The taxonomic distinctness measure
#' of biodiversity: weighting of step lengths between hierarchical levels.
#' Marine Ecology Progress Series, 184, 21-29.
#'
#' @param data A data cube object (class 'processed_cube').
#' @param rows (Optional) Choose which row to select if there are multiple matches when
#' retrieving taxonomic information from GBIF. (Default is 1. Use NA for
#' interactive mode.)
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_map' or 'indicator_ts' and
#' 'tax_distinct' containing the calculated indicator values and metadata.
#'
#' @describeIn tax_distinct_map
#'
#' @examples
#' \dontrun{
#' td_map <- tax_distinct_map(example_cube_1, level = "country", region = "Denmark")
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

#' @describeIn tax_distinct_map
#'
#' @examples
#' \dontrun{
#' td_ts <- tax_distinct_ts(example_cube_1, level = "country", region = "Denmark")
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

#' @title Calculate Occupancy Turnover
#'
#' @description This function calculates occupancy turnover as a time series
#' (see 'Details' for more information).
#'
#' @details
#' Occupancy turnover measures the change in species composition over time,
#' reflecting the rate at which species appear or disappear from a given area.
#' It provides insights into the dynamic nature of ecological communities,
#' highlighting shifts in species distributions and potential environmental
#' changes. High turnover rates may indicate rapid community restructuring,
#' potentially driven by factors such as habitat alteration, climate change,
#' or invasive species. Analyzing occupancy turnover can be crucial for
#' understanding ecosystem stability, identifying areas of conservation concern,
#' and assessing the effectiveness of management strategies.
#'
#' Occupancy turnover can be calculated in different ways, but here we use the
#' Jaccard dissimilarity index (Jaccard, 1901) to measure the similarity between
#' two sets of species occurrences. The Jaccard index is calculated as:
#'
#' \deqn{
#'   J = (b + c) / (a + b + c)
#' }{
#'   J = (b + c) / (a + b + c)
#' }
#'
#' where a is the number of species present in both time periods, b is the
#' number of species present only in the first time period, and c is the number
#' of species present only in the second time period. The index ranges
#' from 0 (no turnover) to 1 (complete turnover).
#'
#' @references
#' Jaccard, P. (1901). &Eacute;tude de la distribution florale dans une portion
#' des Alpes et du Jura. *Bulletin de la Soci&eacute;t&eacute; Vaudoise des*
#' *Science Naturelles*, *37*(142), 547-579.
#'
#' @param data A data cube object (class 'processed_cube').
#'
#' @inheritDotParams compute_indicator_workflow -type -dim_type -data
#'
#' @seealso compute_indicator_workflow
#'
#' @return An S3 object with the classes 'indicator_ts' and 'occ_turnover'
#' containing the calculated indicator values and metadata.
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
