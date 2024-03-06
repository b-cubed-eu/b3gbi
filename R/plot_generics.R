#' @title Plot Occurrences Segregated by Dataset
#'
#' @description  Creates a time series plot of total occurrences, with the
#'   occurrences visually segregated by their dataset of origin. Requires an
#'   'indicator_ts' object.
#'
#' @param x An object containing occurrence data segregated by dataset. Must
#'   be of class 'occ_by_dataset' and an 'indicator_ts' object.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'    segregated by dataset.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'occ_by_dataset_ts'
#' plot.occ_by_dataset(occ_by_dataset_ts)
#'
#' @export
plot.occ_by_dataset <- function(x,
                                facets = FALSE,
                                facet_scales = "free",
                                facet_rows = NULL,
                                facet_cols = NULL,
                                facet_label_width = 60,
                                ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_dataset'.", inherits(x, "occ_by_dataset"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Total Occurrences (Segregated by Dataset)"

    # Set type as a factor and remove any types with only 1 occurrence
    x$data$type <- factor(x$data$type, levels = unique(x$data$type))
    x$data <-
      x$data %>%
      dplyr::filter(length(.) > 1, .by = type)

    # Call generalized plot_map function
    trend_plot <- plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

    if (facets == TRUE) {

      # Use facets to separate multiple species trends
      trend_plot <- trend_plot +
        facet_wrap(vars(type),
                   scales = facet_scales,
                   nrow = facet_rows,
                   ncol = facet_cols,
                   labeller = label_wrap_gen(width = facet_label_width)) +
        theme(legend.position = "none")

    }

    # Show plot
    trend_plot

}

#' @title Plot Occurrences Segregated by Type
#'
#' @description  Creates a time series plot of total occurrences, with the
#'   occurrences visually segregated by their type. Requires an 'indicator_ts'
#'   object.
#'
#' @param x An object containing  occurrence data segregated by type. Must
#'   be of class 'occ_by_type' and an 'indicator_ts' object.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'    segregated by type.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'occ_by_type_ts'
#' plot.occ_by_type(occ_by_type_ts)
#'
#' @export
plot.occ_by_type <- function(x,
                             facets = FALSE,
                             facet_scales = "free",
                             facet_rows = NULL,
                             facet_cols = NULL,
                             facet_label_width = 60,
                             ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_type'.", inherits(x, "occ_by_type"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Total Occurrences (Segregated by Type)"

    # Set type as a factor and remove any types with only 1 occurrence
    x$data$type <- factor(x$data$type, levels = unique(x$data$type))
    x$data <-
      x$data %>%
      dplyr::filter(length(.) > 1, .by = type)

    # Call generalized plot_map function
    trend_plot <- plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

    if (facets == TRUE) {

      # Use facets to separate multiple species trends
      trend_plot <- trend_plot +
        facet_wrap(vars(type),
                   scales = facet_scales,
                   nrow = facet_rows,
                   ncol = facet_cols,
                   labeller = label_wrap_gen(width = facet_label_width)) +
        theme(legend.position = "none")

    }

    # Show plot
    trend_plot


}

#' @title Plot Cumulative Species Richness
#'
#' @description  Plots cumulative species richness data as a time series
#'   using an 'indicator_ts' object.
#'
#' @param x An object containing cumulative species richness data. Must be of
#'   class 'cum_richness' and an 'indicator_ts' object.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_ts`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot of cumulative
#'    species richness.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'cum_richness_ts'
#' plot.cum_richness(cum_richness_ts)
#'
#' @export
plot.cum_richness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'cum_richness'.", inherits(x, "cum_richness"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Cumulative Species Richness"
    auto_title <- "Cumulative Species Richness"

    # Call generalized plot_map function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

}

#' @title Plot Evenness
#'
#' @description  Plots evenness data (either Pielou's Evenness or Williams' Evenness),
#'    either as a time series ('indicator_ts' object)  or as a spatial map
#'    ('indicator_map' object).
#'
#' @param x An object containing evenness data. Must be of class 'evenness',
#'   either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming objects 'evenness_ts' (indicator_ts) and 'evenness_map' (indicator_map)
#' # Time series plot:
#' plot.evenness(evenness_ts)
#'
#' # Map visualization:
#' plot.evenness(evenness_map)
#'
#' @export
plot.evenness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'evenness'.", inherits(x, "evenness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste(ifelse((attr(x, "indicator_id")=="pielou_evenness"), "Pielou's", "Williams'"), " Evenness Trend", sep = "")

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Evenness"
  auto_title <- paste(ifelse((attr(x, "indicator_id")=="pielou_evenness"), "Pielou's", "Williams'"), " Evenness", sep = "")

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Taxonomic Distinctness
#'
#' @description  Plots taxonomic distinctness data, either as a time series
#'   ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing taxonomic distinctness data. Must be of
#'   class 'tax_distinct', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming objects 'tax_distinct_ts' (indicator_ts) and 'tax_distinct_map' (indicator_map)
#' # Time series plot:
#' plot.tax_distinct(tax_distinct_ts)
#'
#' # Map visualization:
#' plot.tax_distinct(tax_distinct_map)
#'
#' @export
plot.tax_distinct <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'tax_distinct'.", inherits(x, "tax_distinct"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Taxonomic Distinctness"
    auto_title <- "Taxonomic Distinctness Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Taxonomic Distinctness"
  auto_title <- "Taxonomic Distinctness"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Occurrence Density
#'
#' @description  Plots occurrence density data, either as a time series
#'   ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing occurrence density data. Must be of
#'   class 'density', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming objects 'density_ts' (indicator_ts) and 'density_map' (indicator_map)
#' # Time series plot:
#' plot.density(density_ts)
#'
#' # Map visualization:
#' plot.density(density_map)
#'
#' @export
plot.density <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'density'.", inherits(x, "density"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences \nper km^2"
    auto_title <- "Trend of Occurrence Density"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences \nper km^2"
    auto_title <- "Density of Occurrences"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Mean Year of Occurrence
#'
#' @description  Creates a spatial map visualization of the mean year of
#'    species occurrences.
#'
#' @param x An object containing mean year of occurrence associated with map
#'   grid cells. Must be of class 'newness' and an 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_map`). See its documentation for details.
#'
#' @return A ggplot object representing a map of the mean year of occurrences.
#'
#' @examples
#' # Assuming you have an 'indicator_map' object named 'newness_map'
#' plot.newness(newness_map)
#'
#' @export
plot.newness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'newness'.", inherits(x, "newness"))

  if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Mean Year of \nOccurrence"
    auto_title <- "Mean Year of Occurrence"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_map'.")

  }
}

#' @title Plot Total Occurrences
#'
#' @description  Plots summed occurrence data, either as a time series
#'   ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing summed occurrence data. Must be of
#'   class 'total_occ', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'total_occ_ts' (indicator_ts) and 'total_occ_map' (indicator_map)
#' # Time series plot:
#' plot.total_occ(total_occ_ts)
#'
#' # Map visualization:
#' plot.total_occ(total_occ_map)
#'
#' @export
plot.total_occ <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'total_occ'.", inherits(x, "total_occ"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Trend of Total Occurrences"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Occurrences"
  auto_title <- "Total Occurrences"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Area-Based Rarity
#'
#' @description  Plots area-based rarity data, either as a time series
#'   ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing area-based rarity data. Must be of
#'   class 'area_rarity', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'area_rarity_ts' (indicator_ts) and 'area_rarity_map' (indicator_map)
#' # Time series plot:
#' plot.area_rarity(area_rarity_ts)
#'
#' # Map visualization:
#' plot.area_rarity(area_rarity_map)
#'
#' @export
plot.area_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'area_rarity'.", inherits(x, "area_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Rarity"
    auto_title <- "Area-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Rarity"
  auto_title <- "Area-Based Rarity"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Abundance-Based Rarity
#'
#' @description  Plots abundance-based rarity data, either as a time series
#'   ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing abundance-based rarity data. Must be of
#'   class 'ab_rarity', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'ab_rarity_ts' (indicator_ts) and 'ab_rarity_map' (indicator_map)
#' # Time series plot:
#' plot.ab_rarity(ab_rarity_ts)
#'
#' # Map visualization:
#' plot.ab_rarity(ab_rarity_map)
#'
#' @export
plot.ab_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'ab_rarity'.", inherits(x, "ab_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Rarity"
    auto_title <- "Abundance-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Rarity"
  auto_title <- "Abundance-Based Rarity"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Rarefied Species Richness
#'
#' @description  Plots rarefied species richness data (estimated by sample
#'   size-based rarefaction), either as a time series ('indicator_ts' object)
#'   or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing rarefied species richness data. Must be of
#'   class 'rarefied', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'rarefied_ts' (indicator_ts) and 'rarefied_map' (indicator_map)
#' # Time series plot:
#' plot.rarefied(rarefied_ts)
#'
#' # Map visualization:
#' plot.rarefied(rarefied_map)
#'
#' @export
plot.rarefied <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'rarefied'.", inherits(x, "rarefied"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness Index"
    auto_title <- "Indexed Species Richness Trend (Estimated by Sample Size-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Richness"
  auto_title <- "Species Richness (Estimated by Sample Size-Based Rarefaction)"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Hill-Simpson Diversity Index (Hill Number 2)
#'
#' @description  Plots Hill-Simpson diversity index data (estimated by coverage-based
#'   rarefaction), either as a time series ('indicator_ts' object)  or as a
#'   spatial map ('indicator_map' object).
#'
#' @param x An object containing Hill-Simpson diversity index (Hill number 2) data.
#'   Must be of class 'hill2', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'hill2_ts' (indicator_ts) and 'hill2_map' (indicator_map)
#' # Time series plot:
#' plot.hill2(hill2_ts)
#'
#' # Map visualization:
#' plot.hill2(hill2_map)
#'
#' @export
plot.hill2 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill2'.", inherits(x, "hill2"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Diversity Index"
    auto_title <- "Indexed Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Simpson \nDiversity"
    auto_title <- "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Hill-Shannon Diversity Index (Hill Number 1)
#'
#' @description  Plots Hill-Shannon diversity index data (estimated by coverage-based
#'   rarefaction), either as a time series ('indicator_ts' object)  or as a
#'   spatial map ('indicator_map' object).
#'
#' @param x An object containing Hill-Shannon diversity index (Hill number 1) data.
#'   Must be of class 'hill1', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'hill1_ts' (indicator_ts) and 'hill1_map' (indicator_map)
#' # Time series plot:
#' plot.hill1(hill1_ts)
#'
#' # Map visualization:
#' plot.hill1(hill1_map)
#'
#' @export
plot.hill1 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill1'.", inherits(x, "hill1"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Diversity Index"
    auto_title <- "Indexed Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Shannon \nDiversity"
    auto_title <- "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Species Richness Index (Hill Number 0)
#'
#' @description  Plots species richness index data (estimated by coverage-based
#'   rarefaction), either as a time series ('indicator_ts' object)  or as a
#'   spatial map ('indicator_map' object).
#'
#' @param x An object containing species richness index (Hill number 0) data.
#'   Must be of class 'hill0', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'hill0_ts' (indicator_ts) and 'hill0_map' (indicator_map)
#' # Time series plot:
#' plot.hill0(hill0_ts)
#'
#' # Map visualization:
#' plot.hill0(hill0_map)
#'
#' @export
plot.hill0 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill0'.", inherits(x, "hill0"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness Index"
    auto_title <- "Indexed Species Richness Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Species Richness (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Observed Species Richness
#'
#' @description  Plots observed species richness data, either as a time series
#'    ('indicator_ts' object)  or as a spatial map ('indicator_map' object).
#'
#' @param x An object containing observed species richness data. Must be of
#'   class 'obs_richness', either an 'indicator_ts' or 'indicator_map' object.
#' @param ... Additional arguments passed to the internal plotting functions
#'   (`plot_ts` or `plot_map`). See their documentation for details.
#'
#' @return A ggplot object representing either a time series plot or a map,
#'   depending on the class of the input object `x`.
#'
#' @examples
#' # Assuming you have objects 'richness_ts' (indicator_ts) and 'richness_map' (indicator_map)
#' # Time series plot:
#' plot.obs_richness(richness_ts)
#'
#' # Map visualization:
#' plot.obs_richness(richness_map)
#'
#' @export
plot.obs_richness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'obs_richness'.", inherits(x, "obs_richness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <- "Observed Species Richness Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Observed Species Richness"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @title Plot Biodiversity Indicator Map
#'
#' @description Creates a map visualization of a calculated biodiversity indicator,
#'   providing customization options.
#'
#' @param x An 'indicator_map' object containing indicator values associated with
#'   map grid cells.
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default Default label for the legend, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param xlims  (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param Europe_crop  If TRUE, crops maps of Europe to exclude far-lying islands.
#' @param surround  If TRUE, includes surrounding countries in gray when plotting
#'    at the country level.
#' @param panel_bg  (Optional) Background color for the map panel.
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param wrap_length  Maximum title length before wrapping to a new line.
#'
#' @return A ggplot object representing the biodiversity indicator map.
#'
#' @examples
#' # Assuming you have an 'indicator_map' object named 'richness_map':
#' plot_map(x = richness_map, title = "Map of Species Richness")
#'
#' @export
plot_map <- function(x,
                     title = "auto",
                     auto_title = NULL,
                     leg_label_default = NULL,
                     xlims = NULL,
                     ylims = NULL,
                     trans = NULL,
                     breaks = NULL,
                     labels = NULL,
                     Europe_crop = TRUE,
                     surround = TRUE,
                     panel_bg = NULL,
                     legend_title = NULL,
                     legend_limits = NULL,
                     wrap_length = 60) {

  # Get map limits
  map_lims <- x$coord_range

  # Crop map of Europe to leave out far-lying islands (if flag set)
  if (Europe_crop == TRUE &
      x$map_level == "continent" &
      x$map_region == "Europe")
  {

    # Set attributes as spatially constant to avoid warnings
    sf::st_agr(x$data) <- "constant"

    # Manually set cropped limits
    map_lims <- c(2600000, 1600000, 7000000, 6000000)
    names(map_lims) <- c("xmin", "ymin", "xmax", "ymax")

  }

  # Set specific instructions for country-level plots
  if (x$map_level == "country")
  {

    # Get world data to plot surrounding land if surround flag is set
    if (surround == TRUE) {
      map_surround <- rnaturalearth::ne_countries(scale = "medium") %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "EPSG:3035")

      # Otherwise make the ocean area white (unless a colour is specified)
    } else {
      if (is.null(panel_bg)) { panel_bg = "white" }
    }
  }

  # Get plot title (if set to "auto")
  if (!is.null(title)) {
    if (title == "auto") {
      title <- auto_title
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }

  # Plot map
  diversity_plot <- ggplot2::ggplot(x$data) +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            colour = "black") +
    cust_leg(list(trans = trans,
                  breaks = breaks,
                  labels = labels,
                  limits = legend_limits)) +
    coord_sf(
      xlim = c(map_lims["xmin"],
               map_lims["xmax"]),
      ylim = c(map_lims["ymin"],
               map_lims["ymax"])
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = if(!is.null(panel_bg)) panel_bg
                                      else "#92c5f0"),
      if(x$map_level == "country") {
        panel.grid.major = element_blank()
        panel.grid.minor = element_blank()
      }
    ) +
    labs(fill = if(!is.null(legend_title)) legend_title
         else leg_label_default)


  # If surround flag set, add surrounding countries to map
  if (surround == TRUE & x$map_level == "country") {
    diversity_plot$layers <- c(geom_sf(data = map_surround, fill = "grey85")[[1]], diversity_plot$layers)
  }

  # Check for custom x and y limits and adjust map if found
  if(any(!is.null(xlims)) & any(!is.null(ylims))) {
    diversity_plot <-
      diversity_plot + coord_sf(xlim = xlims,
                                ylim = ylims)

  }

  # Wrap title if longer than wrap_length
  if(!is.null(title)) {
    wrapper <- function(x, ...)
    {
      paste(strwrap(x, ...), collapse = "\n")
    }
    diversity_plot <-
      diversity_plot +
      labs(title = wrapper(title, wrap_length))
  }

  # Exit function
  return(diversity_plot)

}


#' @title Plot Biodiversity Indicator Trend
#'
#' @description  Creates a time series plot of a calculated biodiversity
#'   indicator, with an optional smoothed trendline, and visualizes uncertainty.
#'
#' @param x An 'indicator_ts' object containing a time series of indicator values.
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default Default label for the y-axis, provided by an appropriate
#'   S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y If TRUE, suppresses y-axis labels.
#' @param linecolour (Optional) Colour for the indicator line.
#'   Default is darkorange.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param auccolour (Currently unused). Colour for the area under the curve of a
#'   cumulative richness plot. Default is orange.
#' @param gridoff  If TRUE, hides gridlines.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param wrap_length  Maximum title length before wrapping to a new line.
#'
#' @return A ggplot object representing the biodiversity indicator time series plot.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'richness_ts' and want
#' default colours:
#' plot_ts(x = richness_ts, title = "Time Series of Species Richness")
#'
#' # For custom colors:
#' plot_ts(x = richness_ts,
#'         title = "Time Series of Species Richness",
#'         linecolour = "thistle",
#'         trendlinecolour = "forestgreen",
#'         envelopecolour = "lightgreen")
#'
#' @export
plot_ts <- function(x,
                    title = "auto",
                    auto_title = NULL,
                    y_label_default = NULL,
                    suppress_y = FALSE,
                    linecolour = NULL,
                    trendlinecolour = NULL,
                    envelopecolour = NULL,
                    auccolour = NULL,
                    gridoff = FALSE,
                    x_label = NULL,
                    y_label = NULL,
                    min_year = NULL,
                    max_year = NULL,
                    wrap_length = 60) {

  # Filter by min and max year if set
  if (!is.null(min_year) | !is.null(max_year)) {
    min_year <- ifelse(is.null(min_year), x$first_year, min_year)
    max_year <- ifelse(is.null(max_year), x$last_year, max_year)
    x$data <-
      x$data %>%
      dplyr::filter(year >= min_year) %>%
      dplyr::filter(year <= max_year)
  }

  # Create plot title if title is set to "auto"
  if (!is.null(title)) {
    if (title == "auto") {
      title <- paste(auto_title,
                     " (",
                     min_year,
                     "-",
                     max_year,
                     ")",
                     sep="")
    }
  }

  # Set some defaults for plotting

  # Set colours
  if (is.null(linecolour)) linecolour = "darkorange"
  if (is.null(trendlinecolour)) trendlinecolour = "blue"
  if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"
  if (is.null(auccolour)) auccolour = "orange"

  # Set axis titles
  if (is.null(x_label)) x_label = "Year"
  if (is.null(y_label)) y_label = y_label_default

  # Create plot with trend line
  trend_plot <-
    ggplot2::ggplot(x$data, aes(x = year,
                                y = diversity_val)) +
    geom_line(colour = linecolour,
              lwd = 1) +
    scale_x_continuous(breaks = breaks_pretty_int(n=10)) +
    scale_y_continuous(breaks = breaks_pretty_int(n = 6)) +
    labs(x = x_label, y = y_label,
         title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          panel.grid.major = if (gridoff == TRUE) {
            element_blank()
          } else {
            element_line()
          },
          panel.grid.minor = element_blank(),
          axis.text.y = if (suppress_y==TRUE) {
            element_blank()
          } else {
            element_text()
          },
          strip.text = element_text(face = "italic")
    )

  # Add a smoothed trend
  trend_plot <- trend_plot +
    geom_smooth(fill = envelopecolour,
                lwd = 1,
                linetype = 0,
                method = "loess",
                formula = "y ~ x") +
    stat_smooth(colour = trendlinecolour,
                geom = "line",
                method = "loess",
                formula = "y ~ x",
                linetype = "dashed",
                alpha = 0.3,
                lwd = 1)

  # Wrap title if longer than wrap_length
  if(!is.null(title)) {
    wrapper <- function(x, ...)
    {
      paste(strwrap(x, ...), collapse = "\n")
    }
    trend_plot <-
      trend_plot +
      labs(title = wrapper(title, wrap_length))
  }

  # Show plot
  trend_plot

}
