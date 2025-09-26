#' @title Plot Occurrence Trends or Range Size Trends for Individual Species
#'
#' @description  Creates time series plots of species occurrences or species
#'  range sizes, with an optional smoothed trendline, and visualizes
#'  uncertainty. Requires an indicator_ts object created using the
#'  \code{spec_occ_ts()} or \code{spec_range_ts()} functions as input. To plot
#'  multi-species indicators (e.g., species richness or evenness), use the
#'  \code{plot_ts()} function instead.
#'
#' @inheritParams plot_ts
#'
#' @param x An 'indicator_ts' object containing time series of indicator values
#'  matched to species names and/or taxon keys, created using the
#'  \code{spec_occ_ts()} or \code{spec_range_ts()} functions. This is a required
#'  parameter with no default.
#' @param species Species you want to map occurrences for. Can be either
#'  numerical taxonKeys or species names. Partial species names can be used
#'  (the function will try to match them). This is a required parameter with
#'  no default.
#' @param single_plot (Optional) By default all species occurrence time series
#'  will be combined into a single multi-panel plot. Set this to FALSE to plot
#'  each species separately.
#' @param spec_name_wrap_length (Optional) Maximum species name length before
#'  wrapping to a new line.
#'
#' @return A ggplot object representing species range or occurrence time series
#'  plot(s). Can be customized using ggplot2 functions.
#'
#' @examples
#' spec_occ_ts_mammals_denmark <- spec_occ_ts(example_cube_1,
#'                                         level = "country",
#'                                         region = "Denmark")
#' # default colours:
#' plot_species_ts(spec_occ_ts_mammals_denmark, c(2440728, 4265185))
#'
#' # custom colours:
#' plot_species_ts(spec_occ_ts_mammals_denmark, c(2440728, 4265185),
#'         linecolour = "thistle",
#'         trendlinecolour = "forestgreen",
#'         envelopecolour = "lightgreen")
#' @export
plot_species_ts <- function(x,
                            species,
                            single_plot = TRUE,
                            min_year = NULL,
                            max_year = NULL,
                            title = "auto",
                            auto_title = NULL,
                            y_label_default = NULL,
                            suppress_y = FALSE,
                            smoothed_trend = TRUE,
                            linecolour = NULL,
                            linealpha = 0.8,
                            ribboncolour = NULL,
                            ribbonalpha = 0.2,
                            error_alpha = 1,
                            trendlinecolour = NULL,
                            trendlinealpha = 0.5,
                            envelopecolour = NULL,
                            envelopealpha = 0.2,
                            smooth_cialpha = 1,
                            point_line = c("point",
                                           "line"),
                            pointsize = 2,
                            linewidth = 1,
                            ci_type = c("error_bars",
                                        "ribbon"),
                            error_width = 1,
                            error_thickness = 1,
                            smooth_linetype = c("solid",
                                                "dashed",
                                                "dotted",
                                                "dotdash",
                                                "longdash",
                                                "twodash"),
                            smooth_linewidth = 1,
                            smooth_cilinewidth = 1,
                            gridoff = FALSE,
                            x_label = NULL,
                            y_label = NULL,
                            x_expand = 0.1,
                            y_expand = 0.1,
                            x_breaks = 10,
                            y_breaks = 6,
                            title_wrap_length = 60,
                            spec_name_wrap_length = 40
) {

  # Set variable definitions to NULL where required
  year <- ll <- ul <- diversity_val <- NULL

  # Match arguments
  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)

  # Check that object to plot is the correct class
  wrong_class(x, "indicator_ts", reason = "incorrect")

  # Filter by min and max year if set
  filtered_data <- filter_data_years(x, min_year, max_year)

  # Unpack filtered data and year range
  x$data <- filtered_data$data
  min_year <- filtered_data$min_year
  max_year <- filtered_data$max_year

  # Check there are at least 3 years to plot if smoothed trend is requested
  if ((max_year - min_year) < 2 && smoothed_trend == TRUE) {
    smoothed_trend <- FALSE
    message("Could not perform loess smooth due to insufficient time points.")
  }

  # Check that species argument has been provided
  if (is.null(species)) {
    stop(paste0(
      "Please enter either the species names or the numeric taxonKeys for the ",
      "species you want to plot."
    ))
  }

  # Split data by species
  split_so <- if (is.numeric(species)) {
    get_occurrences_and_split(x$data, "taxonKey", species)
  } else {
    get_occurrences_and_split(x$data, "scientificName", species)
  }

  # Get unique species names for plot labelling
  sci_names <- split_so %>% purrr::map(function(x) unique(x$scientificName))

  # Create plot title if title is set to "auto"
  if (!is.null(title) && title == "auto") {
    title <- create_auto_title(auto_title, min_year, max_year)
  }

  # Set some defaults for plotting
  plot_options <- set_ts_plot_options(
    linecolour, ribboncolour, trendlinecolour, envelopecolour,
    x_label, y_label, y_label_default, x_expand, y_expand
  )

  # Unpack the default options
  list2env(plot_options, envir = environment())

  # Adjust error bar width according to number of years being plotted
  error_width <- (error_width * (max_year - min_year)) / 100

  # Create the plots for each species
  plot <- purrr::map(seq_along(sci_names), function(i) {
    create_ts_plot(
      data = split_so[[i]],
      point_line = point_line,
      ci_type = ci_type,
      smoothed_trend = smoothed_trend,
      pointsize = pointsize,
      linecolour = linecolour,
      linealpha = linealpha,
      linewidth = linewidth,
      ribboncolour = ribboncolour,
      ribbonalpha = ribbonalpha,
      error_alpha = error_alpha,
      error_width = error_width,
      error_thickness = error_thickness,
      trendlinecolour = trendlinecolour,
      trendlinealpha = trendlinealpha,
      envelopecolour = envelopecolour,
      envelopealpha = envelopealpha,
      smooth_cialpha = smooth_cialpha,
      smooth_linewidth = smooth_linewidth,
      smooth_cilinewidth = smooth_cilinewidth,
      smooth_linetype = smooth_linetype,
      gridoff = gridoff,
      suppress_y = suppress_y,
      x_label = x_label,
      y_label = y_label,
      title_label = wrapper(sci_names[[i]], spec_name_wrap_length),
      title_face = "italic",
      x_expand = x_expand,
      y_expand = y_expand,
      x_breaks = x_breaks,
      y_breaks = y_breaks
    )
  })

  # Name each plot with the corresponding species name
  names(plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if ((length(plot) > 0 && single_plot == TRUE) || length(plot) == 1) {
    plot <- patchwork::wrap_plots(plot) +
      plot_annotation_int(title = wrapper(title, title_wrap_length),
                          theme = theme(plot.title = element_text(size = 20)))
    # Or create each plot separately if single_plot is FALSE
  } else if (length(plot) > 1 && single_plot == FALSE) {
    message(paste0(
      "Option single_plot set to false. Creating separate plot for each ",
      "species.\n\n"
    ))
  }

  # Exit function
  return(plot)

}
