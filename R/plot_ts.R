#' @title Plot Biodiversity Indicator Trend
#'
#' @description  Creates a time series plot of a calculated multi-species
#'  biodiversity indicator (e.g. species richness or evenness), with an optional
#'  smoothed trendline, and visualizes uncertainty. Requires an 'indicator_ts'
#'  object as input. To plot a single-species indicator time series, use the
#'  \code{plot_species_ts()} function instead.
#'
#' @param x An 'indicator_ts' object containing a time series of multi-species
#'  indicator values. This is a required parameter with no default.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default (Optional) Default label for the y-axis, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y (Optional) If TRUE, suppresses y-axis labels.
#' @param smoothed_trend (Optional) If TRUE, plot a smoothed trendline over time
#' (`stats::loess()`).
#' @param linecolour (Optional) Colour for the indicator line or points.
#'   Default is darkorange.
#' @param linealpha (Optional) Transparency for indicator line or points.
#'  Default is 0.8.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence
#'  intervals. Default is goldenrod1. Set to "NA" if you don't want to plot the
#'  CIs.
#' @param ribbonalpha (Optional) Transparency for indicator confidence interval
#'  ribbon (if ci_type = "ribbon"). Default is 0.2.
#' @param error_alpha (Optional) Transparency for indicator error bars (if
#'  ci_type = "error_bar"). Default is 1.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param trendlinealpha (Optional) Transparency for the smoothed trendline.
#'  Default is 0.5.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param envelopealpha (Optional) Transparency for the smoothed trendline
#'  envelope. Default is 0.2.
#' @param smooth_cialpha (Optional) Transparency for the smoothed lines forming
#'  the edges of the trendline envelope. Default is 1.
#' @param point_line (Optional) Whether to plot the indicator as a line or a
#'  series of points. Options are "line" or "point". Default is "point".
#' @param pointsize (Optional) Size of the points if point_line = "point".
#'  Default is 2.
#' @param linewidth (Optional) Width of the line if point_line = "line".
#'  Default is 1.
#' @param ci_type (Optional) Whether to plot bootstrapped confidence intervals
#'  as a "ribbon" or "error_bars". Default is "error_bars".
#' @param error_width (Optional) Width of error bars if ci_type = "error_bars".
#'  Default is 1. Note that unlike the default 'width' parameter in
#'  geom_errorbar, 'error_width' is NOT dependent on the number of data points
#'  in the plot. It is automatically scaled to account for this. Therefore the
#'  width you select will be consistent relative to the plot width even if you
#'  change 'min_year' and 'max_year'.
#' @param error_thickness (Optional) Thickness of error bars if
#'  ci_type = "error_bars". Default is 1.
#' @param smooth_linetype (Optional) Type of line to plot for smoothed
#'  trendline. Default is "solid".
#' @param smooth_linewidth (Optional) Line width for smoothed trendline.
#'  Default is 1.
#' @param smooth_cilinewidth (Optional) Line width for smoothed trendline
#'  confidence intervals. Default is 1.
#' @param gridoff (Optional) If TRUE, hides gridlines.
#' @param x_label (Optional) Label for the x-axis.
#' @param y_label (Optional) Label for the y-axis.
#' @param x_expand (Optional)  Expansion factor to expand the x-axis beyond the
#'  data. Left and right values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to each side. Default is 0.05.
#' @param y_expand (Optional)  Expansion factor to expand the y-axis beyond the
#'  data. Lower and upper values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to the top and bottom. Default is 0.05.
#' @param x_breaks (Optional) Integer giving desired number of breaks for x
#'  axis. (May not return exactly the number requested.)
#' @param y_breaks (Optional) Integer giving desired number of breaks for y
#'  axis. (May not return exactly the number requested.)
#' @param title_wrap_length (Optional) Maximum title length before wrapping to
#'  a new line.
#'
#' @return A ggplot object representing the biodiversity indicator time series
#'  plot. Can be customized using ggplot2 functions.
#'
#' @examples
#' # default colours:
#' plot_ts(example_indicator_ts1,
#'         y_label = "Species Richness",
#'         title = "Observed Species Richness: Mammals in Denmark")
#'
#' # custom colours:
#' plot_ts(example_indicator_ts1,
#'         y_label = "Species Richness",
#'         title = "Observed Species Richness: Mammals in Denmark",
#'         linecolour = "thistle",
#'         trendlinecolour = "forestgreen",
#'         envelopecolour = "lightgreen")
#' @export
plot_ts <- function(x,
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
                    x_expand = 0.05,
                    y_expand = 0.05,
                    x_breaks = 10,
                    y_breaks = 6,
                    title_wrap_length = 60
) {

  # Set variable definitions to NULL where required
  year <- diversity_val <- ul <- ll <- NULL

  # Match arguments
  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)

  # Check that object to plot is the correct class
  wrong_class(x, "indicator_ts", reason = "incorrect")

  # Filter by min and max year if set
  filtered_data <- filter_data_years(x, min_year, max_year)

  # Retrieve filtered data and year range
  x$data <- filtered_data$data
  min_year <- filtered_data$min_year
  max_year <- filtered_data$max_year

  # Check there are at least 3 years to plot if smoothed trend is requested
  if ((max_year - min_year) < 2 && smoothed_trend == TRUE) {
    smoothed_trend <- FALSE
    message("Could not perform loess smooth due to insufficient time points.")
  }

  # Create plot title if title is set to "auto"
  if (!is.null(title) && title == "auto") {
    title <- create_auto_title(auto_title, min_year, max_year)
  }

  # Set some default options for plotting
  plot_options <- set_ts_plot_options(
    linecolour, ribboncolour, trendlinecolour, envelopecolour,
    x_label, y_label, y_label_default, x_expand, y_expand
  )

  # Unpack the default options
  list2env(plot_options, envir = environment())

  # Adjust error bar width according to number of years being plotted
  error_width <- (error_width * (max_year - min_year)) / 100

  # Create the plot
  plot <- create_ts_plot(
    data = x$data,
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
    smooth_linetype = smooth_linetype,
    smooth_linewidth = smooth_linewidth,
    smooth_cilinewidth = smooth_cilinewidth,
    gridoff = gridoff,
    suppress_y = suppress_y,
    x_label = x_label,
    y_label = y_label,
    title_label = wrapper(title, title_wrap_length),
    title_face = "plain",
    x_expand = x_expand,
    y_expand = y_expand,
    x_breaks = x_breaks,
    y_breaks = y_breaks
  )

  # Exit function
  return(plot)

}
