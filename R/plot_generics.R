#' @export
plot.occ_by_dataset <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_dataset'.", inherits(x, "occ_by_dataset"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Total Occurrences (Segregated by Dataset)"

    # Call generalized plot_map function
    plot_ts_seg(x, y_label_default = y_label_default, auto_title = auto_title, ...)

}

#' @export
plot.occ_by_type <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_type'.", inherits(x, "occ_by_type"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Total Occurrences (Segregated by Type)"

    # Call generalized plot_map function
    plot_ts_seg(x, y_label_default = y_label_default, auto_title = auto_title, ...)

}

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

#' @export
plot.evenness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'evenness'.", inherits(x, "evenness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste(ifelse((attr(x, "indicator_id")=="pielou_evenness"), "Pielou's", "E9"), " Evenness Trend", sep = "")

    # Call generalized plot_map function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Evenness"
  auto_title <- paste(ifelse((attr(x, "indicator_id")=="pielou_evenness"), "Pielou's", "E9"), " Evenness", sep = "")

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.tax_distinct <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'tax_distinct'.", inherits(x, "tax_distinct"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Taxonomic Distinctness"
    auto_title <- "Taxonomic Distinctness Trend"

    # Call generalized plot_map function
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

#' @export
plot.density <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'density'.", inherits(x, "density"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences \nper km^2"
    auto_title <- "Trend of Occurrence Density"

    # Call generalized plot_map function
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

#' @export
plot.total_occ <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'total_occ'.", inherits(x, "total_occ"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Trend of Total Occurrences"

    # Call generalized plot_map function
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

#' @export
plot.area_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'area_rarity'.", inherits(x, "area_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Rarity"
    auto_title <- "Area-Based Rarity Trend"

    # Call generalized plot_map function
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

#' @export
plot.ab_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'ab_rarity'.", inherits(x, "ab_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Rarity"
    auto_title <- "Abundance-Based Rarity Trend"

    # Call generalized plot_map function
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

#' @export
plot.rarefied <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'rarefied'.", inherits(x, "rarefied"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness Index"
    auto_title <- "Indexed Species Richness Trend (Estimated by Sample Size-Based Rarefaction)"

    # Call generalized plot_map function
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

#' @export
plot.hill2 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill2'.", inherits(x, "hill2"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Diversity Index"
    auto_title <- "Indexed Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
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

#' @export
plot.hill1 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill1'.", inherits(x, "hill1"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Diversity Index"
    auto_title <- "Indexed Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
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

#' @export
plot.hill0 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill0'.", inherits(x, "hill0"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness Index"
    auto_title <- "Indexed Species Richness Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
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

#' @export
plot.obs_richness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'obs_richness'.", inherits(x, "obs_richness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <- "Observed Species Richness Trend"

    # Call generalized plot_map function
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

#' @export
# Plot observed species richness
plot_map <- function(x,
                     leg_label_default = NULL,
                     auto_title = NULL,
                     xlims = NULL,
                     ylims = NULL,
                     title = "auto",
                     trans = NULL,
                     breaks = NULL,
                     labels = NULL,
                     wrap_length = 60,
                     Europe_crop = TRUE,
                     surround = TRUE,
                     panel_bg = NULL,
                     legend_title = NULL,
                     legend_limits = NULL) {

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

