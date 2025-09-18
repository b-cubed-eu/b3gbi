call_plot <- function(x,
                      y_label_default = NULL,
                      auto_title_ts = NULL,
                      leg_label_default = NULL,
                      auto_title_map = NULL,
                      ...) {

  if (is.null(auto_title_ts) && is.null(auto_title_map) ||
      is.null(y_label_default) && is.null(leg_label_default)) {
    stop("Default titles or labels not provided.")
  }

  plot.args <- list(x = x, ...)

  if (inherits(x, "indicator_ts")) {
    plot.args$y_label_default <- y_label_default
    plot.args$auto_title <- auto_title_ts
    if (inherits(x, c("spec_occ", "spec_range"))) {
      plot.args$species <- list(...)$species
      do.call(plot_species_ts, plot.args)
    } else {
      do.call(plot_ts, plot.args)
    }
  } else if (inherits(x, "indicator_map")) {
    plot.args$leg_label_default <- leg_label_default
    plot.args$auto_title <- auto_title_map
    if (inherits(x, c("spec_occ", "spec_range"))) {
      plot.args$species <- list(...)$species
      do.call(plot_species_map, plot.args)
    } else {
      do.call(plot_map, plot.args)
    }
  }
}
