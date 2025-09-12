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

  if (inherits(x, "indicator_ts")) {
    if (inherits(x, c("spec_occ", "spec_range"))) {
      plot_species_ts(x,
                      y_label_default = y_label_default,
                      auto_title = auto_title_ts)
    } else {
      plot_ts(x,
              y_label_default = y_label_default,
              auto_title = auto_title_ts,
              ...)
    }
  } else if (inherits(x, "indicator_map")) {
    if (inherits(x, c("spec_occ", "spec_range"))) {
      plot_species_map(x,
                       leg_label_default = leg_label_default,
                       auto_title = auto_title_map,
                       ...)
    } else {
      plot_map(x,
               leg_label_default = leg_label_default,
               auto_title = auto_title_map,
               ...)
    }
  }
}
