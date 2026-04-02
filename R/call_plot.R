call_plot <- function(x, ...) {
  # 1. Capture everything into a list immediately
  dots <- list(...)

  # 2. Extract our specific control variables and remove them from dots
  # This helper ensures we get the value whether it was passed
  # positionally or by name, then clears it so it doesn't double-up.
  get_and_remove <- function(name, default = NULL) {
    val <- if (name %in% names(dots)) dots[[name]] else default
    dots[[name]] <<- NULL # Remove from the dots list in the outer scope
    return(val)
  }

  # Pull our logic variables out of the "stream"
  y_label_default   <- get_and_remove("y_label_default")
  auto_title_ts     <- get_and_remove("auto_title_ts")
  leg_label_default <- get_and_remove("leg_label_default")
  auto_title_map    <- get_and_remove("auto_title_map")

  # 3. Validation
  if ((is.null(auto_title_ts) && is.null(auto_title_map)) ||
      (is.null(y_label_default) && is.null(leg_label_default))) {
    stop("Default titles or labels not provided.")
  }

  # 4. Prepare the clean argument list for the next functions
  # We use the 'dots' which now has the 'internal' variables removed
  plot.args <- dots
  plot.args$x <- x

  if (inherits(x, "indicator_ts")) {
    # Only assign defaults if the user didn't provide their own specific label
    if (is.null(plot.args$y_label)) plot.args$y_label_default <- y_label_default
    if (is.null(plot.args$title))   plot.args$auto_title <- auto_title_ts

    target_fun <- if (inherits(x, c("spec_occ", "spec_range"))) plot_species_ts else plot_ts
    return(do.call(target_fun, plot.args))

  } else if (inherits(x, "indicator_map")) {
    if (is.null(plot.args$leg_label)) plot.args$leg_label_default <- leg_label_default
    if (is.null(plot.args$title))     plot.args$auto_title <- auto_title_map

    target_fun <- if (inherits(x, c("spec_occ", "spec_range"))) plot_species_map else plot_map
    return(do.call(target_fun, plot.args))
  }
}

