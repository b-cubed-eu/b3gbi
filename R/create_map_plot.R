#' @noRd
create_map_plot <- function(data,
                            map_surround,
                            layer_list,
                            land_fill_colour,
                            panel_bg,
                            trans,
                            bcpower,
                            breaks,
                            labels,
                            legend_limits,
                            map_level,
                            visible_gridlines,
                            crop_to_grid,
                            map_lims,
                            projection,
                            leg_label_default,
                            legend_title,
                            legend_title_wrap_length,
                            suppress_legend,
                            title_label,
                            title_face = "plain") {

  # Set necessary variables to NULL to avoid R CMD check notes
  geometry <- diversity_val <- NULL

  # Set default value for land_fill_colour if NULL
  land_fill_colour <- land_fill_colour %||% "grey85"

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "transparent"),
      scale.params)
    )
  }

  # Set up transformation if specified
  if (!is.null(trans)) {
    if (trans == "boxcox") {
      trans <- scales::transform_boxcox(p = bcpower)
    } else if (trans == "modulus") {
      trans <- scales::transform_modulus(p = bcpower)
    } else if (trans == "yj") {
      trans <- scales::transform_yj(p = bcpower)
    }
  }

  # Set the grid line color based on the function argument
  # If TRUE, the color is black. If FALSE, it's transparent.
  grid_line_colour <- if (visible_gridlines) "black" else "transparent"

  # Create plot in steps using ggplot
  ###################################

  # Step 1: Create blank plot to fill
  plot <- ggplot2::ggplot(data)

  # Step 2: Add the land data
  plot <- plot +
    ggplot2::geom_sf(
      data = map_surround,
      fill = land_fill_colour,
      colour = "black",
      aes(geometry = geometry),
      inherit.aes = FALSE
    )

  # Step 3: Add additional layers, with ocean and lakes in blue
  for (i in names(layer_list)) {
    layer_data <- layer_list[[i]]
    if (i %in% c("ocean", "lakes")) {
      fill_colour <- "#92c5f0"
    } else {
      fill_colour <- "transparent"
    }
    plot <- plot +
      ggplot2::geom_sf(data = layer_data,
                       aes(geometry = geometry),
                       fill = fill_colour,
                       colour = "black",
                       inherit.aes = FALSE)
  }

  # Step 4: Add indicator values
  plot <- plot +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            colour = grid_line_colour) +
    cust_leg(list(trans = trans,
                  breaks = breaks,
                  labels = labels,
                  limits = legend_limits)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = panel_bg %||% "#92c5f0"),
      legend.text = ggplot2::element_text(),
      strip.text = ggplot2::element_text(face = "italic"),
      plot.title = ggplot2::element_text(face = title_face)
    ) +
    # Wrap legend title if longer than user-specified wrap length
    labs(fill = wrapper(legend_title %||% leg_label_default,
                                                    legend_title_wrap_length)) +
    if (suppress_legend) theme(legend.position = "none") +
    if (map_level == "country") {
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    }

  # Step 5: Re-create lines from layers
  # This ensures that indicator values are plotted on top of layer fill (e.g.,
  # oceans and lakes) but lines are still visible.
  for (i in names(layer_list)) {
    layer_data <- layer_list[[i]]
    plot <- plot +
      ggplot2::geom_sf(data = layer_data,
                       aes(geometry = geometry),
                       fill = "transparent",
                       colour = "black",
                       inherit.aes = FALSE)
  }

  # Step 6: Add the grid outline
  grid_outline <- sf::st_union(data)
  plot <- plot +
    ggplot2::geom_sf(data = grid_outline,
                     colour = "black",
                     linewidth = 0.5,
                     fill = "transparent",
                     inherit.aes = FALSE
    )

  # Step 7: Expand the plot if crop_to_grid is not set
  expand_val <- if (crop_to_grid) FALSE else TRUE
  plot <- plot +
    coord_sf(
      crs = projection,
      xlim = c(map_lims["xmin"],
               map_lims["xmax"]),
      ylim = c(map_lims["ymin"],
               map_lims["ymax"]),
      expand = expand_val)

  # Step 8: Add title
  if (!is.null(title_label)) {
    plot <- plot + ggplot2::labs(title = title_label)
  }

  # Exit function
  return(plot)

}
