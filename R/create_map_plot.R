#' @noRd
create_map_plot <- function(data,
                            map_surround,
                            layer_list,
                            layer_colours,
                            layer_fill_colours,
                            land_fill_colour,
                            ocean_fill_colour,
                            grid_fill_colour,
                            grid_line_colour,
                            grid_outline_colour,
                            grid_line_width,
                            grid_outline_width,
                            grid_fill_transparency,
                            grid_line_transparency,
                            trans,
                            bcpower,
                            breaks,
                            labels,
                            legend_limits,
                            map_level,
                            visible_gridlines,
                            visible_grid_outline,
                            visible_panel_gridlines,
                            complete_grid_outline,
                            crop_to_grid,
                            map_lims,
                            map_lims_original,
                            projection,
                            original_bbox,
                            leg_label_default,
                            legend_title,
                            legend_title_wrap_length,
                            suppress_legend,
                            title_label,
                            title_face = "plain") {

  # Set necessary variables to NULL to avoid R CMD check notes
  geometry <- diversity_val <- NULL

  # Set default values
  ocean_fill_colour <- ocean_fill_colour %||% "#92c5f0"
  land_fill_colour <- land_fill_colour %||% "grey85"
  grid_fill_colour <- grid_fill_colour %||% "transparent"
  grid_line_colour <- if (visible_gridlines) {
    grid_line_colour %||% "black"
  } else {
    "transparent"
  }
  grid_outline_colour <- if (visible_grid_outline) {
    grid_outline_colour %||% "black"
  } else {
    "transparent"
  }
  grid_outline_width <- grid_outline_width %||% 0.5
  grid_line_width <- grid_line_width %||% 0.1
  grid_fill_transparency <- if (visible_gridlines) {
    grid_fill_transparency %||% 0.2
  } else {
    grid_fill_transparency %||% 0
  }
  grid_line_transparency <- if (visible_gridlines) {
    grid_line_transparency %||% 0.5
  } else {
    0
  }

  # Format layer colours
  if (!is.null(layer_colours)) {
    layer_colours <- list("black", layer_colours)
  }
  if (!is.null(layer_fill_colours)) {
    layer_fill_colours <- list("transparent", layer_fill_colours)
  }

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

  # Transform data to the chosen projection
  data <- sf::st_transform(data, crs = projection)

  ###################################
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
  for (i in seq_along(layer_list)) {
    layer_data <- layer_list[[i]]
    layer_fill_colour <- if(names(layer_list)[[i]] %in% c("ocean", "lakes") &&
                            is.null(layer_fill_colours)) {
      "#92c5f0"
    } else if (!is.null(layer_fill_colours)) {
      layer_fill_colours[[i]]
    } else {
      "transparent"
    }
    layer_colour <- if (!is.null(layer_colours)) layer_colours[[i]] else "black"

    plot <- plot +
      ggplot2::geom_sf(data = layer_data,
                       aes(geometry = geometry),
                       fill = layer_fill_colour,
                       colour = layer_colour,
                       inherit.aes = FALSE)
  }

  # Step 4: Add indicator values
  plot <- plot +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            colour = alpha(grid_line_colour, grid_line_transparency),
            linewidth = grid_line_width) +
    cust_leg(list(trans = trans,
                  breaks = breaks,
                  labels = labels,
                  limits = legend_limits)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = ocean_fill_colour),
      legend.text = ggplot2::element_text(),
      strip.text = ggplot2::element_text(face = "italic"),
      plot.title = ggplot2::element_text(face = title_face)
    ) +
    # Wrap legend title if longer than user-specified wrap length
    labs(fill = wrapper(legend_title %||% leg_label_default,
                                                    legend_title_wrap_length)) +
    if (suppress_legend) theme(legend.position = "none")

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

  # Step 6: Add the grid outline and fill
  if (complete_grid_outline == "original") {
    grid_outline <- original_bbox
  } else if (complete_grid_outline == "transformed") {
    grid_outline <- sf::st_as_sfc(sf::st_bbox(map_lims_original,
                                              crs = sf::st_crs(data))) %>%
      sf::st_transform(crs = projection)

  } else {
    grid_outline <- sf::st_union(data)
  }

  plot <- plot +
    ggplot2::geom_sf(data = grid_outline,
                     colour = grid_outline_colour,
                     linewidth = grid_outline_width,
                     fill = grid_fill_colour,
                     alpha = grid_fill_transparency,
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

  if (visible_panel_gridlines != TRUE) {
    plot <- plot + ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  }

  # Exit function
  return(plot)

}
