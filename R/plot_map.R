plot_map <- function(coverage_df_cell, type = "obs") {

  map_lims <- st_bbox(coverage_df_cell)

  if (type == "obs") {

    # Plot observed richness
    obs_richness_plot <- ggplot(coverage_df_cell) +
      #geom_sf(data = map_data, fill = "grey") +
      geom_sf(aes(fill = log(obs_richness+1), geometry = geometry), color = NA) +
      scale_fill_scico(palette = "davos", direction = -1, end = 0.9) +
      coord_sf(
        xlim = c(map_lims["xmin"], map_lims["xmax"]),
        ylim = c(map_lims["ymin"], map_lims["ymax"])
      ) +
      scale_x_continuous() +
      theme(
        plot.background = element_rect(fill = "#f1f2f3"),
        panel.background = element_rect(fill = "#f1f2f3"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.text = element_blank()
      ) +
      labs(fill = "log of \nobserved \nrichness")
    obs_richness_plot

  } else if (type == "adj") {

    # Plot estimated relative richness
    adj_richness_plot <- ggplot(coverage_df_cell) +
      #geom_sf(data = map_data, fill = "grey") +
      geom_sf(aes(fill = est_relative_richness,
                  geometry = geometry),
              color = NA) +
      scale_fill_scico(palette = "davos",
                       direction = -1,
                       end = 0.9) +
      coord_sf(
        xlim = c(map_lims["xmin"],
                 map_lims["xmax"]),
        ylim = c(map_lims["ymin"],
                 map_lims["ymax"])
      ) +
      scale_x_continuous() +
      theme(
        plot.background = element_rect(fill = "#f1f2f3"),
        panel.background = element_rect(fill = "#f1f2f3"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.text = element_blank()
      ) +
      labs(fill = "estimated \nrelative \nrichness")
    adj_richness_plot

  } else {

    print("Please use 'obs' or 'adj' for type.")

  }


}
