plot_map <- function(coverage_df_cell) {

  # get map limits
  map_lims <- st_bbox(coverage_df_cell)

  # determine legend label
  diversity_type <- c("0",
                      "1",
                      "2",
                      "obs_richness",
                      "evenness")

  names(diversity_type) <- c("Estimated \nSpecies \nRichness",
                             "Shannon \nDiversity",
                             "Simpson \nDiversity",
                             "Observed \nSpecies \nRichness",
                             "Evenness")

  leg_label <- names(diversity_type)[diversity_type %in%
                                       coverage_df_cell$diversity_type[1]]

  # coverage_df_cell$diversity_val <- ifelse(coverage_df_cell$diversity_type=="obs_richness",
  #                                          log(coverage_df_cell$diversity_val+1),
  #                                          coverage_df_cell$diversity_val)

  # Plot estimated relative richness
  diversity_plot <- ggplot(coverage_df_cell) +
    geom_sf(aes(fill = diversity_val,
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
    labs(fill = leg_label)
  diversity_plot

}
