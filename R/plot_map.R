plot_map <- function(coverage_df_cell) {

  # get map limits
  map_lims <- st_bbox(coverage_df_cell)

  # determine legend label
  diversity_type <- c("0",
                      "1",
                      "2",
                      "obs_richness",
                      "evenness",
                      "ab_rarity",
                      "area_rarity",
                      "total_obs",
                      "newness",
                      "density")

  names(diversity_type) <- c("Estimated \nSpecies \nRichness",
                             "Shannon \nDiversity",
                             "Simpson \nDiversity",
                             "Observed \nSpecies \nRichness",
                             "Evenness",
                             "Abundance-\nBased \nRarity",
                             "Area-\nBased \nRarity",
                             "Total \nOccurrences",
                             "Mean Year of \nOccurrence",
                             "Occurrences \n per km^2")

  leg_label <- names(diversity_type)[diversity_type %in%
                                       coverage_df_cell$diversity_type[1]]

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

  if (coverage_df_cell$diversity_type[1]=="newness" |
      coverage_df_cell$diversity_type[1]=="total_obs" |
      coverage_df_cell$diversity_type[1]=="density" |
      coverage_df_cell$diversity_type[1]=="evenness") {

    diversity_plot <-
      diversity_plot +
      theme(legend.text = element_text())

  }

  if (coverage_df_cell$diversity_type[1]=="density") {

    diversity_plot <-
      diversity_plot +
      labs(fill = bquote(atop(Occurrences,
                              per~km^2)))

  }

  diversity_plot

}
