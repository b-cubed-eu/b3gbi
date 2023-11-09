plot_map <- function(coverage_df_cell, xlims = NA, ylims = NA, title = NA, cust_limits = NA) {

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
                             "Occurrences",
                             "Mean Year of \nOccurrence",
                             "Occurrences \n per km^2")


  div_type <- dplyr::first(coverage_df_cell$diversity_type, na_rm = TRUE)

  leg_label <- names(diversity_type)[diversity_type %in%
                                       div_type]

  # Plot estimated relative richness
  diversity_plot <- ggplot(coverage_df_cell) +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            color = "grey70") +
    # scale_fill_scico(palette = "davos",
    #                  direction = -1,
    #                  begin = 0.05,
    #                  end = 0.95) +
     scale_fill_gradient(low = "gold",
                         high = "firebrick4",
                         na.value = "grey95") +
    coord_sf(
      xlim = c(map_lims["xmin"],
               map_lims["xmax"]),
      ylim = c(map_lims["ymin"],
               map_lims["ymax"])
    ) +
    theme(
      plot.background = element_rect(fill = "#f1f2f3"),
      panel.background = element_rect(fill = "#92c5f0"),
      panel.grid.major = element_line(linewidth = 0.1,
                                      color = "#80808080"),
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      legend.text = element_blank()
    ) +
    labs(fill = leg_label)

  if (div_type=="newness" |
      div_type=="total_obs" |
      div_type=="density" |
      div_type=="evenness") {

    diversity_plot <-
      diversity_plot +
      theme(legend.text = element_text())

  }

  if (div_type=="total_obs" & any(!is.na(cust_limits))) {

    diversity_plot <-
      diversity_plot +
      scale_fill_gradient(low = "gold",
                        high = "firebrick4",
                        na.value = "grey95",
                        trans = "log1p",
                        breaks = breaks_log(n=10),
                        limits = cust_limits)

  }

  if (div_type=="density") {

    diversity_plot <-
      diversity_plot +
      labs(fill = bquote(atop(Occurrences,
                              per~km^2)))

  }

  if(any(!is.na(xlims)) & any(!is.na(ylims))) {
    diversity_plot <-
      diversity_plot + coord_sf(xlim = xlims,
                                ylim = ylims)

  }

  if(!is.na(title)) {

    diversity_plot <-
      diversity_plot +
      labs(title = title)

  }

  diversity_plot

}
