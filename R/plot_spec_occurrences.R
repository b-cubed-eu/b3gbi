# Function to plot gridded occurrences for selected species
# Input selected taxonKey values to "species"

plot_spec_occurrences <- function(data, species) {

    # Get occurrences for selected species
  species_occurrences <-
    data %>%
    dplyr::filter(taxonKey %in% species)

  sci_names <-
    species_occurrences$scientificName %>%
    unique()

  split_so <-
    species_occurrences %>%
    dplyr::group_split(taxonKey)

  # Plot on map
  spec_occ_plots <- purrr::map2(split_so,
                                sci_names,
                                function(x, y) {
    ggplot(x) +
      geom_sf(data = map_data) +
      geom_sf(aes(fill = num_records,
                  geometry = geometry),
              color = "black") +
      scale_fill_scico(palette = "davos",
                       direction = -1,
                       end = 0.75) +
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
        legend.text = element_text(),
        strip.text = element_text(face = "italic"),
        plot.title = element_text(face = "italic")
      ) +
      labs(title = y,
           fill = "Number of \nOccurrences")
  })

  wrap_plots(spec_occ_plots) + plot_annotation(title = "Species Occurrences")

}
