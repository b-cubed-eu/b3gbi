# Function to plot gridded ranges for selected species
# Input selected taxonKey values to "species"
#' @export
plot_spec_range <- function(data, species, buffer = 20000, xlims = NA, ylims = NA) {

  # check for custom map limits
  if(any(!is.na(xlims)) & any(!is.na(ylims))) {

    map_lims = c(xlims[1], ylims[1], xlims[2], ylims[2])
    names(map_lims) = c("xmin", "ymin", "xmax", "ymax")

  } else {

    # get map limits
    map_lims <- sf::st_bbox(data)

  }

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
  spec_occ_plots <-
    purrr::map2(split_so,
                sci_names,
                function(x, y) {
                  x <- sf::st_buffer(x, dist = buffer) %>%
                    sf::st_union() %>%
                    sf::st_simplify(dTolerance = buffer / 10) %>%
                    sf::st_intersection(data)
                  ggplot2::ggplot(x) +
                    geom_sf(data = data,
                            aes(geometry = geometry),
                            fill = "grey90") +
                    geom_sf(aes(geometry = geometry),
                            fill = "red",
                            alpha = 0.5) +
                    coord_sf(
                      xlim = c(map_lims["xmin"],
                               map_lims["xmax"]),
                      ylim = c(map_lims["ymin"],
                               map_lims["ymax"])
                    ) +
                    scale_x_continuous() +
                    theme(
                      plot.background = element_rect(fill = "white"),
                      panel.background = element_rect(fill = "white"),
                      panel.grid.major = element_line(linewidth = 0.1,
                                                      color = "#80808080"),
                      line = element_blank(),
                      rect = element_blank(),
                      legend.text = element_text(),
                      strip.text = element_text(face = "italic"),
                      plot.title = element_text(face = "italic")
                    ) +
                    labs(title = y)
                })




  wrap_plots_int(spec_occ_plots) +
    plot_annotation_int(title = "Species Range",
                        theme = theme(plot.title = element_text(size = 20)))

}
