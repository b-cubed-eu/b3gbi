# Plot species occurrence maps
#' @export
plot.spec_occ <- function(x,
                          species = NULL,
                          xlims = NULL,
                          ylims = NULL,
                          title = "auto",
                          trans = NULL,
                          breaks = NULL,
                          labels = NULL,
                          wrap_length = 60,
                          Europe_crop = TRUE,
                          surround = TRUE,
                          panel_bg = NULL,
                          legend_title = NULL,
                          legend_limits = NULL) {

  if (is.null(species)) {

    stop("Please enter either the species names or the numeric taxonKeys for the species you want to plot.")

  } else if (is.numeric(species)) {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(taxonKey %in% species) %>%
      {if(nrow(.) < 1)
        stop("No matching taxonKeys. Please check that you have entered them correctly.")
        else (.)
      } %>%
      dplyr::mutate(taxonKey = factor(taxonKey,
                                      levels = unique(taxonKey)))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(taxonKey)

  } else {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(grepl(paste("^", species, collapse="|",sep=""), scientificName)) %>%
      { if(nrow(.) < 1)
        stop("No matching species. Please check that you have entered the names correctly.")
        else (.) } %>%
      dplyr::arrange(scientificName, grepl(paste("^", species, collapse="|",sep=""), scientificName))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(scientificName)

  }

  sci_names <-
    purrr::map(split_so, unique("scientificName"))


  # Get map limits
  map_lims <- x$coord_range

  # Crop map of Europe to leave out far-lying islands (if flag set)
  if (Europe_crop == TRUE &
      x$map_level == "continent" &
      x$map_region == "Europe")
  {

    # Set attributes as spatially constant to avoid warnings
    sf::st_agr(x$data) <- "constant"

    # Manually set cropped limits
    map_lims <- c(2600000, 1600000, 7000000, 6000000)
    names(map_lims) <- c("xmin", "ymin", "xmax", "ymax")

  }

  # Set specific instructions for country-level plots
  if (x$map_level == "country")
  {

    # Get world data to plot surrounding land if surround flag is set
    if (surround == TRUE) {
      map_surround <- rnaturalearth::ne_countries(scale = "medium") %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "EPSG:3035")

      # Otherwise make the ocean area white (unless a colour is specified)
    } else {
      if (is.null(panel_bg)) { panel_bg = "white" }
    }
  }

  # Get indicator type
  div_type <- attr(x, "indicator_id")

  # Get indicator name
  div_name <- x$div_name

  # Get object type
  obj_type <- attr(x, "type")

  # Get legend title
  leg_label <- get_legend_title(div_type, obj_type)

  # Get plot title (if set to "auto")
  if (!is.null(title)) {
    if (title == "auto") {
      title <- get_divname(div_type)
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }



    # Plot on map
    diversity_plot <-
      purrr::map2(split_so,
                  sci_names,
                  function(x., y) {
                    ggplot2::ggplot(x.) +
                      geom_sf(data = x$data,
                              aes(geometry = geometry),
                              fill = "grey95") +
                      geom_sf(aes(fill = num_records,
                                  geometry = geometry)) +
                      cust_leg(list(trans = trans,
                                    breaks = breaks,
                                    labels = labels,
                                    limits = legend_limits)) +
                      coord_sf(
                        xlim = c(map_lims["xmin"],
                                 map_lims["xmax"]),
                        ylim = c(map_lims["ymin"],
                                 map_lims["ymax"])
                      ) +
                      #scale_x_continuous() +
                      theme_bw()+
                      theme(
                        panel.background = element_rect(fill = if(!is.null(panel_bg)) panel_bg
                                                        else "#92c5f0"),
                        if(x$map_level == "country") {
                          panel.grid.major = element_blank()
                          panel.grid.minor = element_blank()
                        },
                        legend.text = element_text(),
                        strip.text = element_text(face = "italic"),
                        plot.title = element_text(face = "italic")
                      ) +
                      labs(title = y,
                           fill = "Occurrences")
                  })

    # If surround flag is set, add surrounding countries to the map
    if (surround == TRUE & x$map_level == "country") {
      for (i in 1:length(diversity_plot)) {
        diversity_plot[[i]]$layers <- c(geom_sf(data = map_surround,
                                                fill = "grey85")[[1]],
                                        diversity_plot[[i]]$layers)
      }
    }

    # Combine plots using internal copy of wrap_plots function from patchwork
    diversity_plot <- wrap_plots_int(diversity_plot) +
      plot_annotation_int(title = title,
                          theme = theme(plot.title = element_text(size = 20)))

    # Exit function
    return(diversity_plot)

}
