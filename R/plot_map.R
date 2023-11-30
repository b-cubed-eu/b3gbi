# Function to plot biodiversity indicators calculated over gridded space
# Accepts tibbles output by plot_ts.R
plot_map <- function(data,
                     species = NULL,
                     xlims = NULL,
                     ylims = NULL,
                     title = NULL,
                     trans = NULL,
                     breaks = NULL,
                     labels = NULL,
                     wrap_length = 60,
                     Europe_crop = TRUE,
                     surround = TRUE,
                     panel_bg = NULL,
                     legend_title = NULL,
                     legend_limits = NULL) {

  # Get map limits
  map_lims <- sf::st_bbox(data)

  # Crop map of Europe to leave out far-lying islands (if flag set)
  if (Europe_crop == TRUE &
      data$map_level[1] == "continent" &
      data$map_region[1] == "Europe")
  {

    # Set attributes as spatially constant to avoid warnings
    sf::st_agr(data) <- "constant"

    # Manually set cropped limits
    map_lims <- c(2600000, 1600000, 7000000, 6000000)
    names(map_lims) <- c("xmin", "ymin", "xmax", "ymax")

  }

  # Set specific instructions for country-level plots
  if (data$map_level[1] == "country")
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
  div_type <- dplyr::first(data$diversity_type, na_rm = TRUE)

  # Get default parameters for indicator
  div_types <- read.csv("diversity_types.csv",
                        na.strings = c(""))
  default_params <- div_types[div_types$label %in% div_type,]

  # Get legend title
  leg_label <- default_params$legend

  # Get plot title (if set to "auto")
  if (!is.null(title)) {
    if (title == "auto") {
      title <- default_params$title
    }
  }

  # Get legend transformations (will not overwrite user input)
  if (is.null(trans)) {
    if(!is.na(default_params$transformation)) {
      trans <- default_params$transformation
    } else {
      trans <- NULL
    }
  }

  # Get legend breaks (will not overwrite user input)
  if (!is.null(trans)) {
    if (trans=="log" | trans=="log10") {
      breaks = breaks_log_int(n=6)
      leg_label <- paste(leg_label,
                         "\n(log-transformed)",
                         sep="")
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }

  # Plot species occurrence maps
  if (div_type == "spec_occ") {

    if (is.null(species) | !is.numeric(species)) {

      stop("Please enter the taxonKeys for the species you want to plot in numeric format.")

    }

    # Get occurrences for selected species
    species_occurrences <-
      data %>%
      dplyr::filter(taxonKey %in% species) %>%
      dplyr::mutate(taxonKey = factor(taxonKey,
                                      levels = unique(taxonKey)))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(taxonKey)

    sci_names <-
      purrr::map(split_so, unique("scientificName"))

    # Plot on map
    diversity_plot <-
      purrr::map2(split_so,
                  sci_names,
                  function(x, y) {
                    ggplot2::ggplot(x) +
                      geom_sf(data = data,
                              aes(geometry = geometry),
                              fill = "grey40") +
                      geom_sf(aes(fill = num_records,
                                  geometry = geometry)) +
                      scale_fill_gradient(low = "gold",
                                          high = "firebrick4") +
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
                        if(data$map_level[1] == "country") {
                          panel.grid.major = element_blank()
                          panel.grid.minor = element_blank()
                        },
                        legend.text = element_text(),
                        strip.text = element_text(face = "italic"),
                        plot.title = element_text(face = "italic")
                      ) +
                      labs(title = y,
                           fill = "Number of \nOccurrences")
                  })

    # If surround flag is set, add surrounding countries to the map
    if (surround == TRUE) {
      for (i in 1:length(diversity_plot)) {
      diversity_plot[[i]]$layers <- c(geom_sf(data = map_surround, fill = "grey85")[[1]], diversity_plot[[i]]$layers)
      }
    }

    # Combine plots using internal copy of wrap_plots function from patchwork
    diversity_plot <- wrap_plots_int(diversity_plot) +
      plot_annotation_int(title = "Species Occurrences",
                          theme = theme(plot.title = element_text(size = 20)))

    # Exit function
    return(diversity_plot)

  }

  # Plot map
  diversity_plot <- ggplot2::ggplot(data) +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            color = "black") +
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
    theme_bw() +
    theme(
      panel.background = element_rect(fill = if(!is.null(panel_bg)) panel_bg
                                      else "#92c5f0"),
      if(data$map_level[1] == "country") {
        panel.grid.major = element_blank()
        panel.grid.minor = element_blank()
      }
  ) +
    labs(fill = if(!is.null(legend_title)) legend_title
         else leg_label)


  # if (div_type=="density") {
  #
  #   diversity_plot <-
  #     diversity_plot +
  #     labs(fill = bquote(atop(Occurrences,
  #                             per~km^2)))
  #
  # }

  # If surround flag set, add surrounding countries to map
  if (surround == TRUE) {
    diversity_plot$layers <- c(geom_sf(data = map_surround, fill = "grey85")[[1]], diversity_plot$layers)
  }

  # Check for custom x and y limits and adjust map if found
  if(any(!is.null(xlims)) & any(!is.null(ylims))) {
    diversity_plot <-
      diversity_plot + coord_sf(xlim = xlims,
                                ylim = ylims)

  }

  # Wrap title if longer than wrap_length
  if(!is.null(title)) {
    wrapper <- function(x, ...)
    {
      paste(strwrap(x, ...), collapse = "\n")
    }
    diversity_plot <-
      diversity_plot +
      labs(title = wrapper(title, wrap_length))
  }

  # Exit function
  return(diversity_plot)

}
