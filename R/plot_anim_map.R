# num_years_div is the number of years to summarize in each frame of the animation

plot_anim_map <- function(data,
                          filename,
                          start_year,
                          end_year,
                          num_years,
                          div_type,
                          delay = 1.5,
                          height = 2000,
                          width = 2500,
                          cust_limits = NA,
                          xlims = NA,
                          ylims = NA,
                          title = NA,
                          level = "continent",
                          region = "Europe",
                          wrap_length = 60) {

  if(is.na(title)) {

    # determine legend label
    diversity_type <- c("0",
                        "1",
                        "2",
                        "obs_richness",
                        "evenness",
                        "ab_rarity",
                        "area_rarity",
                        "total_occ",
                        "newness",
                        "density",
                        "tax_distinct")

    names(diversity_type) <- c("Estimated Species Richness",
                               "Shannon Diversity",
                               "Simpson Diversity",
                               "Observed Species Richness",
                               "Evenness",
                               "Abundance-Based Rarity",
                               "Area-Based Rarity",
                               "Total Occurrences",
                               "Mean Year of Occurrence",
                               "Occurrences per km^2",
                               "Taxonomic Distinctness")

    div_title <- names(diversity_type)[diversity_type %in%
                                     div_type]

    title <- paste(div_title, " in ", paste(c(region), collapse = ", "), sep = "")

  }

  total_years <- end_year - start_year

  num_frames <- ceiling(total_years / num_years)

  final_frame_years <- total_years - ((num_frames - 1) * num_years)

  frame_years_vec <- c(rep(num_years, (num_frames - 1)), final_frame_years)

  map_list <- list()

  plot_list <- list()

  plot_filename_vec <- vector()

  year_counter <- start_year

  map_list <- for (i in 1:num_frames) {

    num_map_years <- frame_years_vec[i]

    actual_map_years <- year_counter:(year_counter + num_map_years - 1)

    map_temp <- calc_map(data[data$year %in% actual_map_years,],
                         level = level,
                         region = region,
                         type = div_type)

    map_list[[i]] <- map_temp

    plot_temp <- plot_map(map_temp,
                          xlims = xlims,
                          ylims = ylims,
                          title = paste(title,
                                        ": ",
                                        year_counter,
                                        " - ",
                                        (year_counter + num_map_years),
                                        sep = ""),
                          cust_limits = cust_limits,
                          wrap_length = wrap_length)

    plot_list[[i]] <- plot_temp

    plot_filename <- paste("plot_temp_", i, ".png", sep="")

    plot_filename_vec[i] <- plot_filename

    ggsave(file=plot_filename,
           plot_temp,
           device = "png",
           dpi = 300,
           height = height,
           width = width,
           units = "px")

    year_counter <- year_counter + num_map_years

  }

  anim <- gifski_int(plot_filename_vec,
                     gif_file = filename,
                     delay = delay,
                     height = height,
                     width = width)


invisible(lapply(plot_filename_vec, file.remove))



}
