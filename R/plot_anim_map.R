# num_years_div is the number of years to summarize in each frame of the animation

plot_anim_map <- function(data, filename, start_year, end_year, num_years, div_type, delay = 1.5, height = 2000, width = 2500, cust_limits = NA, xlims = NA, ylims = NA, title = NA) {

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
                         level = "continent",
                         region = "Europe",
                         type = div_type)

    map_list[i] <- map_temp

    plot_temp <- plot_map(map_temp,
                          xlims = xlims,
                          ylims = ylims,
                          title = paste(title,
                                        ": ",
                                        year_counter,
                                        " - ",
                                        (year_counter + num_map_years),
                                        sep = ""),
                          cust_limits = cust_limits)

    plot_list[i] <- plot_temp

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

  anim <- gifski(plot_filename_vec,
                 gif_file = filename,
                 delay = delay,
                 height = height,
                 width = width)


lapply(plot_filename_vec, file.remove)



}
