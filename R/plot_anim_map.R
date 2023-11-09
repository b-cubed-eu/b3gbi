# num_years_div is the number of years to summarize in each frame of the animation

plot_anim_map <- function(data, start_year, end_year, num_years, div_type) {

  total_years <- end_year - start_year

  num_frames <- ceiling(total_years / num_years)

  final_frame_years <- total_years - ((num_frames - 1) * num_years)

  frame_years_vec <- c(rep(num_years, (num_frames - 1)), final_frame_years)




  mapobs_amph3_1930s <- calc_map(amphib_data3[(amphib_data3$year > 1930 &
                                                 amphib_data3$year <= 1940),],
                                 level = "continent",
                                 region = "Europe",
                                 type = "total_obs")




}
