inputdir <- "inst/extdata/ramsar_site_data_100m_asia"
maindir <- "output/ramsar_metric_results_100m/asia"
shapefiledir <- "inst/extdata/ramsar_sites_wkt"
continent <- substring(inputdir, 36, nchar(inputdir))

countrylist <- list.files(inputdir)[2]
for (i in 1:length(countrylist)) {

  countrydir <- paste0(maindir, "/", countrylist[i])
  if (!dir.exists(countrydir)) {
    dir.create(countrydir, recursive = TRUE)
    message(paste0("Created ", countrylist[i], " output directory:", countrydir))
  }

  sitelist <- list.files(paste0(inputdir, "/", countrylist[i]))


  ormapsdir <- paste0(maindir, "/plots/obs_rich_maps/", countrylist[i])
  if (!dir.exists(ormapsdir)) {
    dir.create(ormapsdir, recursive = TRUE)
    message(paste("Created ", sitelist[j], " observed richness maps directory:", ormapsdir))
  }

  for (j in 1:length(sitelist)) {

    sitename <- substring(sitelist[j], 1, nchar(sitelist[j]) - 9)
    # sitedir <- paste0(countrydir, "/", sitename)
    # if (!dir.exists(sitedir)) {
    #   dir.create(sitedir, recursive = TRUE)
    #   message(paste("Created ", sitelist[j], " output directory:", sitedir))
    # }

    shapefilepath <- paste0(shapefiledir, "/", countrylist[i], "/", sitename, ".wkt")

    temp <- process_cube(paste0(inputdir, "/", countrylist[i], "/", sitelist[j]), separator = ",")
    temp_obsrich_map <- obs_richness_map(temp, cell_size = 0.1, shapefile_path = shapefilepath, ne_scale = "medium", region = continent)
    temp_orm_plot <- plot(temp_obsrich_map, crop_to_grid = TRUE)

    ggsave(filename = paste0(ormapsdir, "/", sitename, ".png"), temp_orm_plot, device = "png", height = 8000, width = 8000, units = "px")

  }
}

