inputdir <- "inst/extdata/ramsar_site_data_100m_asia"
maindir <- "output/ramsar_metric_results_100m/asia"
shapefiledir <- "inst/extdata/ramsar_sites_wkt"
continent <- substring(inputdir, 36, nchar(inputdir))

countrylist <- list.files(inputdir)[10]
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
    message(paste("Created ", sitelist[i], " observed richness maps directory:", ormapsdir))
  }
 for (j in 41:length(sitelist)) {
#  for (j in 1:length(sitelist)) {

    tryCatch({

      sitename <- substring(sitelist[j], 1, nchar(sitelist[j]) - 9)
      # sitedir <- paste0(countrydir, "/", sitename)
      # if (!dir.exists(sitedir)) {
      #   dir.create(sitedir, recursive = TRUE)
      #   message(paste("Created ", sitelist[j], " output directory:", sitedir))
      # }

      shapefilepath <- paste0(shapefiledir, "/", countrylist[i], "/", sitename, ".wkt")

      temp <- process_cube(paste0(inputdir, "/", countrylist[i], "/", sitelist[j]),
                           separator = ",")
      temp_obsrich_map <- obs_richness_map(temp,
                                           shapefile_path = shapefilepath,
                                           ne_scale = "large",
                                           region = continent,
                                           layers = c("ocean", "lakes", "rivers_lake_centerlines")
                                           )
      temp_orm_plot <- plot(temp_obsrich_map)

      ggsave(filename = paste0(ormapsdir, "/", sitename, ".png"), temp_orm_plot, device = "png", height = 4000, width = 4000, units = "px")

    }, error = function(e) {
      if (grepl("No spatial intersection between map data and grid.", e) ||
          grepl("Error in FUN", e)) {
        message(
          paste0(
            "Encountered a geometry error during intersection. This may be ",
            "due to coordinate mismatches."
          )
        )
      } else {
        stop(e)
      }
    })

  }
}

