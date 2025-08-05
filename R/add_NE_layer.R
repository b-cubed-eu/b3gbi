add_NE_layer <- function(layer_name, scale, latlong_extent) {
  # Load necessary packages if not already loaded (good practice for functions)
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) stop("rnaturalearth package required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf package required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr package required.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang package required for dynamic filtering.") # Added rlang

  geometry <- featurecla <- scalerank <- min_zoom <- NULL

  if (layer_name == "admin_0_countries") {
    # Use the robust ne_countries function for the base layer
    # The ne_countries function handles downloading the data if it doesn't exist
    layer_raw <- rnaturalearth::ne_countries(scale = scale,
                                             returnclass = "sf")
  } else {

    # --- Dynamic Category Lookup ---
    # Combine physical and cultural layer info
    all_layers_info <- rbind(
      rnaturalearth::df_layers_physical %>%
        dplyr::mutate(category = "physical",
                      type = layer),
      rnaturalearth::df_layers_cultural %>%
        dplyr::mutate(category = "cultural",
                      type = layer)
    )

    # Map 'scale' argument to the column names in df_layers_X for availability check
    scale_col <- switch(scale,
                        "large" = "scale10",
                        "medium" = "scale50",
                        "small" = "scale110",
                        stop("Unsupported scale. Choose 'small', 'medium', or 'large'.")
    )

    # Filter to find the layer and its category at the specified scale
    layer_info <- all_layers_info %>%
      dplyr::filter(type == layer_name) %>%
      dplyr::filter(!!rlang::sym(scale_col) == 1) # Check if available at the specified scale

    if (nrow(layer_info) == 0) {
      stop(paste0("Layer '", layer_name, "' not found or not available at scale '",
                  scale, "'. Please check rnaturalearth::df_layers_physical and rnaturalearth::df_layers_cultural."))
    }
    # Assuming unique layer_name + scale combination yields a single category
    category <- layer_info$category[1]
    # --- End Dynamic Category Lookup ---


    # Step 1: Use tryCatch ONLY for the loading/downloading part
    layer_raw <- tryCatch({
      rnaturalearth::ne_load(scale = scale,
                             returnclass = "sf",
                             type = layer_name,
                             category = category)
    }, error = function(e) {
      if (grepl("the file .* seems not to exist", e, ignore.case = TRUE) ||
          grepl("Failed to download", e, ignore.case = TRUE) ||
          grepl("HTTP status was 404", e, ignore.case = TRUE)) {
        message(paste0("Attempting to download '", layer_name,
                       "' data due to previous load error."))
        rnaturalearth::ne_download(scale = scale,
                                   returnclass = "sf",
                                   type = layer_name,
                                   category = category)
      } else {
        stop(e) # Re-throw other unhandled errors
      }
    })

  }

  le_projected <- sf::st_transform(sf::st_as_sfc(latlong_extent),
                                   crs = "+proj=eck4 +datum=WGS84")

  # Attempt to perform cropping for efficiency FIRST
  # If it fails, validate first
  processed_layer <- tryCatch({
    layer_raw %>%
      sf::st_transform(crs = "+proj=eck4 +datum=WGS84") %>%
      group_by(scalerank, featurecla, min_zoom) %>%
      dplyr::summarize(geometry = sf::st_crop(geometry, le_projected)) %>%
      sf::st_transform(crs = sf::st_crs(latlong_extent))
  }, error = function(e) {
    layer_raw %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=eck4 +datum=WGS84") %>%
      group_by(scalerank, featurecla, min_zoom) %>%
      dplyr::summarize(geometry = sf::st_crop(geometry, le_projected)) %>%
      sf::st_transform(crs = sf::st_crs(latlong_extent))
  })

  # Then validate and filter (on the now smaller dataset)
  processed_layer <- processed_layer %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(geometry)) # Filter out empty geometries

  return(processed_layer)
}
