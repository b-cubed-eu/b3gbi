# Load global ramsar shapefile
ramsar_global <- sf::st_read("inst/extdata/Ramsar_boundaries/features_publishedPolygon.shp")

ramsar_global %>% # fix bad country names
  dplyr::mutate(country_en = dplyr::case_when(
    country_en == "C\xf4te d'Ivoire" ~ "Cote dIvoire",
    country_en == "T\xfcrkiye" ~ "Turkey",
    country_en == "Viet Nam" ~ "Vietnam",
    TRUE ~ country_en
  )) -> ramsar_global

# Step through countries
for (i in unique(ramsar_global$country_en)) {
  # Get data for country
  ramsar_country <- ramsar_global[ramsar_global$country_en == i, ]

  # Create directory for country if it doesn't exist
  dir.create(file.path("inst/ext_data", "ramsar", i), showWarnings = FALSE, recursive = TRUE)

  # Save the individual country's shapefile
  sf::st_write(ramsar_country, dsn = paste0("inst/extdata/", "ramsar/", i, "/ramsar_sites_", i, ".shp"), append = FALSE)
}


library(sf)

# --- Configuration ---
# Set the base directory where your country-specific Ramsar site folders are located.
# This should now point to 'inst/extdata/ramsar/'
base_ramsar_dir <- "inst/extdata/ramsar" # Adjust this path if your 'inst' is in a different location relative to your R script

# --- Get all country subdirectories ---
# list.dirs() finds all directories. We'll filter to get only the immediate country subdirectories.
country_dirs <- list.dirs(base_ramsar_dir, recursive = FALSE, full.names = TRUE)

if (length(country_dirs) == 0) {
  stop("No country subdirectories found in: ", base_ramsar_dir, ". Please check the path and ensure it's correct.")
}

message(paste("Found", length(country_dirs), "country directories to process."))

# --- Loop through each country directory ---
for (country_dir in country_dirs) {

  # Extract the country name from the directory path
  # This assumes the last part of the path is the country name (e.g., "Germany" from ".../ramsar/Germany")
  country_name_sanitized <- basename(country_dir)

  message(paste0("\n--- Processing directory: ", country_name_sanitized, " ---"))

  # Construct the expected path to the shapefile within this directory
  # The shapefile name now follows the pattern 'ramsar_sites_countryname.shp'
  shapefile_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".shp"))

  # Check if the shapefile actually exists before attempting to read
  if (!file.exists(shapefile_path)) {
    message(paste("  Skipping:", country_name_sanitized, "- Shapefile not found at:", shapefile_path))
    next # Skip to the next directory in the loop
  }

  # --- Read the shapefile ---
  message(paste("  Reading shapefile:", shapefile_path))
  ramsar_country_sf <- tryCatch({
    st_read(shapefile_path, quiet = TRUE) # quiet=TRUE suppresses some console messages
  }, error = function(e) {
    message(paste("  Error reading shapefile for", country_name_sanitized, ":", e$message))
    return(NULL) # Return NULL to indicate an error
  })

  # If there was an null value after reading the shapefile, skip to the next directory
  if (is.null(ramsar_country_sf)) {
    next
  }

  # Ensure the object has a valid geometry column
  if (!inherits(ramsar_country_sf, "sf") || is.null(st_geometry(ramsar_country_sf))) {
    message(paste("  Skipping", country_name_sanitized, ": No valid simple feature geometry found after reading."))
    next
  }

  # --- Attempt to make geometries valid ---
  message(paste("  Attempting to make geometries valid for", country_name_sanitized, "..."))
  ramsar_country_sf_valid <- tryCatch({
    st_make_valid(ramsar_country_sf)
  }, error = function(e) {
    message(paste("  Error making geometries valid for", country_name_sanitized, ":", e$message))
    return(NULL) # Return NULL if make_valid itself fails
  })

  if (is.null(ramsar_country_sf_valid)) {
    message(paste("  Skipping WKT creation for", country_name_sanitized, "due to failed validation."))
    next
  }

  # --- Create and Save WKT File ---

  # Define output file path for WKT, now following the 'ramsar_sites_countryname.wkt' pattern
  output_wkt_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".wkt"))

  # Combine geometries for the current country into a single (multi)polygon WKT string
  combined_geometry <- tryCatch({
    st_union(ramsar_country_sf_valid) # Use the validated geometries for union
  }, error = function(e) {
    message(paste("  Warning: Could not union VALIDATED geometries for", country_name_sanitized, ". Error:", e$message))
    return(NULL)
  })

  if (!is.null(combined_geometry) && !st_is_empty(combined_geometry)) {
    # Convert the combined geometry to WKT
    # You can add `digits = X` to control precision if needed (e.g., `digits = 6`)
    wkt_string <- st_as_text(combined_geometry)

    # Save the WKT string to a .wkt file
    message(paste("  Saving WKT to:", output_wkt_path))
    writeLines(wkt_string, output_wkt_path)
  } else {
    message(paste("  No valid or unionable geometry to convert to WKT for", country_name_sanitized, ". Skipping WKT file creation."))
  }
}

message("\n--- WKT conversion process complete. ---")






message("--- Starting retry process for failed WKT conversions ---")

# --- Identify countries that failed WKT creation ---
all_country_dirs <- list.dirs(base_ramsar_dir, recursive = FALSE, full.names = TRUE)

if (length(all_country_dirs) == 0) {
  stop("No country subdirectories found in: ", base_ramsar_dir, ". Please check the path.")
}

failed_country_dirs <- c()

for (country_dir in all_country_dirs) {
  country_name_sanitized <- basename(country_dir)
  wkt_file_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".wkt"))

  if (!file.exists(wkt_file_path)) {
    failed_country_dirs <- c(failed_country_dirs, country_dir)
  }
}

if (length(failed_country_dirs) == 0) {
  message("No missing WKT files found. All countries successfully processed in the previous run or no shapefiles present.")
  message("--- Retry process finished. ---")
  q(save="no") # Exit script if nothing to do
} else {
  message(paste("\nFound", length(failed_country_dirs), "countries with missing WKT files. Retrying with s2=FALSE."))
  message("Countries to retry: ", paste(basename(failed_country_dirs), collapse = ", "))
}


# --- Loop through only the identified failed country directories ---
for (country_dir in failed_country_dirs) {

  country_name_sanitized <- basename(country_dir)

  message(paste0("\n--- Retrying directory: ", country_name_sanitized, " ---"))

  shapefile_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".shp"))

  if (!file.exists(shapefile_path)) {
    message(paste("  Skipping retry:", country_name_sanitized, "- Shapefile not found at:", shapefile_path))
    next # Skip to the next directory if shapefile itself is missing
  }

  # --- Read the shapefile ---
  message(paste("  Reading shapefile:", shapefile_path))
  ramsar_country_sf <- tryCatch({
    st_read(shapefile_path, quiet = TRUE)
  }, error = function(e) {
    message(paste("  Error reading shapefile for", country_name_sanitized, ":", e$message))
    return(NULL)
  })

  if (is.null(ramsar_country_sf)) {
    next
  }

  if (!inherits(ramsar_country_sf, "sf") || is.null(st_geometry(ramsar_country_sf))) {
    message(paste("  Skipping retry", country_name_sanitized, ": No valid simple feature geometry found after reading."))
    next
  }

  # --- TEMPORARILY DISABLE S2 FOR THIS OPERATION BLOCK ---
  original_s2_setting <- sf_use_s2() # Store original setting
  sf_use_s2(FALSE) # Use planar geometry engine (GEOS)
  message("  S2 geometry engine temporarily disabled.")


  # --- Attempt to make geometries valid ---
  message(paste("  Attempting to make geometries valid (GEOS engine) for", country_name_sanitized, "..."))
  ramsar_country_sf_valid <- tryCatch({
    st_make_valid(ramsar_country_sf)
  }, error = function(e) {
    message(paste("  Error making geometries valid for", country_name_sanitized, ":", e$message))
    return(NULL)
  })

  if (is.null(ramsar_country_sf_valid)) {
    message(paste("  Skipping WKT creation for", country_name_sanitized, "due to failed validation."))
    # Restore s2 setting before continuing to next iteration
    sf_use_s2(original_s2_setting)
    next
  }

  # --- Create and Save WKT File ---
  output_wkt_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".wkt"))

  combined_geometry <- tryCatch({
    st_union(ramsar_country_sf_valid) # Union using planar engine
  }, error = function(e) {
    message(paste("  Warning: Could not union VALIDATED geometries for", country_name_sanitized, ". Error:", e$message))
    return(NULL)
  })

  # --- RESTORE ORIGINAL S2 SETTING ---
  sf_use_s2(original_s2_setting)
  message("  S2 geometry engine restored to original setting.")


  if (!is.null(combined_geometry) && !st_is_empty(combined_geometry)) {
    wkt_string <- st_as_text(combined_geometry)
    message(paste("  Saving WKT to:", output_wkt_path))
    writeLines(wkt_string, output_wkt_path)
  } else {
    message(paste("  No valid or unionable geometry to convert to WKT for", country_name_sanitized, ". Skipping WKT file creation."))
  }
}

message("\n--- Retry process complete. ---")
