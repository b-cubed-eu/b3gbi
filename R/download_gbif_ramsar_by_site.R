library(rgbif)
library(sf)
library(dplyr)
library(utils) # For unzip

# --- Configuration for GBIF downloads ---
# Ensure your GBIF credentials are set as R options or environment variables!
# e.g., options(gbif_user = "YOUR_GBIF_USERNAME") etc.
# gbif_user <- getOption("gbif_user")
# gbif_pwd <- getOption("gbif_pwd")
# gbif_email <- getOption("gbif_email")

# if (is.null(gbif_user) || is.null(gbif_pwd) || is.null(gbif_email)) {
#   stop("GBIF credentials (user, pwd, email) are not set. Please set them using options() or environment variables.")
# }

# --- Base directory where your individual Ramsar site WKT files are located ---
# This should be the 'output_wkt_base_dir' from your WKT generation script.
wkt_input_base_dir <- "inst/extdata/ramsar_sites_wkt"
if (!dir.exists(wkt_input_base_dir)) {
  stop("WKT input directory not found: ", wkt_input_base_dir, ". Please ensure WKT files are generated first.")
}

# --- Directory to save the downloaded GBIF data ---
# This will be the parent directory for country/site specific GBIF data.
output_gbif_base_dir <- "inst/extdata/gbif_occurrences_by_ramsar_site" # Changed to 'data/' for common practice
if (!dir.exists(output_gbif_base_dir)) {
  dir.create(output_gbif_base_dir, recursive = TRUE)
  message(paste("Created base GBIF output directory:", output_gbif_base_dir))
}

# --- Optional GBIF API parameters (adjust as needed) ---
gbif_base_filters <- list(
  hasGeospatialIssue = FALSE,
  hasCoordinate = TRUE,
  basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "LITERATURE", "LIVING_SPECIMEN", "MATERIAL_SAMPLE", "MACHINE_OBSERVATION")
  # year = "1990,2024" # Example date range
)

# --- Find all individual WKT files ---
# This will list all .wkt files recursively within the wkt_input_base_dir
all_wkt_files <- list.files(wkt_input_base_dir, pattern = "\\.wkt$", recursive = TRUE, full.names = TRUE)

if (length(all_wkt_files) == 0) {
  stop("No WKT files found in: ", wkt_input_base_dir, ". Please ensure WKT files were generated successfully.")
}

message(paste("Found", length(all_wkt_files), "individual Ramsar site WKT files to process."))

# --- SELECT ONLY ONE SITE FOR TESTING ---
# Take the first WKT file found for testing.
test_wkt_filepath <- all_wkt_files[1] # <--- This selects the first site found
test_wkt_filepath <- "inst/extdata/ramsar_sites_wkt/Switzerland/site_1_Le Rh_ne genevoi_ _ Vallon_ de l_Allondon et de la Laire_bbox.wkt"

# Create a list containing only this one WKT file for the loop
wkt_files_to_process <- c(test_wkt_filepath)

message(paste("\nRunning test for only the first site found:", basename(test_wkt_filepath)))

# --- Loop through each individual WKT file ---
# Change 'wkt_files_to_process' to 'all_wkt_files' for a full run
for (wkt_filepath in wkt_files_to_process) {

  # Extract country name, site ID, and sanitized site name from the file path
  # Path example: inst/extdata/ramsar_sites_wkt/CountryName/site_ID_SiteName.wkt
  path_parts <- strsplit(wkt_filepath, .Platform$file.sep, fixed = TRUE)[[1]]
  # The country name is the second to last part of the path
  country_name <- path_parts[length(path_parts) - 1]
  # The filename is the last part
  filename_full <- basename(wkt_filepath)
  # Split filename to get site ID and sanitized site name
  filename_parts_split <- strsplit(filename_full, "_", fixed = TRUE)[[1]]
  ramsar_site_id <- filename_parts_split[1]
  # Reconstruct sanitized site name (all parts after ID and before .wkt)
  sanitized_site_name <- paste(filename_parts_split[2:(length(filename_parts_split)-1)], collapse = "_")
  sanitized_site_name <- gsub("\\.wkt$", "", sanitized_site_name) # Remove .wkt extension

  message(paste0("\n--- Attempting GBIF download for site: ", sanitized_site_name, " (ID: ", ramsar_site_id, ", Country: ", country_name, ") ---"))

  # Read the WKT string from the file
  wkt_string <- readLines(wkt_filepath, warn = FALSE)[1]

  if (length(wkt_string) == 0 || nchar(wkt_string) < 10) {
    message(paste("  Skipping site", ramsar_site_id, ": WKT file is empty or invalid."))
    next
  }

  message(paste("  WKT loaded for site", ramsar_site_id, ". Length:", nchar(wkt_string), "characters."))

  # --- Initiate GBIF Download Request ---
  all_predicates_list <- list(rgbif::pred("geometry", wkt_string))

  for (n in names(gbif_base_filters)) {
    val <- gbif_base_filters[[n]]
    if (length(val) == 1) {
      all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred(n, val)
    } else {
      all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred_in(n, val)
    }
  }

  final_predicate <- do.call(rgbif::pred_and, all_predicates_list)

  message(paste("  Requesting GBIF data for site", ramsar_site_id, "..."))
  download_key <- tryCatch({
    occ_download(
      final_predicate,
      format = "SIMPLE_CSV",
      user = getOption("gbif_user"), # Ensure credentials are set!
      pwd = getOption("gbif_pwd"),
      email = getOption("gbif_email")
    )
  }, error = function(e) {
    message(paste("  Error requesting GBIF download for site", ramsar_site_id, ":", e$message))
    return(NULL)
  })

  if (is.null(download_key)) {
    message(paste("  Failed to initiate download for site", ramsar_site_id, ". Skipping."))
    next
  }

  message(paste("  Download initiated. Key:", download_key))

  # --- Wait for Download Completion and Retrieve Data ---
  message("  Waiting for download to complete... (This may take several minutes)")
  download_status <- occ_download_wait(download_key)

  if (download_status$status != "SUCCEEDED") {
    message(paste("  Download failed for site", ramsar_site_id, ". Status:", download_status$status))
    next
  }

  message(paste("  Download succeeded. Downloading data to local storage..."))

  # --- Create site-specific output directory for GBIF data ---
  site_gbif_output_dir <- file.path(output_gbif_base_dir, country_name, paste0(ramsar_site_id, "_", sanitized_site_name))
  if (!dir.exists(site_gbif_output_dir)) {
    dir.create(site_gbif_output_dir, recursive = TRUE)
    message(paste("  Created new GBIF directory:", site_gbif_output_dir))
  }

  # Download the ZIP file to the new site-specific directory
  zip_file_path <- tryCatch({
    occ_download_get(download_key, path = site_gbif_output_dir, overwrite = TRUE)
  }, error = function(e) {
    message(paste("  Error retrieving downloaded zip file for site", ramsar_site_id, ":", e$message))
    return(NULL)
  })

  if (is.null(zip_file_path)) {
    message(paste("  Failed to retrieve GBIF zip file for site", ramsar_site_id, ". Skipping."))
    next
  }

  message(paste("  GBIF data (zipped) saved for site", ramsar_site_id, "to:", zip_file_path))

  # --- Unzip the file and rename ---
  message(paste("  Unzipping data for site", ramsar_site_id, "..."))
  extracted_files <- tryCatch({
    utils::unzip(zip_file_path, exdir = site_gbif_output_dir)
  }, error = function(e) {
    message(paste("  Error unzipping file for site", ramsar_site_id, ":", e$message))
    return(NULL)
  })

  if (is.null(extracted_files)) {
    message(paste("  Failed to unzip data for site", ramsar_site_id, ". Skipping cleanup."))
    if (file.exists(zip_file_path)) file.remove(zip_file_path)
    next
  }

  # Find the main data CSV/TXT file (usually 'occurrence.txt' or 'data.csv')
  data_files <- extracted_files[grepl("\\.(csv|txt)$", extracted_files, ignore.case = TRUE)]

  if (length(data_files) > 0) {
    old_csv_path <- data_files[1]
    new_csv_name <- paste0("gbif_occurrences_", ramsar_site_id, "_", sanitized_site_name, ".csv")
    new_csv_path <- file.path(site_gbif_output_dir, new_csv_name)

    if (file.exists(old_csv_path)) {
      file.rename(old_csv_path, new_csv_path)
      message(paste("  Renamed extracted data to:", new_csv_path))
    } else {
      message(paste("  Warning: Extracted data file not found at expected path:", old_csv_path))
    }
  } else {
    message(paste("  Warning: No CSV or TXT file found in extracted archive for site", ramsar_site_id))
  }

  # --- Optional: Remove the original ZIP file after extraction ---
  if (file.exists(zip_file_path)) {
    file.remove(zip_file_path)
    message(paste("  Removed original zip file:", zip_file_path))
  }
}

message("\n--- GBIF data download and unzipping process complete. ---")
