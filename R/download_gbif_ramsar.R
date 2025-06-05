library(rgbif)
library(sf)
library(dplyr) # For data manipulation if needed
library(utils)

# --- Configuration ---
# Base directory where your country-specific Ramsar site folders (containing WKT files) are.
base_ramsar_dir <- "inst/extdata/ramsar" # Ensure this path is correct

# Directory to save the downloaded GBIF data
output_gbif_base_dir <- "inst/extdata/gbif_ramsar" # Create this directory if it doesn't exist
if (!dir.exists(output_gbif_base_dir)) {
  dir.create(output_gbif_base_dir, recursive = TRUE)
}

# --- Optional GBIF API parameters (adjust as needed) ---
# You can add more filters here, e.g., taxonomy, date ranges
# See ?occ_download for full list of predicates
gbif_base_filters <- list(
  hasGeospatialIssue = FALSE, # Exclude records flagged with geospatial issues
  hasCoordinate = TRUE,       # Ensure records have coordinates
  basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "LITERATURE", "LIVING_SPECIMEN", "MATERIAL_SAMPLE", "MACHINE_OBSERVATION")
  # Exclude FOSSIL_SPECIMEN, GEOLOGICAL_SPECIMEN, etc., if not relevant for biodiversity
  # You might also add: year = "1990,2024" for a specific date range
  # country = "DE" for a specific country (though WKT is more precise)
)

# --- Get all country subdirectories ---
country_dirs <- list.dirs(base_ramsar_dir, recursive = FALSE, full.names = TRUE)

if (length(country_dirs) == 0) {
  stop("No country subdirectories found in: ", base_ramsar_dir, ". Please ensure the path is correct.")
}

message(paste("Found", length(country_dirs), "country WKTs to use for GBIF download."))

# --- Select only the first country for test ---
# Take the first element of the country_dirs vector
test_country_dir <- country_dirs[5]
# Create a new list/vector containing only this one directory
country_dirs_for_test <- c(test_country_dir)

message(paste("Running test for only the first country:", basename(test_country_dir)))

# --- Loop through only the selected test country directory ---
for (country_dir in country_dirs_for_test) {

  country_name_sanitized <- basename(country_dir)

  message(paste0("\n--- Attempting GBIF download for: ", country_name_sanitized, " ---"))

  wkt_file_path <- file.path(country_dir, paste0("ramsar_sites_", country_name_sanitized, ".wkt"))

  if (!file.exists(wkt_file_path)) {
    message(paste("  Skipping:", country_name_sanitized, "- WKT file not found at:", wkt_file_path))
    next
  }

  wkt_string <- readLines(wkt_file_path, warn = FALSE)[1]

  if (length(wkt_string) == 0 || nchar(wkt_string) < 10) { # Basic check for empty or too short WKT
    message(paste("  Skipping:", country_name_sanitized, "- WKT file is empty or invalid:", wkt_file_path))
    next
  }

  message(paste("  WKT loaded for", country_name_sanitized, ". Length:", nchar(wkt_string), "characters."))

  # --- Initiate GBIF Download Request ---

  # REFINED PREDICATE CONSTRUCTION HERE
  # Start with the geometry predicate
  all_predicates_list <- list(rgbif::pred("geometry", wkt_string))

  # Add other base filters, dynamically choosing pred() or pred_in()
  for (n in names(gbif_base_filters)) {
    val <- gbif_base_filters[[n]]
    if (length(val) == 1) {
      all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred(n, val)
    } else {
      all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred_in(n, val)
    }
  }

  final_predicate <- do.call(rgbif::pred_and, all_predicates_list) # Use the clean list here

  message(paste("  Requesting GBIF data for", country_name_sanitized, "..."))
  download_key <- tryCatch({
    occ_download(
      final_predicate,
      format = "SIMPLE_CSV"
    )
  }, error = function(e) {
    message(paste("  Error requesting GBIF download for", country_name_sanitized, ":", e$message))
    return(NULL)
  })

  if (is.null(download_key)) {
    message(paste("  Failed to initiate download for", country_name_sanitized, ". Skipping."))
    next
  }

  message(paste("  Download initiated. Key:", download_key))

  # --- Wait for Download Completion and Retrieve Data ---
  message("  Waiting for download to complete... (This may take several minutes)")
  download_status <- occ_download_wait(download_key)

  if (download_status$status != "SUCCEEDED") {
    message(paste("  Download failed for", country_name_sanitized, ". Status:", download_status$status))
    next
  }

  message(paste("  Download succeeded. Downloading data to local storage..."))
  # --- New: Create country-specific output directory ---
  country_output_dir <- file.path(output_gbif_base_dir, country_name_sanitized)
  if (!dir.exists(country_output_dir)) {
    dir.create(country_output_dir, recursive = TRUE)
    message(paste("  Created new directory:", country_output_dir))
  }

  # Download the ZIP file to the new country-specific directory
  zip_file_path <- tryCatch({
    occ_download_get(download_key, path = country_output_dir, overwrite = TRUE)
  }, error = function(e) {
    message(paste("  Error retrieving downloaded zip file for", country_name_sanitized, ":", e$message))
    return(NULL)
  })

  if (is.null(zip_file_path)) {
    message(paste("  Failed to retrieve GBIF zip file for", country_name_sanitized, ". Skipping."))
    next
  }

  message(paste("  GBIF data (zipped) saved for", country_name_sanitized, "to:", zip_file_path))

  # --- New: Unzip the file and rename ---
  message(paste("  Unzipping data for", country_name_sanitized, "..."))
  extracted_files <- tryCatch({
    utils::unzip(zip_file_path, exdir = country_output_dir)
  }, error = function(e) {
    message(paste("  Error unzipping file for", country_name_sanitized, ":", e$message))
    return(NULL)
  })

  if (is.null(extracted_files)) {
    message(paste("  Failed to unzip data for", country_name_sanitized, ". Skipping cleanup."))
    # Still remove zip even if unzip failed partially
    if (file.exists(zip_file_path)) file.remove(zip_file_path)
    next
  }

  # Find the main data CSV/TXT file (usually 'occurrence.txt' or 'data.csv')
  # Filter for files that are likely the main data, e.g., ending in .csv or .txt
  data_files <- extracted_files[grepl("\\.(csv|txt)$", extracted_files, ignore.case = TRUE)]

  if (length(data_files) > 0) {
    # Assuming the first data file found is the one we want
    old_csv_path <- data_files[1]
    new_csv_name <- paste0("gbif_occurrences_", country_name_sanitized, ".csv")
    new_csv_path <- file.path(country_output_dir, new_csv_name)

    # Rename the extracted file
    if (file.exists(old_csv_path)) { # Check if the file exists before renaming
      file.rename(old_csv_path, new_csv_path)
      message(paste("  Renamed extracted data to:", new_csv_path))
    } else {
      message(paste("  Warning: Extracted data file not found at expected path:", old_csv_path))
    }
  } else {
    message(paste("  Warning: No CSV or TXT file found in extracted archive for", country_name_sanitized))
  }

  # --- Optional: Remove the original ZIP file after extraction ---
  if (file.exists(zip_file_path)) {
    file.remove(zip_file_path)
    message(paste("  Removed original zip file:", zip_file_path))
  }

}

message("\n--- GBIF data download and unzipping process complete. ---")
