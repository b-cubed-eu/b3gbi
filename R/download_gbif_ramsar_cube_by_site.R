library(rgbif)
library(sf)
library(dplyr)

# --- Configuration for GBIF downloads ---
# Make sure your GBIF credentials are set as R options or environment variables!
# e.g., options(gbif_user = "YOUR_GBIF_USERNAME", gbif_pwd = "YOUR_GBIF_PASSWORD", gbif_email = "your.email@example.com")
# Replace "YOUR_GBIF_USERNAME", "YOUR_GBIF_PASSWORD", "your.email@example.com" with your actual credentials.

gbif_user <- getOption("gbif_user")
gbif_pwd <- getOption("gbif_pwd")
gbif_email <- getOption("gbif_email")

if (is.null(gbif_user) || is.null(gbif_pwd) || is.null(gbif_email)) {
  stop("GBIF credentials (user, pwd, email) are not set. Please set them using options() or environment variables before running.")
}

# Define the base directory where your BBOX WKT files are located
wkt_input_base_dir <- "inst/extdata/ramsar_sites_wkt_bbox" # Points to your BBOX WKTs

# Directory to save the downloaded GBIF Data Cubes
output_gbif_cubes_base_dir <- "data/gbif_data_cubes_mgrs" # Changed directory for MGRS cubes
if (!dir.exists(output_gbif_cubes_base_dir)) {
  dir.create(output_gbif_cubes_base_dir, recursive = TRUE)
  message(paste("Created base GBIF data cube output directory:", output_gbif_cubes_base_dir))
}

# --- GBIF API parameters for the Data Cube ---
gbif_cube_filters <- list(
  hasGeospatialIssue = FALSE,
  hasCoordinate = TRUE,
  basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "LITERATURE", "LIVING_SPECIMEN", "MATERIAL_SAMPLE", "MACHINE_OBSERVATION")
  # Add other filters here if needed, e.g.:
  # year = "1990,2024",
  # taxonKey = 212 # Example: filter for Homo sapiens
)

# --- Data Cube specific parameters - YOU MUST DEFINE THESE! ---
# 1. Choose your desired measurement type for the cube:
#    - "OCCURRENCE_COUNT": Number of raw occurrences in each grid cell.
#    - "SPECIES_COUNT": Number of unique species in each grid cell (species richness).
#    - Refer to GBIF Gridded Data API for other possibilities if you need them.
cube_measurement_type <- "SPECIES_COUNT" # <--- IMPORTANT: CHOOSE YOUR DESIRED CUBE TYPE

# 2. Define the grid system and resolution
cube_grid_system <- "MGRS"
cube_resolution <- "1km" # "1km", "100m", "10m", "1m" (use "1km" or finer as needed, warning about size)

# --- SELECT A SINGLE RAMSAR SITE BBOX WKT FILE FOR TESTING ---
# IMPORTANT: Adjust this path to one of your successfully generated _bbox.wkt files.
# Example: file.path(wkt_input_base_dir, "Andorra", "site_1_Estanys_de_Malniu_bbox.wkt")
test_bbox_wkt_filepath <- file.path(wkt_input_base_dir, "Andorra", "site_1_Estanys_de_Malniu_bbox.wkt") # <--- REPLACE WITH A REAL PATH

if (!file.exists(test_bbox_wkt_filepath)) {
  stop("The specified test_bbox_wkt_filepath does not exist: ", test_bbox_wkt_filepath,
       ". Please choose a valid path to an existing BBOX WKT file.")
}

message(paste0("\n--- Processing single test site for MGRS DATA CUBE: ", basename(test_bbox_wkt_filepath), " ---"))
message(paste0("  Measurement Type: ", cube_measurement_type))
message(paste0("  Grid System: ", cube_grid_system, ", Resolution: ", cube_resolution))

# 1. Read the BBOX WKT string from the file
message(paste("Reading BBOX WKT from:", test_bbox_wkt_filepath))
wkt_string_test <- readLines(test_bbox_wkt_filepath, warn = FALSE)[1]
if (length(wkt_string_test) == 0 || nchar(wkt_string_test) < 10) {
  stop("Failed to read a valid WKT string from: ", test_bbox_wkt_filepath)
}

# 2. Extract site ID and name from the filename for output organization
# Assuming filename format is "ramsar_site_id_sanitized_site_name_bbox.wkt"
filename_parts <- strsplit(basename(test_bbox_wkt_filepath), "_", fixed = TRUE)[[1]]
ramsar_site_id_test <- filename_parts[1]
sanitized_site_name_test <- paste(filename_parts[2:(length(filename_parts)-2)], collapse = "_")
test_country_name <- basename(dirname(test_bbox_wkt_filepath))

# Create specific output directory for this site's GBIF data cube
site_cube_output_dir <- file.path(output_gbif_cubes_base_dir, test_country_name, paste0(ramsar_site_id_test, "_", sanitized_site_name_test))
if (!dir.exists(site_cube_output_dir)) {
  dir.create(site_cube_output_dir, recursive = TRUE)
  message(paste("Created GBIF data cube output directory for test site:", site_cube_output_dir))
}

# Define the output raster file path (GeoTIFF, as MGRS cubes are usually GeoTIFFs)
output_raster_filename <- paste0("gbif_datacube_", ramsar_site_id_test, "_", cube_measurement_type, "_", cube_grid_system, "_", cube_resolution, ".tif")
output_raster_path <- file.path(site_cube_output_dir, output_raster_filename)


# 3. Initiate the GBIF Data Cube download request
message("Initiating GBIF Data Cube download for test site...")
# Construct the predicate list dynamically
all_predicates_list <- list(rgbif::pred("geometry", wkt_string_test))

for (n in names(gbif_cube_filters)) {
  val <- gbif_cube_filters[[n]]
  if (length(val) == 1) {
    all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred(n, val)
  } else {
    all_predicates_list[[length(all_predicates_list) + 1]] <- rgbif::pred_in(n, val)
  }
}
final_predicate <- do.call(rgbif::pred_and, all_predicates_list)

gbif_download_request <- tryCatch({
  occ_download_cube(
    final_predicate,
    measurementType = cube_measurement_type,
    gridSystem = cube_grid_system, # MGRS grid system
    resolution = cube_resolution,   # MGRS resolution (e.g., "1km")
    user = gbif_user,
    pwd = gbif_pwd,
    email = gbif_email
  )
}, error = function(e) {
  message(paste("Error initiating GBIF Data Cube download:", e$message))
  return(NULL)
})

if (is.null(gbif_download_request)) {
  stop("Failed to initiate GBIF Data Cube download request. Check error messages above and GBIF credentials.")
}

download_key <- gbif_download_request # For cubes, occ_download_cube directly returns the key
message(paste("GBIF Data Cube download initiated. Key:", download_key))
message("Waiting for data cube to be processed... (This may take significant time depending on area/resolution)")

# 4. Wait for the download to complete
gbif_download_status <- tryCatch({
  occ_download_wait(key = download_key)
}, error = function(e) {
  message(paste("Error waiting for GBIF Data Cube download:", e$message))
  return(NULL)
})

if (is.null(gbif_download_status) || gbif_download_status$status != "SUCCEEDED") {
  stop(paste("GBIF Data Cube download did not succeed for key:", download_key, ". Status:", gbif_download_status$status,
             ". Check GBIF portal for more details on download", download_key))
}

message(paste("GBIF Data Cube download for key", download_key, "SUCCEEDED."))

# 5. Get the downloaded data (which will be a GeoTIFF)
message("Retrieving GBIF Data Cube...")
downloaded_file_path <- tryCatch({
  occ_download_get(key = download_key, path = site_cube_output_dir)
}, error = function(e) {
  message(paste("Error retrieving GBIF Data Cube:", e$message))
  return(NULL)
})

if (is.null(downloaded_file_path)) {
  stop("Failed to retrieve GBIF Data Cube. Check error messages above.")
}

# The downloaded file is typically a .tif, but GBIF might zip it if multiple layers or large.
# Unzip if it's a zip archive (e.g., download.zip)
if (tools::file_ext(downloaded_file_path) %in% c("zip", "gz")) {
  message("Downloaded file is a compressed archive, attempting to unzip.")
  extracted_files <- tryCatch({
    utils::unzip(downloaded_file_path, exdir = site_cube_output_dir)
  }, error = function(e) {
    message(paste("Error unzipping downloaded file:", e$message))
    return(NULL)
  })
  if (is.null(extracted_files) || length(extracted_files) == 0) {
    stop("Failed to unzip or found no files after unzipping.")
  }
  # Find the GeoTIFF within the unzipped files
  tif_files <- extracted_files[grepl("\\.tif$", extracted_files, ignore.case = TRUE)]
  if (length(tif_files) > 0) {
    # If multiple TIFs, you might need to select carefully, but usually it's one main one.
    file.rename(tif_files[1], output_raster_path) # Rename the extracted TIF
    # Clean up other extracted files if any, and the original zip
    other_files <- setdiff(extracted_files, tif_files[1])
    if(length(other_files) > 0) file.remove(other_files)
    file.remove(downloaded_file_path) # Remove the original zip
    message(paste("Unzipped and renamed GeoTIFF to:", output_raster_path))
  } else {
    stop("No GeoTIFF found after unzipping the downloaded file.")
  }
} else {
  # If it's already a .tif or similar, just rename it
  file.rename(downloaded_file_path, output_raster_path)
  message(paste("GBIF Data Cube (GeoTIFF) saved to:", output_raster_path))
}


message("\n--- Single site GBIF Data Cube (MGRS) download test complete. ---")
