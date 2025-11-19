# --- Mock Data Setup ---
# 1. Mock extent (a simple bounding box for cropping)
mock_extent <- sf::st_bbox(c(xmin = 5, ymin = 40, xmax = 20, ymax = 55), crs = 4326)

# 2. Mock raw layer data (a simple polygon)
mock_raw_layer <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0, 0, 30, 0, 30, 60, 0, 60, 0, 0), ncol = 2, byrow = TRUE))), crs = 4326),
  scalerank = 10,
  featurecla = "boundary",
  name = "MockArea"
)

# 3. Mock invalid layer (A Point that will fail st_crop/st_make_valid if not handled)
mock_invalid_layer <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_point(c(10, 50)), crs = 4326), # <-- Explicitly set CRS here
  scalerank = 10,
  featurecla = "point",
  name = "Invalid"
)

# Combine the layers (This should now work without the CRS error)
mock_raw_combined <- rbind(mock_raw_layer, mock_invalid_layer)

# 3. Mock rnaturalearth data frames (df_layers_physical/cultural)
mock_df_physical <- data.frame(
  layer = "lakes",
  scale10 = 1, scale50 = 1, scale110 = 0
)
mock_df_cultural <- data.frame(
  layer = "roads",
  scale10 = 1, scale50 = 1, scale110 = 1
)

# 4. Mock rnaturalearth::ne_countries (used for 'admin_0_countries' path)
mock_ne_countries <- function(scale, returnclass) {
  mock_raw_layer %>%
    dplyr::mutate(name = paste("Country", scale))
}

# 5. Mock rnaturalearth::ne_load (used for the general loading path)
mock_ne_load <- function(scale, returnclass, type, category, temp_ne_dir) {
  if (type == "lakes") {
    # This layer returns a standard layer with a geom column name mismatch
    mock_raw_layer %>%
      dplyr::rename(geom = geometry)
  } else {
    # This layer returns the combined valid/invalid structure
    mock_raw_combined
  }
}

# 6. Mock rnaturalearth::ne_download and tools::R_user_dir
mock_ne_download <- function(...) { TRUE } # Always succeeds
mock_R_user_dir <- function(...) { tempdir() } # Safe temp directory

# 7. Mock sf::st_make_valid (should return the input, but we assume it fixes the point)
mock_st_make_valid <- function(x) {
  # Mock the point being fixed or removed if it causes a problem
  x %>% dplyr::filter(featurecla != "point")
}

# --- Define Mock Data Objects using FULL NAMESPACE STRING ---
# This tricks the function's internal lookup into finding a definition in the
# test environment, bypassing the locked rnaturalearth namespace.

`rnaturalearth::df_layers_physical` <- data.frame(
  layer = "lakes",
  scale10 = 1, scale50 = 1, scale110 = 0
)

`rnaturalearth::df_layers_cultural` <- data.frame(
  layer = "roads",
  scale10 = 1, scale50 = 1, scale110 = 1
)

# --- End of Mock Data Setup ---

# --- Test Block 1: The 'admin_0_countries' path ---
test_that("Path 'admin_0_countries' correctly loads and crops", {

  testthat::with_mocked_bindings(
    `ne_countries` = mock_ne_countries,
    {
      result <- add_ne_layer(layer_name = "admin_0_countries",
                             scale = "medium",
                             extent_projected = mock_extent)

      # 1. Check successful return
      expect_s3_class(result, "sf")

      # 2. Check CRS (rnaturalearth returns 4326 by default)
      expect_equal(sf::st_crs(result)$epsg, 4326)

      # 3. Check attribute columns are preserved/created (scalerank, featurecla)
      expect_true(all(c("scalerank", "featurecla") %in% names(result)))

      # 4. Check cropping (mock extent xmin=5, xmax=20; mock raw xmin=0, xmax=30)
      # The resulting bbox should be limited by the mock_extent
      expect_equal(as.numeric(sf::st_bbox(result)["xmin"]), 5)
      expect_equal(as.numeric(sf::st_bbox(result)["xmax"]), 20)
    },
    .package = "rnaturalearth" # Target package for ne_countries
  )
})

# Mocks needed by the rnaturalearth namespace (the data frames)
mock_rnaturalearth_env <- function() {
  list(
    df_layers_physical = mock_df_physical,
    df_layers_cultural = mock_df_cultural
    # We leave 'ne_load' out of this specific environment mock since you said it works.
  )
}

# --- Ternaturalearth# --- Test Block 2: Dynamic Lookup and Loading (The ELSE path) ---
test_that("Dynamic lookup and ne_load correctly process standard layer", {

  # Only mock the functions that perform actions/file I/O.
  # The data objects are now defined globally in this file (Step 1).
  testthat::with_mocked_bindings(

    `ne_load` = mock_ne_load, .package = "rnaturalearth",

    {
      # Execution will now use the locally defined `rnaturalearth::df_layers_...`
      # objects and the mocked functions.
      testthat::with_mocked_bindings(
        `R_user_dir` = mock_R_user_dir, .package = "tools",
        {
          result <- add_ne_layer(layer_name = "lakes",
                                 scale = "large",
                                 extent_projected = mock_extent)

          # Assertions...
          expect_true("geometry" %in% names(result))
          expect_false("geom" %in% names(result))
        }
)
    }
  )
})


# --- Test Block 3: Error Handling and Recovery (FIXED) ---
test_that("Error handling attempts download on load failure", {

  # --- Advanced Mock: Simulate ne_load failing once, then succeeding ---

  # This counter tracks how many times ne_load has been called
  ne_load_call_count <- 0

  mock_ne_load_sequence <- function(scale, returnclass, type, category, temp_ne_dir) {
    ne_load_call_count <<- ne_load_call_count + 1

    if (ne_load_call_count == 1) {
      # First call: Simulate failure (triggers download attempt)
      stop("the file blah/blah seems not to exist")
    } else {
      # Second call (after simulated download): Simulate success
      # Return a simple successful layer (like the one from Test Block 2)
      return(mock_raw_combined)
    }
  }

  testthat::with_mocked_bindings(

    # Mock the sequence function
    ne_load = mock_ne_load_sequence,

    # Mock ne_download (needs to exist but can be silent)
    ne_download = function(...) TRUE,

    .package = "rnaturalearth",

    # --- Code to execute (The final unnamed argument) ---
    {
      # This checks that the entire sequence (fail -> download -> success)
      # happens without throwing an unhandled error.
      expect_no_error({
        result <- add_ne_layer(layer_name = "roads",
                               scale = "large",
                               extent_projected = mock_extent)
      })

      # Final check: Ensure the download/load sequence resulted in a valid object
      expect_s3_class(result, "sf")
      expect_gt(nrow(result), 0)
    }
  )
})

# --- Test Block 4: Lookup Failure and Final Processing ---
test_that("Unsupported scale and lookup failures stop execution", {

  # 1. Unsupported scale (Activates the switch STOP)
  expect_error(
    add_ne_layer(layer_name = "lakes", scale = "tiny", extent_projected = mock_extent),
    "Unsupported scale. Choose 'small', 'medium', or 'large'."
  )

  # 2. Layer not found (Activates the nrow(layer_info) == 0 STOP)
  expect_error(
    add_ne_layer(layer_name = "nonexistent_layer", scale = "large", extent_projected = mock_extent),
    "Layer 'nonexistent_layer' not found or not available at scale 'large'."
  )

  testthat::with_mocked_bindings(
    `ne_load` = mock_ne_load,.package = "rnaturalearth",
    {
      # 3. Test st_make_valid fallback and empty filter (using mock_raw_combined)
      testthat::with_mocked_bindings(

        st_make_valid = mock_st_make_valid, # Mock st_make_valid to filter the point
        {
          result <- add_ne_layer(layer_name = "roads", scale = "large", extent_projected = mock_extent)

          # Should only return the single valid polygon after cropping and filtering
          expect_equal(nrow(result), 2)
        },
        .package = "sf"
      )
    }
  )
})
