# --- Mock Data Setup ---

# Data that crosses two different UTM zones (32 and 33) and hemispheres (North and South)
mock_mgrs_df <- data.frame(
  cellCode = c("32U LV 85600 59900",
               "33T WA 12345 67890",
               "18C FG 12345 67890",
               "19D JH 54321 98765"),
  xcoord = c(485600, 312345, 312345, 543210),
  ycoord = c(5990000, 5678900, 8100000, 7800000),
  value = 1:4
)

# --- Test Block 1: Input Validation ---
test_that("Function stops on missing required coordinate columns", {

  # 1. Missing xcoord
  df_missing_x <- mock_mgrs_df %>% select(-xcoord)
  expect_error(
    mgrs_to_latlong_bbox(df_missing_x),
    "Columns 'xcoord' and/or 'ycoord' not found in data frame.",
    fixed = TRUE
  )

  # 2. Missing ycoord
  df_missing_y <- mock_mgrs_df %>% select(-ycoord)
  expect_error(
    mgrs_to_latlong_bbox(df_missing_y),
    "Columns 'xcoord' and/or 'ycoord' not found in data frame.",
    fixed = TRUE
  )
})


# --- Test Block 2: Core Functionality and Multi-Zone Handling ---
test_that("MGRS data correctly converted and combined into a single bbox", {

  # Run the function on the complex dataset
  result_bbox <- mgrs_to_latlong_bbox(mock_mgrs_df)

  # 1. Check the class and structure of the result
  expect_s3_class(result_bbox, "bbox")
  expect_equal(length(result_bbox), 4)

  # 2. Check the CRS of the resulting bbox (must be EPSG:4326)
  expect_equal(st_crs(result_bbox)$epsg, 4326)

  # 3. Check the calculated extent (to ensure all points were transformed and merged)

  # Expected min/max values after transformation to EPSG:4326 (WGS84)
  # Based on MGRS coordinates:
  # Pt 1 (32U) is approx 10.5E, 54.0N
  # Pt 2 (33T) is approx 13.5E, 51.2N
  # Pt 3 (18C) is approx -75.7E, -17.1S
  # Pt 4 (19D) is approx -70.4E, -19.7S

  # Expected minimum longitude (xmin) should come from S hemisphere data (Pt 3)
  expect_gt(result_bbox["xmin"], -77)
  expect_lt(result_bbox["xmin"], -76)

  # Expected maximum longitude (xmax) should come from N hemisphere data (Pt 2)
  expect_gt(result_bbox["xmax"], 12)
  expect_lt(result_bbox["xmax"], 13)

  # Expected minimum latitude (ymin) should come from S hemisphere data (Pt 4)
  expect_gt(result_bbox["ymin"], -20)
  expect_lt(result_bbox["ymin"], -19)

  # Expected maximum latitude (ymax) should come from N hemisphere data (Pt 1)
  expect_gt(result_bbox["ymax"], 54)
  expect_lt(result_bbox["ymax"], 55)
})
