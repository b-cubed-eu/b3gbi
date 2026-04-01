# Mock my_DataInfo to avoid slow iNEXT calculations
mock_DataInfo <- function(x, ...) {
    tibble::tibble(
        Assemblage = names(x),
        SC = rep(0.8, length(x))
    )
}

# Mock get_ne_data to avoid slow NE downloads and PROJ issues on CI
mock_get_ne_data <- function(projected_crs, latlong_bbox = NULL, ...) {
    if (is.null(projected_crs)) stop("No projected CRS provided.")
    # Create polygon directly in the target CRS to avoid st_transform on CI
    # Use the coord_range from the mock cube (Denmark area in EPSG:4326)
    dummy_geom <- sf::st_sfc(
        sf::st_polygon(list(matrix(c(
            3, 51, 13, 51, 13, 60, 3, 60, 3, 51
        ), ncol = 2, byrow = TRUE))),
        crs = 4326
    )
    list(
        combined = sf::st_sf(geometry = sf::st_transform(dummy_geom, projected_crs)),
        saved = sf::st_sf(geometry = sf::st_transform(dummy_geom, projected_crs))
    )
}

test_that("completeness_map works with isea3h cube", {
    skip_if_not(
        requireNamespace("dggridR", quietly = TRUE),
        "dggridR not available for safe ISEA3H grid creation"
    )

    mock_data <- tibble::tibble(
        cellCode = rep(c("-458282525011250000", "452583359004665569"), each = 10),
        year = rep(2020:2021, 10),
        obs = rpois(20, 5) + 1,
        taxonKey = rep(1:10, 2),
        scientificName = paste("Species", rep(1:10, 2)),
        xcoord = rep(c(11.25, 4.67), each = 10),
        ycoord = rep(c(58.28, 52.58), each = 10),
        resolution = "4"
    )

    mock_cube <- list(
        data = mock_data,
        grid_type = "isea3h",
        first_year = 2020,
        last_year = 2021,
        num_species = 10,
        species_names = paste("Species", 1:10),
        coord_range = c(xmin = 3, xmax = 13, ymin = 51, ymax = 60),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    testthat::local_mocked_bindings(
        my_DataInfo = mock_DataInfo,
        get_ne_data = mock_get_ne_data,
        .package = "b3gbi"
    )
    result <- suppressWarnings(suppressMessages(
        completeness_map(mock_cube)
    ))
    expect_s3_class(result, "indicator_map")
    expect_equal(result$grid_type, "isea3h")
    expect_true("diversity_val" %in% names(result$data))
})

test_that("indicator_ts for completeness works with isea3h", {
    skip_if_not(
        requireNamespace("dggridR", quietly = TRUE),
        "dggridR not available for safe ISEA3H grid creation"
    )

    mock_data <- tibble::tibble(
        cellCode = rep(c("-458282525011250000", "452583359004665569"), each = 10),
        year = rep(2020:2021, 10),
        obs = rpois(20, 5) + 1,
        taxonKey = rep(1:10, 2),
        scientificName = paste("Species", rep(1:10, 2)),
        xcoord = rep(c(11.25, 4.67), each = 10),
        ycoord = rep(c(58.28, 52.58), each = 10),
        resolution = "4"
    )

    mock_cube <- list(
        data = mock_data,
        grid_type = "isea3h",
        first_year = 2020,
        last_year = 2021,
        num_species = 10,
        species_names = paste("Species", 1:10),
        coord_range = c(xmin = 3, xmax = 13, ymin = 51, ymax = 60),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    testthat::local_mocked_bindings(
        my_DataInfo = mock_DataInfo,
        get_ne_data = mock_get_ne_data,
        .package = "b3gbi"
    )
    result <- suppressWarnings(suppressMessages(
        completeness_ts(mock_cube, ci_type = "none")
    ))
    expect_s3_class(result, "indicator_ts")
    expect_equal(nrow(result$data), 2)
})
