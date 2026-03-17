library(testthat)
library(b3gbi)
library(sf)
library(dplyr)
library(readr)

test_that("process_cube handles ISEA3H grid", {
    # Simple valid ISEA3H Mocnik ID
    cube_df <- tibble::tibble(
        year = 2020,
        cellCode = "105000000010000000",
        occurrences = 10,
        scientificName = "Species A",
        speciesKey = 123
    )

    result <- suppressWarnings(process_cube(cube_df, grid_type = "isea3h"))
    expect_s3_class(result, "processed_cube")
    expect_true("xcoord" %in% names(result$data))
    expect_equal(result$grid_type, "isea3h")
})

test_that("process_cube handles case-insensitive column matching", {
    cube_df <- tibble::tibble(
        year = 2020,
        cellCode = "E32N20",
        occurrences = 5,
        scientificName = "Species B",
        speciesKey = 456,
        Kingdom = "Plantae"
    )

    result <- process_cube(cube_df, grid_type = "eea")
    expect_true("kingdom" %in% names(result$data))
})

test_that("process_cube handles optional column mappings", {
    cube_df <- tibble::tibble(
        year = 2020,
        cellCode = "1kmE3200N20000",
        obs_count = 5,
        sci_name = "Species C",
        key = 789,
        k_name = "Animalia",
        f_name = "Felidae",
        k_key = 1,
        f_key = 10,
        sex_val = "M",
        stage = "Adult"
    )

    result <- suppressWarnings(process_cube(cube_df,
        grid_type = "eea",
        cols_occurrences = "obs_count",
        cols_scientificName = "sci_name",
        cols_speciesKey = "key",
        cols_kingdom = "k_name",
        cols_family = "f_name",
        cols_kingdomKey = "k_key",
        cols_familyKey = "f_key",
        cols_sex = "sex_val",
        cols_lifeStage = "stage"
    ))

    expect_equal(result$data$obs[1], 5)
    expect_equal(result$data$scientificName[1], "Species C")
    expect_equal(result$data$kingdom[1], "Animalia")
    expect_equal(result$data$sex[1], "M")
})

test_that("process_cube handles custom separator", {
    temp_file <- tempfile(fileext = ".csv")
    writeLines("year;cellCode;occurrences;scientificName;speciesKey\n2020;1kmE32N20;10;Species D;999", temp_file)

    result <- suppressWarnings(process_cube(temp_file, separator = ";"))
    expect_equal(nrow(result$data), 1)
    expect_equal(result$data$obs[1], 10)
    unlink(temp_file)
})

test_that("process_cube_old handles legacy data", {
    occ_file <- tempfile(fileext = "_occ.csv")
    tax_file <- tempfile(fileext = "_tax.csv")

    # legacy data logic: first_year = min(year), last_year = max(year) - 1
    readr::write_csv(tibble::tibble(
        year = c(2010, 2011, 2012),
        eea_cell_code = c("1kmE32N20", "1kmE32N20", "1kmE32N20"),
        n = c(5, 6, 7),
        min_coord_uncertainty = c(100, 100, 100),
        speciesKey = c(101, 101, 101)
    ), occ_file)

    readr::write_csv(tibble::tibble(
        speciesKey = 101,
        scientificName = "Legacy Species",
        rank = "SPECIES",
        taxonomicStatus = "ACCEPTED",
        kingdom = "Animalia",
        includes = NA_character_
    ), tax_file)

    result <- b3gbi:::process_cube_old(occ_file, tax_info = tax_file)

    expect_s3_class(result, "processed_cube")
    expect_equal(result$grid_type, "eea")
    expect_true(nrow(result$data) > 0)

    unlink(c(occ_file, tax_file))
})
