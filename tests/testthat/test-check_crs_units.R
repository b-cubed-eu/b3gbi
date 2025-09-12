test_that("check_crs_units handles valid EPSG codes", {
  expect_equal(
    check_crs_units(
      4326
    ), "degrees"
  )
  expect_equal(
    check_crs_units(
      25832
    ), "km"
  )
})

test_that("check_crs_units handles valid PROJ.4 strings", {
  expect_equal(
    check_crs_units(
      "+proj=longlat +datum=WGS84 +no_defs"
    ), "degrees"
  )
  expect_equal(
    check_crs_units(
      "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
    ), "km"
  )
})

test_that("check_crs_units handles valid WKT strings", {
  expect_equal(
    check_crs_units(
      paste0(
        "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,",
        "298.257223563]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",",
        "0.0174532925199433]]"
      )
    ), "degrees"
  )
  expect_equal(
    check_crs_units(
      paste0(
        "PROJCS[\"ETRS89 / UTM zone 32N\",GEOGCS[\"ETRS89\",DATUM",
        "[\"European_Terrestrial_Reference_System_1989\",",
        "SPHEROID[\"GRS 1980\",6378137,298.257222101]],",
        "PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433,",
        "AUTHORITY[\"EPSG\",\"9122\"]]],PROJECTION[\"Transverse_Mercator\"],",
        "PARAMETER[\"latitude_of_origin\",0],",
        "PARAMETER[\"central_meridian\",9],PARAMETER[\"scale_factor\",0.9996],",
        "PARAMETER[\"false_easting\",500000],PARAMETER[\"false_northing\",0],",
        "UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],",
        "AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"
      )
    ), "km"
  )
})

test_that("check_crs_units handles invalid CRS inputs", {
  expect_error(
    check_crs_units(
      "not a crs"
    ),
    "invalid crs"
  )
  expect_warning(
    expect_error(
      check_crs_units(
        99999  # Invalid EPSG code
      ), "Invalid CRS input."
    )
  )
})

test_that("check_crs_units handles invalid units", {
  expect_error(
    check_crs_units(
      paste0(
        "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 ",
        "+datum=NAD83 +units=us-ft +no_defs"
      )
    )
  )
  expect_error(
    check_crs_units(
      paste0(
        "+proj=somerc +lat_0=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 ",
        "+units=km +no_defs"
      )
    )
  )
})
