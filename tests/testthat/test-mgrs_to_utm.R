test_that("mgrs_to_utm matches reference conversions (mgrs package, GeoTrans)", {
  # Reference values were generated once with mgrs::mgrs_to_utm() for random
  # points worldwide at all precisions (100 km to 1 m), plus Norway/Svalbard
  # special zones and codes at the equator.
  ref <- utils::read.csv(
    test_path("fixtures", "mgrs_reference.csv"),
    colClasses = c(mgrs = "character", hemisphere = "character")
  )
  res <- mgrs_to_utm(ref$mgrs)
  expect_equal(res$zone, as.integer(ref$zone))
  expect_equal(res$hemisphere, ref$hemisphere)
  expect_equal(res$easting, ref$easting)
  expect_equal(res$northing, ref$northing)
})

test_that("mgrs_to_utm returns the south-west corner of known squares", {
  res <- mgrs_to_utm(c("32UNG1234", "31UDQ4825511932", "4QFJ12345678", "33UUB"))
  expect_equal(res$zone, c(32L, 31L, 4L, 33L))
  expect_equal(res$hemisphere, c("N", "N", "N", "N"))
  expect_equal(res$easting, c(512000, 448255, 612340, 300000))
  expect_equal(res$northing, c(6134000, 5411932, 2356780, 6100000))
})

test_that("mgrs_to_utm accepts padded zones, lower case and spaces", {
  res <- mgrs_to_utm(c("04QFJ12345678", "32ung 1234"))
  expect_equal(res$easting, c(612340, 512000))
  expect_equal(res$northing, c(2356780, 6134000))
})

test_that("mgrs_to_utm returns NA with one warning for invalid codes", {
  expect_warning(
    res <- mgrs_to_utm(c("32UNG1234", "abc", "32VNM5", "31IAA", "60CAA")),
    "4 of 5 MGRS code"
  )
  expect_equal(is.na(res$easting), c(FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(res$mgrs, c("32UNG1234", "abc", "32VNM5", "31IAA", "60CAA"))
})

test_that("mgrs_to_utm handles empty and NA input", {
  expect_equal(nrow(mgrs_to_utm(character(0))), 0)
  expect_no_warning(res <- mgrs_to_utm(NA_character_))
  expect_true(is.na(res$northing))
})
