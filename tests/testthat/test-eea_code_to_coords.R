# EEA reference grid codes: easting/northing are divided by 10^n, where n is
# the number of trailing zeros of the cell size in metres (EEA/INSPIRE rule).
# Examples for 100 km to 100 m are the GBIF_EEARGCode() examples in the GBIF
# SQL download documentation; 250 m is the example in the EEA grid documentation.

test_that("EEA codes are converted with the 10^n rule at all standard sizes", {
  codes <- c(
    "100kmE51N29",        # x 10^5
    "50kmE510N290",       # x 10^4
    "10kmE510N293",       # x 10^4
    "5kmE5100N2930",      # x 10^3
    "2kmE5104N2932",      # x 10^3
    "1kmE5105N2933",      # x 10^3
    "250mE1025N22000",    # x 10^1
    "100mE51052N29336",   # x 10^2
    "25mE5105200N2933600"  # x 10^0 (25 has no trailing zeros)
  )
  res <- eea_code_to_coords(codes)
  expect_equal(res$xcoord, c(5100000, 5100000, 5100000, 5100000, 5104000,
                             5105000, 10250, 5105200, 5105200))
  expect_equal(res$ycoord, c(2900000, 2900000, 2930000, 2930000, 2932000,
                             2933000, 220000, 2933600, 2933600))
  expect_equal(res$resolution, c("100km", "50km", "10km", "5km", "2km", "1km",
                                 "250m", "100m", "25m"))
})

test_that("the same location gives consistent coordinates across sizes", {
  # A point at (5,105,234; 2,933,678) falls in these cells
  res <- eea_code_to_coords(c("10kmE510N293", "1kmE5105N2933",
                              "100mE51052N29336"))
  expect_true(all(res$xcoord <= 5105234 & res$xcoord > 5105234 - 10000))
  expect_true(all(res$ycoord <= 2933678 & res$ycoord > 2933678 - 10000))
})

test_that("non-standard codes already in metres are handled with a warning", {
  expect_warning(
    res <- eea_code_to_coords("10kmE4321000N3210000"),
    "do not follow the EEA naming rule"
  )
  expect_equal(res$xcoord, 4321000)
  expect_equal(res$ycoord, 3210000)
})

test_that("eea_trailing_zeros counts zeros of cell sizes in metres", {
  expect_equal(eea_trailing_zeros(c(25, 100, 250, 1000, 2000, 5000, 10000,
                                    50000, 1e5, NA)),
               c(0L, 2L, 1L, 3L, 3L, 3L, 4L, 4L, 5L, 0L))
})
