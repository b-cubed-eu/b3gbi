test_that("union_helper dissolves all features into one", {
  sq <- function(x, y) {
    sf::st_polygon(list(rbind(c(x, y), c(x + 1, y), c(x + 1, y + 1),
                              c(x, y + 1), c(x, y))))
  }
  land <- sf::st_sf(geometry = sf::st_sfc(sq(0, 0), sq(2, 0), sq(4, 0),
                                          crs = 3035))
  ocean <- sf::st_sf(geometry = sf::st_sfc(sq(1, 0), sq(3, 0), crs = 3035))
  res <- union_helper(land, ocean)
  # Not the pairwise union (3 x 2 = 6 overlapping features)
  expect_equal(nrow(res), 1)
  expect_equal(as.numeric(sf::st_area(res)), 5)
})
