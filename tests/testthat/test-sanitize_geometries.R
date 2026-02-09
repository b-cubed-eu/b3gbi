# --- Mock Data Setup ---

# 1. Null object
null_object <- NULL

# 2. Empty sf object
empty_sf <- sf::st_sf(geometry = sf::st_sfc())

# 3. Simple sfc object (needs conversion to sf)
sfc_input <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

# 4. Point geometry (should become POLYGON/MULTIPOLYGON via buffer)
sf_point <- sf::st_sf(geom = sf::st_sfc(sf::st_point(c(1, 1))), data.frame(id = 1), crs = 4326)

# 5. MultiPoint geometry
sf_multipoint <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE))),
                           data.frame(id = 1), crs = 4326)

# 6. LineString geometry (should become POLYGON via st_cast)
sf_linestring <- sf::st_sf(geom = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
                           data.frame(id = 1), crs = 4326)

# 7. GeometryCollection (must be cast to MULTIPOLYGON)
sf_poly1 <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE))))
sf_pt1 <- sf::st_sfc(sf::st_point(c(2, 2)))
sf_geom_collection <- sf::st_sf(geom = sf::st_sfc(sf::st_geometrycollection(c(sf_poly1, sf_pt1)), crs = 4326), data.frame(id = 1))

# 8. Polygon geometry (needs final cast to MULTIPOLYGON)
sf_polygon <- sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(matrix(c(10,10, 20,10, 20,20, 10,20, 10,10), ncol=2, byrow=TRUE)))),
                        data.frame(id = 1), crs = 4326)

# 9. MultiPolygon geometry (the desired final output state)
sf_multipolygon <- sf::st_sf(geom = sf::st_sfc(sf::st_multipolygon(list(list(matrix(c(30,30, 40,30, 40,40, 30,40, 30,30), ncol=2, byrow=TRUE))))),
                             data.frame(id = 1), crs = 4326)


# --- Test Block 1: Edge Cases (Null, Empty, sfc) ---
test_that("Handles null, empty, and sfc input correctly", {

  # 1. Null Check (Activates the first IF branch)
  expect_null(sanitize_geometries(null_object))

  # 2. sfc to sf Conversion (Activates the !inherits(sf_object, "sf") branch)
  result_sfc <- suppressWarnings(
    sanitize_geometries(sfc_input)
  )
  expect_true(inherits(result_sfc, "sf"))

  # 3. Empty Check (Activates the nrow(sf_object) == 0 branch)
  expect_equal(nrow(sanitize_geometries(empty_sf)), 0)
  expect_true(inherits(sanitize_geometries(empty_sf), "sf"))
})

# --- Test Block 2: Point Geometries (Point and MultiPoint) ---
test_that("Point geometries are correctly converted to POLYGONs via buffer", {

  # 1. POINT (Activates the geom_type == "POINT" branch)
  result_point <- suppressWarnings(
    sanitize_geometries(sf_point)
  )
  # Transform to a projected CRS (e.g., 3857 Pseudo-Mercator) before area calc
  result_projected <- sf::st_transform(result_point, 3857)
  expect_equal(as.character(sf::st_geometry_type(
    result_point, by_geometry = FALSE
  )), "MULTIPOLYGON")
  expect_true(all(as.numeric(sf::st_area(result_projected)) > 0)) # Check buffer successfully created area

  # 2. MULTIPOINT (Activates the geom_type == "MULTIPOINT" branch)
  result_multipoint <- suppressWarnings(sanitize_geometries(sf_multipoint))
  expect_equal(as.character(sf::st_geometry_type(
    result_multipoint, by_geometry = FALSE
  )), "MULTIPOLYGON")
})

# --- Test Block 3: Line Geometries and Collections ---
test_that("Line and Collection geometries are cast to POLYGONs", {

  # 1. LINESTRING (Activates the geom_type == "LINESTRING" branch)
  result_linestring <- sanitize_geometries(sf_linestring)
  # The final check will ensure it's MULTIPOLYGON
  expect_equal(as.character(sf::st_geometry_type(result_linestring, by_geometry = FALSE)), "MULTIPOLYGON")

  # 2. GEOMETRYCOLLECTION (Activates the geom_type == "GEOMETRYCOLLECTION" branch)
  result_collection <- sanitize_geometries(sf_geom_collection)
  expect_equal(as.character(sf::st_geometry_type(result_collection, by_geometry = FALSE)), "MULTIPOLYGON")
})


# --- Test Block 4: Final Standardization Check ---
test_that("Polygon input is correctly standardized to MULTIPOLYGON", {

  # 1. POLYGON (Activates the final if-check for !="MULTIPOLYGON" and st_cast)
  result_polygon <- sanitize_geometries(sf_polygon)
  expect_equal(as.character(sf::st_geometry_type(result_polygon, by_geometry = FALSE)), "MULTIPOLYGON")

  # 2. MULTIPOLYGON (Should pass through without casting/error)
  result_multipolygon <- sanitize_geometries(sf_multipolygon)
  expect_equal(as.character(sf::st_geometry_type(result_multipolygon, by_geometry = FALSE)), "MULTIPOLYGON")
  expect_identical(result_multipolygon, sf_multipolygon)
})
