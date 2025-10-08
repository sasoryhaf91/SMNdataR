# Tests for smn_int_extract_coordinates (internal)

test_that("returns NA row for invalid station id", {
  # Un id inválido hace que smn_int_get_url() falle; la función debe capturarlo
  expect_warning(
    out <- smn_int_extract_coordinates("00000"),
    regexp = "Invalid station id|Insufficient metadata|Could not read metadata",
    all = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_true(all(c("latitude","longitude","altitude") %in% names(out)))
  expect_identical(nrow(out), 1L)
  expect_true(all(is.na(out[1, c("latitude","longitude","altitude")])))
})

test_that("extracts coords and applies hemisphere hints (Sur/Oeste)", {
  # Mock metadata content (no red)
  mock_lines <- c(
    "Nombre: Estación Demo",
    "Latitud: 19,50 Sur",      # Sur -> negativo; coma decimal
    "Longitud: 99.10 Oeste",   # Oeste -> negativo; punto decimal
    "Altitud: 2300 m"
  )

  testthat::local_mocked_bindings(
    # No importa la URL, pero la necesitamos para el atributo/columna
    smn_int_get_url = function(station) sprintf("mock://station/%s", station),
    # Forzamos al manejador de reintentos a devolver nuestras líneas simuladas
    smn_int_handle_error = function(expr, ...) mock_lines,
    .package = "SMNdataR"
  )

  res <- smn_int_extract_coordinates("15101")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$latitude,  -19.50, tolerance = 1e-6)
  expect_equal(res$longitude, -99.10, tolerance = 1e-6)
  expect_equal(res$altitude,  2300)

  # Atributos de procedencia
  expect_false(is.null(attr(res, "station")))
  expect_false(is.null(attr(res, "source_url")))
})

test_that("add_meta_cols=TRUE appends provenance columns", {
  mock_lines <- c(
    "Latitud: 20.0 Norte",
    "Longitud: 100.0 Este",
    "Altitud: 1600 m"
  )

  testthat::local_mocked_bindings(
    smn_int_get_url = function(station) sprintf("mock://station/%s", station),
    smn_int_handle_error = function(expr, ...) mock_lines,
    .package = "SMNdataR"
  )

  res <- smn_int_extract_coordinates("15101", add_meta_cols = TRUE)
  expect_true(all(c("latitude","longitude","altitude","station","source_url") %in% names(res)))
  expect_identical(res$station, "15101")
  expect_match(res$source_url, "^mock://station/")
})

test_that("out-of-range values are set to NA with a warning", {
  mock_lines <- c(
    "Latitud: 45.0",
    "Longitud: 200.0",   # fuera de rango
    "Altitud: 10 m"
  )

  testthat::local_mocked_bindings(
    smn_int_get_url = function(station) sprintf("mock://station/%s", station),
    smn_int_handle_error = function(expr, ...) mock_lines,
    .package = "SMNdataR"
  )

  expect_warning(
    res <- smn_int_extract_coordinates("15101"),
    regexp = "Out-of-range coordinate"
  )
  expect_true(is.na(res$longitude))
  expect_equal(res$latitude, 45)
  expect_equal(res$altitude, 10)
})

