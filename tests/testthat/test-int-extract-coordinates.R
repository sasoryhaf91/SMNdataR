test_that("extracts coords and applies hemisphere hints (Sur/Oeste)", {
  mock_lines <- c(
    "Nombre: Estación Demo",
    "Latitud: 19.5 Sur",      # <-- usar punto, no coma
    "Longitud: 99.10 Oeste",
    "Altitud: 2300 m"
  )
  testthat::local_mocked_bindings(
    smn_int_get_url = function(station) sprintf("mock://station/%s", station),
    smn_int_handle_error = function(expr, ...) mock_lines,
    .package = "SMNdataR"
  )
  res <- smn_int_extract_coordinates("15101", add_meta_cols = FALSE)
  expect_equal(res$latitude,  -19.5, tolerance = 1e-6)
  expect_equal(res$longitude, -99.10, tolerance = 1e-6)
  expect_equal(res$altitude,  2300)
})
