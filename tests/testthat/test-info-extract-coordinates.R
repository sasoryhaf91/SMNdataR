test_that("validates input and returns empty for no stations", {
  expect_error(smn_info_extract_coordinates(character(0)))
})

test_that("extracts for multiple stations and fills NA on failure", {
  # mock: primer id ok, segundo falla
  ok_lines <- c("Latitud: 20.0", "Longitud: -100.0", "Altitud: 1500 m")

  testthat::local_mocked_bindings(
    smn_int_get_url = function(st) paste0("mock://", st),
    smn_int_handle_error = function(expr, ...) ok_lines,
    .package = "SMNdataR"
  )
  res1 <- smn_info_extract_coordinates(c("15021","15022"), show_progress = FALSE)
  expect_s3_class(res1, "data.frame")
  expect_true(all(c("station","latitude","longitude","altitude") %in% names(res1)))
  expect_identical(nrow(res1), 2L)
  expect_equal(res1$latitude[1], 20)
  expect_true(is.numeric(res1$altitude[1]))

  # ahora forzamos fallo en la 2ª
  testthat::local_mocked_bindings(
    smn_int_extract_coordinates = function(st, ...) {
      if (st == "15022") stop("boom")
      data.frame(latitude=20, longitude=-100, altitude=1500)
    },
    .package = "SMNdataR"
  )
  res2 <- smn_info_extract_coordinates(c("15021","15022"), show_progress = FALSE)
  expect_true(is.na(res2$latitude[2]))
  expect_true(is.na(res2$longitude[2]))
  expect_true(is.na(res2$altitude[2]))
})

test_that("propagates add_meta_cols via ...", {
  testthat::local_mocked_bindings(
    smn_int_extract_coordinates = function(st, add_meta_cols = FALSE, ...) {
      df <- data.frame(latitude=1, longitude=2, altitude=3)
      if (isTRUE(add_meta_cols)) {
        df$station <- st; df$source_url <- paste0("mock://", st)
      }
      df
    },
    .package = "SMNdataR"
  )
  res <- smn_info_extract_coordinates(c("A","B"), show_progress = FALSE, add_meta_cols = TRUE)
  expect_true(all(c("station","source_url") %in% names(res)))
})
