test_that("extracts for multiple stations and fills NA on failure", {
  testthat::local_mocked_bindings(
    smn_int_extract_coordinates = function(st, ...) {
      if (st == "15022") stop("boom")
      data.frame(latitude = 20, longitude = -100, altitude = 1500)
    },
    .package = "SMNdataR"
  )

  expect_warning(
    res <- smn_info_extract_coordinates(c("15021","15022"), show_progress = FALSE),
    regexp = "Failed to extract data for station 15022"
  )

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 2L)
  expect_equal(res$latitude[1], 20)
  expect_true(all(is.na(res[2, c("latitude","longitude","altitude")])))
})

