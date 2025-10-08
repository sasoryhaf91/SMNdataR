test_that("smn_int_get_station validates input", {
  expect_error(smn_int_get_station(NA))
  expect_error(smn_int_get_station())
})

test_that("smn_int_get_station returns empty data.frame for unknown id", {
  out <- smn_int_get_station("00000")
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("smn_int_get_station finds a known station (smoke test)", {
  # Load dataset for test scope (same as function does)
  utils::data("stations", package = "SMNdataR", envir = environment())
  skip_if_not(exists("stations", inherits = FALSE))

  # take any one id from the dataset
  sid <- as.character(stations$station[1])
  out <- smn_int_get_station(sid)

  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 1L)
  expect_true("station" %in% names(out))
  expect_identical(as.character(out$station), sid)
})
