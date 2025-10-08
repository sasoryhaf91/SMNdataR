# Tests for smn_dl_daily_single()

# Helper: a small synthetic raw dataframe factory
.fake_raw <- function(dates,
                      prec = NA_real_,
                      evap = NA_real_,
                      tmax = NA_real_,
                      tmin = NA_real_) {
  data.frame(
    date = as.Date(dates),
    prec = rep_len(prec, length(dates)),
    evap = rep_len(evap, length(dates)),
    tmax = rep_len(tmax, length(dates)),
    tmin = rep_len(tmin, length(dates)),
    stringsAsFactors = FALSE
  )
}

test_that("returns full data with coordinates (happy path)", {
  raw <- .fake_raw(
    dates = c("2020-01-01","2020-01-02"),
    prec = c(0, 5),
    evap = c(1, NA),
    tmax = c(25, 26),
    tmin = c(10, 11)
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) {
      data.frame(latitude = 19.5, longitude = -99.1, altitude = 2240)
    },
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101",
                             start_date = "2020-01-01",
                             end_date   = "2020-01-31",
                             output_format = "full")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("station","latitude","longitude","altitude",
                    "date","prec","evap","tmax","tmin") %in% names(out)))
  expect_equal(out$station[1], "15101")
  expect_equal(out$latitude[1], 19.5)
  expect_equal(out$longitude[1], -99.1)
  expect_equal(out$altitude[1], 2240)
  expect_equal(out$prec, c(0,5))
})

test_that("reduced format returns only expected columns", {
  raw <- .fake_raw(dates = c("2020-02-01","2020-02-02"), prec = c(1, 2),
                   evap = 3, tmax = 25, tmin = 12)

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) {
      data.frame(latitude = 1, longitude = 2, altitude = 3)
    },
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101",
                             start_date = "2020-01-01",
                             end_date   = "2020-12-31",
                             output_format = "reduced")
  expect_identical(names(out), c("date","prec","evap","tmax","tmin"))
  expect_equal(nrow(out), 2L)
})

test_that("del_na = 'yes' drops rows with any NA in returned columns", {
  raw <- .fake_raw(
    dates = c("2020-03-01","2020-03-02","2020-03-03"),
    prec  = c(0, NA, 2),
    evap  = c(1,  2, NA),
    tmax  = c(25, 26, 27),
    tmin  = c(10, NA, 12)
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) {
      data.frame(latitude = 10, longitude = 20, altitude = 30)
    },
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101",
                             start_date = "2020-03-01",
                             end_date   = "2020-03-31",
                             output_format = "full",
                             del_na = "yes")
  # Only the first row has no NA in any of the value columns
  expect_equal(nrow(out), 1L)
  expect_equal(out$date, as.Date("2020-03-01"))
})

test_that("empty after date filter returns correct empty schema", {
  raw <- .fake_raw(dates = c("2019-12-30","2019-12-31"), prec = 0, evap = 1, tmax = 20, tmin = 10)

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) {
      data.frame(latitude = 1, longitude = 2, altitude = 3)
    },
    .package = "SMNdataR"
  )

  out_full <- smn_dl_daily_single("15101",
                                  start_date = "2020-01-01",
                                  end_date   = "2020-01-31",
                                  output_format = "full")
  expect_equal(nrow(out_full), 0L)
  expect_true(all(c("station","latitude","longitude","altitude",
                    "date","prec","evap","tmax","tmin") %in% names(out_full)))

  out_red <- smn_dl_daily_single("15101",
                                 start_date = "2020-01-01",
                                 end_date   = "2020-01-31",
                                 output_format = "reduced")
  expect_equal(nrow(out_red), 0L)
  expect_identical(names(out_red), c("date","prec","evap","tmax","tmin"))
})

test_that("raw download failure returns empty (no error)", {
  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) stop("boom"),
    smn_int_extract_coordinates = function(st, ...) {  # won't be called
      data.frame(latitude = 0, longitude = 0, altitude = 0)
    },
    .package = "SMNdataR"
  )
  expect_warning(
    out <- smn_dl_daily_single("15101", start_date = "2020-01-01", end_date = "2020-12-31"),
    regexp = "Failed to download raw data"
  )
  expect_equal(nrow(out), 0L)
})

test_that("coordinate failure is tolerated (coords become NA)", {
  raw <- .fake_raw(dates = c("2020-04-01","2020-04-02"), prec = c(1,2), evap = 3, tmax = 25, tmin = 10)

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) stop("coords boom"),
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101", start_date = "2020-04-01", end_date = "2020-04-30")
  expect_true(all(is.na(out$latitude)))
  expect_true(all(is.na(out$longitude)))
  expect_true(all(is.na(out$altitude)))
  expect_equal(nrow(out), 2L)
})

test_that("missing variables in raw are filled with NA in output", {
  # Raw without 'evap'
  raw <- data.frame(
    date = as.Date(c("2020-05-01","2020-05-02")),
    prec = c(0, 1),
    tmax = c(25, 26),
    tmin = c(10, 11),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) data.frame(latitude=1, longitude=2, altitude=3),
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101", start_date = "2020-05-01", end_date = "2020-05-31")
  expect_true(all(is.na(out$evap)))
  expect_equal(nrow(out), 2L)
})

test_that("date validation errors are raised", {
  expect_error(
    smn_dl_daily_single("15101", start_date = "not-a-date", end_date = "2020-01-01"),
    regexp = "coercible to Date"
  )
  expect_error(
    smn_dl_daily_single("15101", start_date = "2020-02-01", end_date = "2020-01-01"),
    regexp = "on or after"
  )
})

test_that("output is ordered by date ascending", {
  raw <- .fake_raw(dates = c("2020-01-03","2020-01-01","2020-01-02"), prec = c(3,1,2),
                   evap = 0, tmax = 25, tmin = 10)

  testthat::local_mocked_bindings(
    smn_dl_daily_raw = function(st) raw,
    smn_int_extract_coordinates = function(st, ...) data.frame(latitude=1, longitude=2, altitude=3),
    .package = "SMNdataR"
  )

  out <- smn_dl_daily_single("15101", start_date = "2020-01-01", end_date = "2020-01-31")
  expect_equal(out$date, as.Date(c("2020-01-01","2020-01-02","2020-01-03")))
  expect_equal(out$prec, c(1,2,3))
})
