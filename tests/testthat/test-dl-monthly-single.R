# Tests for smn_dl_monthly_single() - LONG format (wide-by-variable)

# --- helper: synthetic daily "full" with metadata ----------------------------
.fake_daily_full <- function(dates,
                             prec = 1,
                             evap = 2,
                             tmax = 20,
                             tmin = 10,
                             station = "15101",
                             lat = 19.5, lon = -99.1, alt = 2240) {
  n <- length(dates)
  data.frame(
    station   = rep(station, n),
    latitude  = rep(lat, n),
    longitude = rep(lon, n),
    altitude  = rep(alt, n),
    date      = as.Date(dates),
    prec      = rep_len(prec, n),
    evap      = rep_len(evap, n),
    tmax      = rep_len(tmax, n),
    tmin      = rep_len(tmin, n),
    stringsAsFactors = FALSE
  )
}

test_that("long format with ALL variables returns metadata + date + requested vars", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-29"), by = "day") # incluye bisiesto
  raw <- .fake_daily_full(dates) # prec=1, evap=2, tmax=20, tmin=10

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101",
                               variables = "all",
                               start_date = "2020-01-01",
                               end_date   = "2020-12-31",
                               output_format = "long")

  expect_s3_class(out, "data.frame")
  expect_true(all(c("station","latitude","longitude","altitude","date",
                    "prec","evap","tmin","tmax") %in% names(out)))
  expect_equal(nrow(out), 2L)                 # Enero y Febrero 2020
  expect_equal(out$station[1], "15101")
  expect_equal(out$latitude[1],  19.5)
  expect_equal(out$longitude[1], -99.1)
  expect_equal(out$altitude[1],  2240)

  # Enero: 31 días -> prec=31, evap=62, tmax=20, tmin=10
  jan <- out[out$date == as.Date("2020-01-01"), ]
  expect_equal(jan$prec, 31)
  expect_equal(jan$evap, 62)
  expect_equal(jan$tmax, 20)
  expect_equal(jan$tmin, 10)

  # Febrero 2020: 29 días -> prec=29, evap=58
  feb <- out[out$date == as.Date("2020-02-01"), ]
  expect_equal(feb$prec, 29)
  expect_equal(feb$evap, 58)
})

test_that("long format with a subset of variables returns only those columns", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101",
                               variables = c("prec","tmax"),
                               output_format = "long")
  expect_identical(
    names(out),
    c("station","latitude","longitude","altitude","date","prec","tmax")
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$prec, 31)
  expect_equal(out$tmax, 20)
})

test_that("long format with a single variable returns that var only", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101",
                               variables = "evap",
                               output_format = "long")
  expect_identical(
    names(out),
    c("station","latitude","longitude","altitude","date","evap")
  )
  expect_equal(out$evap, 31 * 2)   # 2 por día en enero
})

test_that("missing fraction threshold sets monthly value to NA in long format", {
  dates <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day")
  raw <- .fake_daily_full(dates)
  # Provoca >10% faltantes en 'prec' (p.ej., 5/29 ~ 17.2%)
  raw$prec[1:5] <- NA_real_

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101", variables = "prec",
                               start_date = "2020-02-01", end_date = "2020-02-29",
                               max_missing_frac = 0.10, output_format = "long")
  expect_true(is.na(out$prec))
})

test_that("writes CSV in long format", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  f <- file.path(tempdir(), "monthly_long_new.csv")
  on.exit(unlink(f, force = TRUE), add = TRUE)

  out <- smn_dl_monthly_single("15101",
                               variables = c("prec","evap","tmin","tmax"),
                               output_format = "long",
                               csv_file = f)
  expect_true(file.exists(f))
  expect_equal(nrow(out), 1L)
})

test_that("errors when requesting variables not present in daily data (long)", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  raw <- .fake_daily_full(dates)
  raw$evap <- NULL   # quita 'evap' para provocar error

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  expect_error(
    smn_dl_monthly_single("15101", variables = c("prec","evap"), output_format = "long"),
    regexp = "not present"
  )
})
