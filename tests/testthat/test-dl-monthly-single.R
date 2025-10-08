# Tests for reworked smn_dl_monthly_single()

# ---- helper: synthetic daily "full" with metadata ---------------------------
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

test_that("LONG: all variables -> metadata + date + prec/evap/tmin/tmax (natural order)", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-29"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out <- smn_dl_monthly_single("15101",
                               variables = "all",
                               start_date = "2020-01-01",
                               end_date   = "2020-12-31",
                               output_format = "long")

  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("station","latitude","longitude","altitude","date","prec","evap","tmin","tmax")
  )
  expect_equal(nrow(out), 2L)

  jan <- out[out$date == as.Date("2020-01-01"), ]
  expect_equal(jan$prec, 31)
  expect_equal(jan$evap, 62)
  expect_equal(jan$tmax, 20)
  expect_equal(jan$tmin, 10)

  feb <- out[out$date == as.Date("2020-02-01"), ]
  expect_equal(feb$prec, 29)
  expect_equal(feb$evap, 58)
})

test_that("LONG: subset -> solo vars pedidas, orden natural y metadata + date", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out <- smn_dl_monthly_single("15101",
                               variables = c("tmax","prec"),
                               output_format = "long")

  expect_identical(
    names(out),
    c("station","latitude","longitude","altitude","date","prec","tmax")
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$prec, 31)
  expect_equal(out$tmax, 20)
})

test_that("LONG: single variable -> solo esa variable + metadata/date", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out <- smn_dl_monthly_single("15101",
                               variables = "evap",
                               output_format = "long")
  expect_identical(
    names(out),
    c("station","latitude","longitude","altitude","date","evap")
  )
  expect_equal(out$evap, 62)
})

test_that("LONG: max_missing_frac pone NA cuando se excede el umbral", {
  dates <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day")
  raw <- .fake_daily_full(dates)
  raw$prec[1:5] <- NA_real_ # 5/29 > 0.10

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out <- smn_dl_monthly_single("15101",
                               variables = "prec",
                               start_date = "2020-02-01", end_date = "2020-02-29",
                               max_missing_frac = 0.10,
                               output_format = "long")
  expect_true(is.na(out$prec))
})

test_that("LONG: aggregator único y lista nombrada funcionan", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates, prec = 1:31, tmax = 1:31)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out1 <- smn_dl_monthly_single("15101",
                                variables = c("prec","tmax"),
                                output_format = "long",
                                aggregator = median)
  expect_equal(out1$prec, median(1:31))
  expect_equal(out1$tmax, median(1:31))

  out2 <- smn_dl_monthly_single("15101",
                                variables = c("prec","tmax"),
                                output_format = "long",
                                aggregator = list(prec = max, tmax = min))
  expect_equal(out2$prec, 31)
  expect_equal(out2$tmax, 1)
})

test_that("LONG: escribe CSV", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  f <- file.path(tempdir(), "monthly_long_out.csv")
  on.exit(unlink(f, force = TRUE), add = TRUE)

  out <- smn_dl_monthly_single("15101",
                               variables = c("prec","evap","tmin","tmax"),
                               output_format = "long",
                               csv_file = f)
  expect_true(file.exists(f))
  expect_equal(nrow(out), 1L)
})

test_that("LONG: error si se pide variable ausente en daily", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  raw <- .fake_daily_full(dates); raw$evap <- NULL

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  expect_error(
    smn_dl_monthly_single("15101", variables = c("prec","evap"), output_format = "long"),
    regexp = "not present"
  )
})

test_that("REDUCE: una variable -> wide con JAN..DEC y resúmenes", {
  dates <- c(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"),
             seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day"))
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  wide <- smn_dl_monthly_single("15101", variables = "prec", output_format = "reduce")
  expect_true(all(c("YEAR","JAN","FEB","MAR","APR","MAY","JUN",
                    "JUL","AUG","SEP","OCT","NOV","DEC","ACUM","PROM","MONTHS") %in% names(wide)))
  expect_equal(nrow(wide), 1L)
  expect_equal(wide$JAN, 31)
  expect_equal(wide$FEB, 29)
  expect_equal(wide$ACUM, 60)
  expect_equal(wide$PROM, 30)
  expect_equal(wide$MONTHS, 2L)
})

test_that("REDUCE: varias variables -> lista nombrada de data.frames", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  out <- smn_dl_monthly_single("15101",
                               variables = c("prec","tmax"),
                               output_format = "reduce")
  expect_type(out, "list")
  expect_true(all(c("prec","tmax") %in% names(out)))
  expect_s3_class(out$prec, "data.frame")
  expect_s3_class(out$tmax, "data.frame")
})

test_that("REDUCE: escribe un archivo por variable cuando son múltiples", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .env = asNamespace("SMNdataR")
  )

  base <- file.path(tempdir(), "monthly_reduce_out.csv")
  on.exit(unlink(list.files(tempdir(), pattern = "monthly_reduce_out", full.names = TRUE), force = TRUE), add = TRUE)

  res <- smn_dl_monthly_single("15101",
                               variables = c("prec","evap"),
                               output_format = "reduce",
                               csv_file = base)
  expect_true(is.list(res) && length(res) == 2L)
  files <- list.files(tempdir(), pattern = "monthly_reduce_out_.*\\.csv$", full.names = TRUE)
  expect_true(length(files) >= 2L)
})

test_that("daily_del_na se propaga a smn_dl_daily_single()", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  raw <- .fake_daily_full(dates)
  called_del_na <- NULL

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") {
      called_del_na <<- del_na
      raw
    },
    .env = asNamespace("SMNdataR")
  )

  invisible(smn_dl_monthly_single("15101",
                                  variables = "prec",
                                  daily_del_na = "yes",
                                  output_format = "long"))
  expect_identical(called_del_na, "yes")
})

