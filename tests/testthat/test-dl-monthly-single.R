# Tests for smn_dl_monthly_single()

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

test_that("long format returns tidy table with metadata for 'all' variables", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-29"), by = "day")
  # prec=1, evap=2, tmax=20, tmin=10  (constantes => sum/mean conocidas)
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101", variables = "all",
                               start_date = "2020-01-01", end_date = "2020-12-31",
                               output_format = "long")

  expect_s3_class(out, "data.frame")
  expect_true(all(c("station","latitude","longitude","altitude",
                    "variable","YEAR","MONTH","value") %in% names(out)))
  # Deben existir 4 variables * 2 meses = 8 filas
  expect_equal(nrow(out), 8L)

  # Comprobaciones puntuales:
  # Enero 2020: 31 días -> prec=31, evap=62, tmax=20, tmin=10
  jan <- subset(out, YEAR == 2020 & MONTH == 1)
  expect_equal(setNames(jan$value, jan$variable)[c("prec","evap","tmax","tmin")],
               c(31, 62, 20, 10))
  # Febrero 2020 (bisiesto): 29 días -> prec=29, evap=58
  feb <- subset(out, YEAR == 2020 & MONTH == 2)
  expect_equal(setNames(feb$value, feb$variable)[c("prec","evap")],
               c(29, 58))
})

test_that("missing fraction threshold sets monthly value to NA", {
  dates <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day")
  raw <- .fake_daily_full(dates)
  # introduce NAs en 'prec' para superar 10% faltantes (p.ej., 5/29 > 0.1)
  na_idx <- 1:5
  raw$prec[na_idx] <- NA_real_

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101", variables = "prec",
                               start_date = "2020-02-01", end_date = "2020-02-29",
                               max_missing_frac = 0.10, output_format = "long")
  expect_true(all(is.na(out$value)))
})

test_that("reduce format (single variable) returns wide with JAN..DEC and summaries", {
  dates <- c(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"),
             seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day"))
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  wide <- smn_dl_monthly_single("15101", variables = "prec", output_format = "reduce")
  expect_true(all(c("YEAR","JAN","FEB","MAR","APR","MAY","JUN",
                    "JUL","AUG","SEP","OCT","NOV","DEC","ACUM","PROM","MONTHS") %in% names(wide)))
  expect_equal(nrow(wide), 1L)
  expect_equal(wide$JAN, 31)  # suma de enero (prec=1/día)
  expect_equal(wide$FEB, 29)
  expect_equal(wide$MONTHS, 2L)
  # ACUM para variable de suma = 31 + 29
  expect_equal(wide$ACUM, 60)
  # PROM = promedio de meses válidos = (31 + 29)/2
  expect_equal(wide$PROM, 30)
})

test_that("reduce format with multiple variables returns a named list", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101", variables = c("prec","tmax"),
                               output_format = "reduce")
  expect_type(out, "list")
  expect_true(all(c("prec","tmax") %in% names(out)))
  expect_s3_class(out$prec, "data.frame")
  expect_s3_class(out$tmax, "data.frame")
})

test_that("custom aggregator (single function) is applied", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  # tmax varía para comprobar mediana
  raw <- .fake_daily_full(dates, tmax = c(rep(10, 15), rep(30, 16)))

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101", variables = "tmax",
                               output_format = "long", aggregator = median)
  # Mediana de 15 "10" y 16 "30" = 30
  expect_equal(out$value, 30)
})

test_that("custom aggregator named list per variable works", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates, prec = 1:31, tmax = 1:31)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  out <- smn_dl_monthly_single("15101",
                               variables = c("prec","tmax"),
                               output_format = "long",
                               aggregator = list(prec = max, tmax = min))
  # Enero: max(1..31)=31 para prec; min(1..31)=1 para tmax
  vals <- setNames(out$value, out$variable)
  expect_equal(vals["prec"], 31)
  expect_equal(vals["tmax"], 1)
})

test_that("writes CSV for long and reduce (multi-var) outputs", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  raw <- .fake_daily_full(dates)

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  # long: un solo archivo
  f_long <- file.path(tempdir(), "monthly_long.csv")
  on.exit(unlink(f_long, force = TRUE), add = TRUE)
  res_long <- smn_dl_monthly_single("15101", variables = "all",
                                    output_format = "long", csv_file = f_long)
  expect_true(file.exists(f_long))
  expect_gt(nrow(res_long), 0)

  # reduce multi-var: un archivo por variable
  f_base <- file.path(tempdir(), "monthly_reduce.csv")
  on.exit(unlink(list.files(tempdir(), pattern = "monthly_reduce", full.names = TRUE), force = TRUE), add = TRUE)
  res_red <- smn_dl_monthly_single("15101", variables = c("prec","evap"),
                                   output_format = "reduce", csv_file = f_base)
  expect_true(is.list(res_red) && length(res_red) == 2L)
  files <- list.files(tempdir(), pattern = "monthly_reduce_.*\\.csv$", full.names = TRUE)
  expect_true(length(files) >= 2L)
})

test_that("errors when requesting variables not present in daily data", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  raw <- .fake_daily_full(dates)
  # elimina 'evap' para provocar error
  raw$evap <- NULL

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date, output_format = "full", del_na = "no") raw,
    .package = "SMNdataR"
  )

  expect_error(
    smn_dl_monthly_single("15101", variables = c("prec","evap"), output_format = "long"),
    regexp = "not present"
  )
})
