# tests/testthat/test-dl-yearly-single.R

testthat::test_that("reduce: all vars, metadata present and missing-fraction QC works", {
  # --- dos años sintéticos (2000, 2001) ---
  make_daily <- function(station = "15101") {
    d1 <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
    d2 <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    date <- c(d1, d2)

    prec <- rep(1, length(date))
    evap <- rep(2, length(date))
    tmax <- rep(20, length(date))
    tmin <- rep(10, length(date))

    # ~50% NA en 2001 para prec, para superar max_missing_frac = 0.2
    idx_2001 <- which(format(date, "%Y") == "2001")
    prec[idx_2001][seq(1, length(idx_2001), by = 2)] <- NA_real_

    data.frame(
      station   = station,
      latitude  = 19.5,
      longitude = -99.1,
      altitude  = 2240,
      date, prec, evap, tmax, tmin,
      check.names = FALSE
    )
  }

  syn <- make_daily()

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station = "15101",
    start_date = "2000-01-01",
    end_date   = "2001-12-31",
    variables  = "all",
    max_missing_frac = 0.2
  )

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","evap","tmax","tmin") %in% names(out)))
  testthat::expect_equal(nrow(out), 2L)

  # Metadatos constantes
  testthat::expect_true(all(out$station   == "15101"))
  testthat::expect_true(all(out$latitude  == 19.5))
  testthat::expect_true(all(out$longitude == -99.1))
  testthat::expect_true(all(out$altitude  == 2240))

  # 2000: prec suma = 366
  r2000 <- out[out$YEAR == 2000, , drop = FALSE]
  testthat::expect_equal(as.numeric(r2000$prec), 366)
  testthat::expect_equal(as.numeric(r2000$evap), 2 * 366)
  testthat::expect_equal(as.numeric(r2000$tmax), 20)
  testthat::expect_equal(as.numeric(r2000$tmin), 10)

  # 2001: prec debe ser NA por exceso de NA diarios; evap = 730
  r2001 <- out[out$YEAR == 2001, , drop = FALSE]
  testthat::expect_true(is.na(r2001$prec))
  testthat::expect_equal(as.numeric(r2001$evap), 2 * 365)
})

testthat::test_that("reduce: subset de variables y aggregator como lista nombrada", {
  date <- seq(as.Date("2005-01-01"), as.Date("2006-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101",
    latitude  = 19.5,
    longitude = -99.1,
    altitude  = 2240,
    date      = date,
    prec      = 1,
    evap      = 2,
    tmax      = 20,
    tmin      = 10
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station   = "15101",
    start_date = "2005-01-01",
    end_date   = "2006-12-31",
    variables  = c("prec","tmax"),
    aggregator = list(prec = sum, tmax = median)
  )

  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","tmax") %in% names(out)))
  testthat::expect_equal(nrow(out), 2L)

  r2005 <- out[out$YEAR == 2005, , drop = FALSE]
  testthat::expect_equal(as.numeric(r2005$prec), 365)
  testthat::expect_equal(as.numeric(r2005$tmax), 20)

  r2006 <- out[out$YEAR == 2006, , drop = FALSE]
  testthat::expect_equal(as.numeric(r2006$prec), 365)
  testthat::expect_equal(as.numeric(r2006$tmax), 20)
})

testthat::test_that("reduce: aggregator único aplicado a todas las variables", {
  date <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101",
    latitude  = 19.5,
    longitude = -99.1,
    altitude  = 2240,
    date = date, prec = 1, evap = 2, tmax = 20, tmin = 10
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station   = "15101",
    start_date = "2012-01-01",
    end_date   = "2012-12-31",
    variables  = "all",
    aggregator = median
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(as.numeric(out$prec), 1)
  testthat::expect_equal(as.numeric(out$evap), 2)
  testthat::expect_equal(as.numeric(out$tmax), 20)
  testthat::expect_equal(as.numeric(out$tmin), 10)
})

testthat::test_that("reduce: CSV writing y esquema estable", {
  date <- seq(as.Date("2010-01-01"), as.Date("2011-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101",
    latitude  = 19.5,
    longitude = -99.1,
    altitude  = 2240,
    date = date, prec = 1, evap = 2, tmax = 20, tmin = 10
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  out <- SMNdataR::smn_dl_yearly_single(
    station   = "15101",
    start_date = "2010-01-01",
    end_date   = "2011-12-31",
    variables  = c("prec","evap"),
    csv_file   = tmp
  )

  testthat::expect_true(file.exists(tmp))
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","evap") %in% names(out)))
})

testthat::test_that("reduce: sin datos devuelve esquema vacío correcto", {
  # mock que devuelve vacío
  empty_daily <- data.frame()
  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") empty_daily,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station   = "15101",
    start_date = "1990-01-01",
    end_date   = "1990-12-31",
    variables  = c("prec","tmax")
  )

  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","tmax") %in% names(out)))
})

testthat::test_that("reduce: si falta una variable en el diario, la columna existe y es NA", {
  # diario sin 'evap'
  date <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101", latitude = 19.5, longitude = -99.1, altitude = 2240,
    date = date, prec = 1, tmax = 20, tmin = 10
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station   = "15101",
    start_date = "2015-01-01",
    end_date   = "2015-12-31",
    variables  = c("prec","evap","tmax")
  )

  testthat::expect_true(all(c("prec","evap","tmax") %in% names(out)))
  testthat::expect_true(all(is.na(out$evap)))
})
