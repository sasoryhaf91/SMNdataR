# tests/testthat/test-dl-yearly-single.R
testthat::test_that("LONG: all variables, metadata and missing-fraction QC", {
  # --- synthetic daily generator (two years) ---------------------------------
  make_daily <- function(station = "15101") {
    d1 <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
    d2 <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    date <- c(d1, d2)

    # base signals
    prec <- rep(1, length(date))
    evap <- rep(2, length(date))
    tmax <- rep(20, length(date))
    tmin <- rep(10, length(date))

    # introduce many NAs in 2001 for 'prec' to exceed max_missing_frac
    idx_2001 <- which(format(date, "%Y") == "2001")
    prec[idx_2001][seq(1, length(idx_2001), by = 2)] <- NA_real_  # ~50% NA

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

  # --- mock smn_dl_daily_single() from SMNdataR ------------------------------
  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  # --- run yearly (default aggregator: sum for prec/evap, mean for tmax/tmin)
  out <- SMNdataR::smn_dl_yearly_single(
    station = "15101",
    start_date = "2000-01-01",
    end_date   = "2001-12-31",
    variables = "all",
    max_missing_frac = 0.2,       # 2001-prec should be NA
    output_format = "long"
  )

  # shape: 2 años * 4 variables
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 8L)
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","variable","value") %in% names(out)))

  # metadatos repetidos
  testthat::expect_true(all(out$station   == "15101"))
  testthat::expect_true(all(out$latitude  == 19.5))
  testthat::expect_true(all(out$longitude == -99.1))
  testthat::expect_true(all(out$altitude  == 2240))

  # valores esperados:
  # 2000-prec = suma (366 días en 2000) -> 366
  v_2000_prec <- out$value[out$YEAR == 2000 & out$variable == "prec"]
  testthat::expect_equal(as.numeric(v_2000_prec), 366)

  # 2001-prec NA por exceder max_missing_frac
  v_2001_prec <- out$value[out$YEAR == 2001 & out$variable == "prec"]
  testthat::expect_true(is.na(v_2001_prec))

  # 2000-tmax = mean 20
  v_2000_tmax <- out$value[out$YEAR == 2000 & out$variable == "tmax"]
  testthat::expect_equal(as.numeric(v_2000_tmax), 20)

  # 2001-evap = suma 2 por día (365 días) -> 730
  v_2001_evap <- out$value[out$YEAR == 2001 & out$variable == "evap"]
  testthat::expect_equal(as.numeric(v_2001_evap), 730)
})

testthat::test_that("REDUCE: subset of variables + named-list aggregator", {
  # synthetic
  date <- seq(as.Date("2005-01-01"), as.Date("2006-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101",
    latitude  = 19.5,
    longitude = -99.1,
    altitude  = 2240,
    date      = date,
    prec      = rep(1, length(date)),
    evap      = rep(2, length(date)),
    tmax      = rep(20, length(date)),
    tmin      = rep(10, length(date))
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station = "15101",
    start_date = "2005-01-01",
    end_date   = "2006-12-31",
    variables = c("prec","tmax"),
    aggregator = list(prec = sum, tmax = median),  # diferentes funciones
    output_format = "reduce"
  )

  # columnas esperadas en reduce
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","tmax") %in% names(out)))
  testthat::expect_equal(nrow(out), 2L)

  # 2005: prec = suma (365); tmax = mediana (20)
  r2005 <- out[out$YEAR == 2005, , drop = FALSE]
  testthat::expect_equal(as.numeric(r2005$prec), 365)
  testthat::expect_equal(as.numeric(r2005$tmax), 20)

  # 2006: prec = 365 (no bisiesto); tmax = 20
  r2006 <- out[out$YEAR == 2006, , drop = FALSE]
  testthat::expect_equal(as.numeric(r2006$prec), 365)
  testthat::expect_equal(as.numeric(r2006$tmax), 20)
})

testthat::test_that("Single aggregator function applied to all variables", {
  # 1 año simple
  date <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101", latitude = 19.5, longitude = -99.1, altitude = 2240,
    date = date, prec = 1, evap = 2, tmax = 20, tmin = 10
  )

  testthat::local_mocked_bindings(
    smn_dl_daily_single = function(station, start_date, end_date,
                                   output_format = "full", del_na = "no") syn,
    .package = "SMNdataR"
  )

  out <- SMNdataR::smn_dl_yearly_single(
    station = "15101",
    start_date = "2012-01-01",
    end_date   = "2012-12-31",
    variables = "all",
    aggregator = median,
    output_format = "long"
  )

  # Debe haber 4 filas (1 año * 4 variables), medianas: 1,2,20,10
  testthat::expect_equal(nrow(out), 4L)
  vals <- setNames(out$value, out$variable)
  testthat::expect_equal(as.numeric(vals["prec"]), 1)
  testthat::expect_equal(as.numeric(vals["evap"]), 2)
  testthat::expect_equal(as.numeric(vals["tmax"]), 20)
  testthat::expect_equal(as.numeric(vals["tmin"]), 10)
})

testthat::test_that("CSV writing works and schema is stable", {
  date <- seq(as.Date("2010-01-01"), as.Date("2011-12-31"), by = "day")
  syn <- data.frame(
    station   = "15101", latitude = 19.5, longitude = -99.1, altitude = 2240,
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
    station = "15101",
    start_date = "2010-01-01",
    end_date   = "2011-12-31",
    variables  = c("prec","evap"),
    output_format = "reduce",
    csv_file = tmp
  )

  testthat::expect_true(file.exists(tmp))
  testthat::expect_true(all(c("station","latitude","longitude","altitude",
                              "YEAR","prec","evap") %in% names(out)))
})
