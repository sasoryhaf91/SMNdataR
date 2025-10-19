# tests/testthat/test-dl-daily-batch-hybrid.R

testthat::test_that("HYBRID: fusiona SMN + NASA por fecha y respeta el esquema/orden", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:1

  smn_ok <- function(station, start_date, end_date, output_format, del_na) {
    data.frame(
      station   = "S1",
      latitude  = 19.5,
      longitude = -99.1,
      altitude  = 2240,
      date = dates,
      prec = c(0, 5),
      evap = c(2.1, 2.0),
      tmax = c(24, 25),
      tmin = c(10, 11),
      stringsAsFactors = FALSE
    )
  }

  nasa_ok <- function(station, lat, lon, altitude, start_date, end_date,
                      vars, community, retries, sleep_sec, output_format) {
    data.frame(
      station   = "S1",
      latitude  = 19.5,
      longitude = -99.1,
      altitude  = 2240,
      date = dates,
      T2M_MIN = c(9.5, 10.4),
      T2M_MAX = c(24.6, 25.2),
      PRECTOTCORR = c(0.0, 5.1),
      stringsAsFactors = FALSE
    )
  }

  expected_cols <- c("station","latitude","longitude","altitude","date",
                     "prec","evap","tmax","tmin","T2M_MIN","T2M_MAX","PRECTOTCORR")

  testthat::with_mocked_bindings(
    {
      out <- smn_dl_daily_batch(
        stations = "S1",
        source = "hybrid",
        start_date = "2020-01-01",
        end_date   = "2020-01-02",
        vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
        .progress = FALSE
      )

      testthat::expect_s3_class(out, "data.frame")
      testthat::expect_true(all(expected_cols %in% names(out)))
      testthat::expect_equal(names(out)[seq_along(expected_cols)], expected_cols)

      testthat::expect_equal(nrow(out), 2L)
      testthat::expect_true(isTRUE(all.equal(out$date, sort(out$date))))

      testthat::expect_equal(sum(is.na(out$T2M_MIN)), 0L)
      testthat::expect_equal(sum(is.na(out$T2M_MAX)), 0L)
      testthat::expect_equal(sum(is.na(out$PRECTOTCORR)), 0L)

      testthat::expect_equal(sum(is.na(out$prec)), 0L)
      testthat::expect_equal(sum(is.na(out$tmax)), 0L)
    },
    smn_dl_daily_single = smn_ok,
    smn_dl_daily_nasa   = nasa_ok
  )
})

testthat::test_that("HYBRID: cuando SMN está vacío devuelve data.frame VACÍO con esquema estable", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:1

  smn_empty <- function(station, start_date, end_date, output_format, del_na) {
    data.frame()
  }

  nasa_ok <- function(station, lat, lon, altitude, start_date, end_date,
                      vars, community, retries, sleep_sec, output_format) {
    data.frame(
      station   = "S3",
      latitude  = 21.0,
      longitude = -100.0,
      altitude  = 2000,
      date = dates,
      T2M_MIN = c(10.5, 11.2),
      T2M_MAX = c(24.3, 25.1),
      PRECTOTCORR = c(0.0, 1.8),
      stringsAsFactors = FALSE
    )
  }

  expected_cols <- c("station","latitude","longitude","altitude","date",
                     "prec","evap","tmax","tmin","T2M_MIN","T2M_MAX","PRECTOTCORR")

  testthat::with_mocked_bindings(
    {
      out <- smn_dl_daily_batch(
        stations = "S3",
        source = "hybrid",
        start_date = "2020-01-01",
        end_date   = "2020-01-02",
        vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
        .progress = FALSE
      )

      testthat::expect_s3_class(out, "data.frame")
      testthat::expect_equal(nrow(out), 0L)
      testthat::expect_true(all(expected_cols %in% names(out)))
      testthat::expect_equal(names(out)[seq_along(expected_cols)], expected_cols)
    },
    smn_dl_daily_single = smn_empty,
    smn_dl_daily_nasa   = nasa_ok
  )
})

testthat::test_that("HYBRID: respeta return_list=TRUE (una entrada por estación)", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:1

  smn_ok <- function(station, start_date, end_date, output_format, del_na) {
    data.frame(
      station   = station,
      latitude  = 20,
      longitude = -100,
      altitude  = 2100,
      date = dates,
      prec = c(0, 1),
      evap = c(2, 2),
      tmax = c(25, 26),
      tmin = c(10, 11),
      stringsAsFactors = FALSE
    )
  }

  nasa_ok <- function(station, lat, lon, altitude, start_date, end_date,
                      vars, community, retries, sleep_sec, output_format) {
    data.frame(
      station   = station,
      latitude  = 20,
      longitude = -100,
      altitude  = 2100,
      date = dates,
      T2M_MIN = c(9.9, 10.0),
      T2M_MAX = c(25.5, 26.1),
      PRECTOTCORR = c(0.2, 0.0),
      stringsAsFactors = FALSE
    )
  }

  testthat::with_mocked_bindings(
    {
      out_list <- smn_dl_daily_batch(
        stations = c("A1","B2"),
        source = "hybrid",
        start_date = "2020-01-01",
        end_date   = "2020-01-02",
        vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
        return_list = TRUE,
        .progress = FALSE
      )

      testthat::expect_type(out_list, "list")
      testthat::expect_equal(length(out_list), 2L)
      testthat::expect_true(all(vapply(out_list, function(x) inherits(x,"data.frame"), logical(1))))
      testthat::expect_true(all(vapply(out_list, nrow, integer(1)) == 2L))
    },
    smn_dl_daily_single = smn_ok,
    smn_dl_daily_nasa   = nasa_ok
  )
})

testthat::test_that("HYBRID: columnas base siempre; NASA solo las pedidas. Las no pedidas si aparecen deben ser NA", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:1

  smn_ok <- function(station, start_date, end_date, output_format, del_na) {
    data.frame(
      station   = "S4",
      latitude  = 22,
      longitude = -101,
      altitude  = 1900,
      date = dates,
      prec = c(0, 0),
      evap = c(1.8, 1.7),
      tmax = c(26, 27),
      tmin = c(12, 13),
      stringsAsFactors = FALSE
    )
  }

  nasa_ok <- function(station, lat, lon, altitude, start_date, end_date,
                      vars, community, retries, sleep_sec, output_format) {
    # Solo devuelve T2M_MIN (como si hubiéramos pedido únicamente esa)
    data.frame(
      station   = "S4",
      latitude  = 22,
      longitude = -101,
      altitude  = 1900,
      date = dates,
      T2M_MIN = c(11.5, 12.0),
      stringsAsFactors = FALSE
    )
  }

  base_cols <- c("station","latitude","longitude","altitude","date",
                 "prec","evap","tmax","tmin")
  requested_nasa <- c("T2M_MIN")
  possible_extra <- c("T2M_MAX","PRECTOTCORR")  # podrían no existir

  testthat::with_mocked_bindings(
    {
      out <- smn_dl_daily_batch(
        stations = "S4",
        source = "hybrid",
        start_date = "2020-01-01",
        end_date   = "2020-01-02",
        vars = requested_nasa,  # solo pedimos T2M_MIN
        .progress = FALSE
      )

      # Siempre deben estar las base + las NASA solicitadas
      testthat::expect_true(all(base_cols %in% names(out)))
      testthat::expect_true(all(requested_nasa %in% names(out)))

      # Si aparecen columnas NASA NO solicitadas, deben ser todo NA
      for (extra in possible_extra) {
        if (extra %in% names(out)) {
          testthat::expect_true(all(is.na(out[[extra]])))
        }
      }

      # La solicitada debe tener datos
      testthat::expect_equal(sum(is.na(out$T2M_MIN)), 0L)
    },
    smn_dl_daily_single = smn_ok,
    smn_dl_daily_nasa   = nasa_ok
  )
})

