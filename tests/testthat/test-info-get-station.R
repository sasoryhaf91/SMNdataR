test_that("vector, single, and data.frame inputs work", {
  utils::data("stations", package = "SMNdataR", envir = environment())
  sid <- as.character(stations$station[1])
  df  <- smn_info_get_station(sid)
  expect_identical(nrow(df), 1L)

  sids <- as.character(stations$station[1:3])
  df2  <- smn_info_get_station(sids)
  expect_identical(nrow(df2), 3L)

  df3  <- smn_info_get_station(data.frame(CLAVE = sids))
  expect_identical(nrow(df3), 3L)
})

test_that("empty/unknown ids return empty data.frame with correct schema", {
  out <- smn_info_get_station(character())
  utils::data("stations", package = "SMNdataR", envir = environment())
  expect_identical(names(out), names(stations))
  expect_identical(nrow(out), 0L)

  out2 <- smn_info_get_station("00000")
  expect_identical(nrow(out2), 0L)
})

test_that("errors on data.frame without id column", {
  expect_error(smn_info_get_station(data.frame(x = 1:3)))
})
