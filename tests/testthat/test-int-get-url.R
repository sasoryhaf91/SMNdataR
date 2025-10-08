test_that("smn_int_get_url validates input", {
  expect_error(smn_int_get_url(""))
  expect_error(smn_int_get_url("abc"))
  expect_error(smn_int_get_url("123"))   # too short
})

test_that("smn_int_get_url builds expected paths", {
  u <- smn_int_get_url("15101")  # state id "15" -> mex, prefix "dia"
  expect_true(grepl("/mex/dia15101\\.txt$", u))

  v <- smn_int_get_url("9123")   # state id "9" -> df, prefix "dia0"
  expect_true(grepl("/df/dia09123\\.txt$", v))
})

test_that("smn_int_get_url errors on unknown state ids", {
  expect_error(smn_int_get_url("99101")) # "99" not mapped
})
