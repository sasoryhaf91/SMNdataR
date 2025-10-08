test_that("returns NA for no numeric token or NA input", {
  expect_equal(smn_int_extract_numeric("abc"), NA_real_)
  expect_equal(smn_int_extract_numeric(NA_character_), NA_real_)
})

test_that("extracts integers and signed decimals", {
  expect_identical(smn_int_extract_numeric("x 10 y"), 10)
  expect_identical(smn_int_extract_numeric("z -2.5"), -2.5)
  expect_identical(smn_int_extract_numeric("+3.14 rad"), 3.14)
  expect_identical(smn_int_extract_numeric(".5 deg"), 0.5)
})

test_that("is vectorized and returns one value per element", {
  x <- c("a", "b 10", "c -2.5", "d 3.0", "e .25")
  out <- smn_int_extract_numeric(x)
  expect_length(out, length(x))
  expect_equal(out, c(NA_real_, 10, -2.5, 3.0, 0.25))
})

test_that("takes the first number when multiple are present", {
  expect_identical(smn_int_extract_numeric("x 5 y 7"), 5)
  expect_identical(smn_int_extract_numeric("(-3) and 2"), -3)
})
