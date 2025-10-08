test_that("returns NA for no numeric token or NA input", {
  expect_equal(smn_int_extract_numeric("abc"), NA_real_)
  expect_equal(smn_int_extract_numeric(NA_character_), NA_real_)
})

test_that("extracts basic numbers with optional sign", {
  expect_identical(smn_int_extract_numeric("x 10 y"), 10)
  expect_identical(smn_int_extract_numeric("z -2.5"), -2.5)
  expect_identical(smn_int_extract_numeric("+3.14 rad"), 3.14)
})

test_that("extracts scientific notation", {
  expect_identical(smn_int_extract_numeric("val=-1.2e3"), -1200)
  expect_identical(smn_int_extract_numeric("1E2 units"), 100)
})

test_that("handles comma as decimal mark and thousands separators", {
  expect_equal(smn_int_extract_numeric("Temp: -2,3 °C"), -2.3)
  expect_equal(smn_int_extract_numeric("Value: 1,234.56 m"), 1234.56)
  expect_equal(smn_int_extract_numeric("Value: 1.234,56 m"), 1234.56)
  expect_equal(smn_int_extract_numeric("Pop 1,234,567"), 1234567)
  expect_equal(smn_int_extract_numeric("Pop 1.234.567"), 1234567)
})

test_that("is vectorized and returns one value per element", {
  x <- c("a", "b 10", "c -1.2e1", "d 3,5", "e 1.234,00")
  out <- smn_int_extract_numeric(x)
  expect_length(out, length(x))
  expect_equal(out, c(NA_real_, 10, -12, 3.5, 1234.00))
})

test_that("takes the first number when multiple are present", {
  expect_identical(smn_int_extract_numeric("x 5 y 7"), 5)
  expect_identical(smn_int_extract_numeric("(-3) and 2"), -3)
})
