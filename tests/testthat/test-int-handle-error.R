test_that("smn_int_handle_error succeeds for simple expr", {
  x <- smn_int_handle_error({ 2 + 2 }, max_attempts = 1)
  expect_identical(x, 4)
})

test_that("retries then succeeds", {
  i <- 0
  x <- smn_int_handle_error({
    i <<- i + 1
    if (i < 3) stop("boom")
    i
  }, max_attempts = 5, wait_sec = 0, quiet = TRUE)
  expect_identical(x, 3)
})

test_that("validates with is_ok", {
  expect_error(
    smn_int_handle_error({ 1 }, max_attempts = 2, wait_sec = 0,
                         is_ok = function(v) v > 1, quiet = TRUE)
  )
})

test_that("warnings can trigger retry when requested", {
  i <- 0
  x <- smn_int_handle_error({
    i <<- i + 1
    if (i < 2) warning("transient"); 42
  }, max_attempts = 3, wait_sec = 0, retry_on_warning = TRUE, quiet = TRUE)
  expect_identical(x, 42)
})
