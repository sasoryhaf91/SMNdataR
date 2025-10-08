test_that("retries then succeeds", {
  f <- local({ i <- 0; function() { i <<- i + 1; if (i < 3) stop("boom"); i } })
  x <- smn_int_handle_error(f(), max_attempts = 5, wait_sec = 0, quiet = TRUE)
  expect_identical(x, 3)
})

test_that("warnings can trigger retry when requested", {
  f <- local({ i <- 0; function() { i <<- i + 1; if (i < 2) warning("transient"); 42 } })
  x <- smn_int_handle_error(f(), max_attempts = 3, wait_sec = 0,
                            retry_on_warning = TRUE, quiet = TRUE)
  expect_identical(x, 42)
})

