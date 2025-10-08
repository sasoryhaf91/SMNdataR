test_that("smn_int_handle_error succeeds for simple expr", {
  x <- smn_int_handle_error({ 2 + 2 }, max_attempts = 1, wait_sec = 0, quiet = TRUE,
                            envir = environment())
  expect_identical(x, 4)
})

test_that("retries then succeeds", {
  # closure con estado interno (evita depender de <<- fuera del entorno)
  f <- local({ i <- 0; function() { i <<- i + 1; if (i < 3) stop("boom"); i } })

  x <- smn_int_handle_error(
    { f() },
    max_attempts = 5, wait_sec = 0, quiet = TRUE,
    envir = environment()  # <- clave: el entorno donde vive f
  )
  expect_identical(x, 3)
})

test_that("warnings can trigger retry when requested", {
  f <- local({ i <- 0; function() { i <<- i + 1; if (i < 2) warning("transient"); 42 } })

  x <- smn_int_handle_error(
    { f() },
    max_attempts = 3, wait_sec = 0, retry_on_warning = TRUE, quiet = TRUE,
    envir = environment()
  )
  expect_identical(x, 42)
})

test_that("predicate is_ok forces retry/failure when FALSE", {
  expect_error(
    smn_int_handle_error(
      { 1 },
      max_attempts = 2, wait_sec = 0,
      is_ok = function(v) v > 1,
      envir = environment(), quiet = TRUE
    )
  )
})
