#' Handle Errors with Retries (internal, robust)
#'
#' Evaluates an expression and, if an error occurs, retries execution up to a
#' maximum number of attempts. Between attempts, it sleeps using an exponential
#' backoff (with optional jitter). You may treat warnings as retryable and/or
#' validate the returned value with a predicate function.
#'
#' @param expr An expression to evaluate (non-standard evaluation).
#' @param max_attempts Integer. Maximum number of attempts (default `10`).
#' @param wait_sec Numeric. Initial wait (seconds) before the **next** attempt
#'   (default `1`). If `max_attempts == 1`, no waiting is performed.
#' @param backoff Numeric `>= 1`. Exponential backoff multiplier (default `1.5`).
#' @param jitter Numeric in `[0, 1]`. Random fractional jitter applied to the
#'   computed wait (default `0.2`).
#' @param retry_on_warning Logical. If `TRUE`, warnings are treated as retryable
#'   (default `FALSE`).
#' @param is_ok Optional function taking the result and returning `TRUE` if it is
#'   acceptable; if `FALSE`, the call is retried (default `NULL`).
#' @param envir Environment in which to evaluate `expr`. Defaults to
#'   `parent.frame(2)` so it usually matches the caller's scope (handy in tests).
#' @param quiet Logical. If `TRUE`, suppress progress messages (default `FALSE`).
#'
#' @return The result of the evaluated expression if successful. If all attempts
#'   fail, an error is thrown with the last error message.
#'
#' @examples
#' # Simple success:
#' smn_int_handle_error({ 2 + 2 })
#'
#' # Retries then succeeds:
#' f <- local({ i <- 0; function() { i <<- i + 1; if (i < 3) stop("boom"); i } })
#' smn_int_handle_error(f(), max_attempts = 5, wait_sec = 0, quiet = TRUE)
#'
#' # Validate result:
#' smn_int_handle_error({ 1 }, is_ok = function(x) x > 0, wait_sec = 0)
#'
#' @keywords internal
#' @noRd
smn_int_handle_error <- function(expr,
                                 max_attempts     = 10,
                                 wait_sec         = 1,
                                 backoff          = 1.5,
                                 jitter           = 0.2,
                                 retry_on_warning = FALSE,
                                 is_ok            = NULL,
                                 envir            = parent.frame(2),
                                 quiet            = FALSE) {
  if (!is.numeric(max_attempts) || max_attempts < 1) stop("`max_attempts` must be >= 1.")
  if (!is.numeric(wait_sec)   || wait_sec   < 0)     stop("`wait_sec` must be >= 0.")
  if (!is.numeric(backoff)    || backoff    < 1)     stop("`backoff` must be >= 1.")
  if (!is.numeric(jitter)     || jitter < 0 || jitter > 1) stop("`jitter` must be in [0,1].")
  if (!is.null(is_ok) && !is.function(is_ok)) stop("`is_ok` must be a function or NULL.")
  if (!is.environment(envir)) stop("`envir` must be an environment.")

  expr_quo <- substitute(expr)
  last_err <- NULL

  for (attempt in seq_len(max_attempts)) {
    warn_msg <- NULL
    res <- withCallingHandlers(
      tryCatch(
        eval(expr_quo, envir = envir),
        error = function(e) { last_err <<- e; NULL }
      ),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        if (retry_on_warning) invokeRestart("muffleWarning")
      }
    )

    ok <- !is.null(res)
    if (ok && !is.null(is_ok)) {
      ok <- isTRUE(is_ok(res))
      if (!ok) last_err <- simpleError("Result validation failed (`is_ok` returned FALSE).")
    }
    if (ok && (!retry_on_warning || is.null(warn_msg))) return(res)

    if (!quiet) {
      msg <- if (!is.null(last_err)) {
        sprintf("Attempt %d/%d failed: %s", attempt, max_attempts, conditionMessage(last_err))
      } else if (!is.null(warn_msg) && retry_on_warning) {
        sprintf("Attempt %d/%d warning (retrying): %s", attempt, max_attempts, warn_msg)
      } else {
        sprintf("Attempt %d/%d did not meet success criteria.", attempt, max_attempts)
      }
      message(msg)
    }

    if (attempt < max_attempts) {
      wait <- wait_sec * (backoff^(attempt - 1))
      if (jitter > 0) wait <- wait * (1 + stats::runif(1, -jitter, jitter))
      if (wait > 0) Sys.sleep(wait)
    }
  }

  stop(if (!is.null(last_err))
    paste0("Maximum attempts exhausted. Last error: ", conditionMessage(last_err))
    else
      "Maximum attempts exhausted.")
}

