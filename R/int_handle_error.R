#' Handle Errors with Retries (internal, robust)
#'
#' Evaluates an expression and, if an error occurs, retries execution up to a
#' maximum number of attempts. Between attempts, it sleeps using an exponential
#' backoff (with optional jitter). You can also choose to treat warnings as
#' retryable and/or validate the returned value with a predicate.
#'
#'
#' @param expr An expression to evaluate (non-standard evaluation).
#' @param max_attempts Integer. Maximum number of attempts (default `10`).
#' @param wait_sec Numeric. Initial wait in seconds before the **next** attempt
#'   (default `1`). If `max_attempts == 1`, no waiting is performed.
#' @param backoff Numeric \eqn{>= 1}. Multiplier for exponential backoff
#'   (default `1.5`), e.g., waits: `wait_sec * backoff^(attempt-1)`.
#' @param jitter Numeric in `[0, 1]`. Random fractional jitter applied to the
#'   computed wait to avoid thundering herds (default `0.2`).
#' @param retry_on_warning Logical. If `TRUE`, warnings are treated as
#'   retryable (default `FALSE`).
#' @param is_ok Optional function taking the **result** and returning `TRUE`
#'   if it is acceptable; if it returns `FALSE`, the call is retried
#'   (default `NULL`, i.e., accept any non-error result).
#' @param quiet Logical. If `TRUE`, suppress progress messages (default `FALSE`).
#'
#' @return The result of the evaluated expression if successful (and passing
#'   `is_ok`, when provided). If all attempts fail, it stops with the last error
#'   message (or a generic message if unavailable).
#'
#' @examples
#' # 1) Simple success
#' smn_int_handle_error({ 1 + 1 })
#'
#' # 2) Transient failure with retries (toy example)
#' i <- 0
#' smn_int_handle_error({
#'   i <<- i + 1
#'   if (i < 3) stop("try again")
#'   i
#' }, max_attempts = 5, wait_sec = 0.1, quiet = TRUE)
#'
#' # 3) Validate the result shape/value
#' smn_int_handle_error({
#'   list(status = 200, payload = 123)
#' }, is_ok = function(x) is.list(x) && x$status == 200)
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
                                 quiet            = FALSE) {
  if (!is.numeric(max_attempts) || max_attempts < 1) {
    stop("`max_attempts` must be a positive integer.")
  }
  if (!is.numeric(wait_sec) || wait_sec < 0) {
    stop("`wait_sec` must be a non-negative number.")
  }
  if (!is.numeric(backoff) || backoff < 1) {
    stop("`backoff` must be >= 1.")
  }
  if (!is.numeric(jitter) || jitter < 0 || jitter > 1) {
    stop("`jitter` must be in [0, 1].")
  }
  if (!is.null(is_ok) && !is.function(is_ok)) {
    stop("`is_ok` must be a function or NULL.")
  }

  # capture expression without adding rlang as a dependency
  expr_quo <- substitute(expr)
  last_err <- NULL
  last_warn <- NULL

  for (attempt in seq_len(max_attempts)) {
    # evaluate with handlers
    warn_msg <- NULL
    res <- withCallingHandlers(
      tryCatch(
        eval(expr_quo, envir = parent.frame()),
        error = function(e) {
          last_err <<- e
          NULL
        }
      ),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        if (retry_on_warning) {
          invokeRestart("muffleWarning")
        }
      }
    )

    # decide success
    ok_value <- !is.null(res)
    if (ok_value && !is.null(is_ok)) {
      ok_value <- isTRUE(is_ok(res))
      if (!ok_value) {
        last_err <- simpleError("Result validation failed (`is_ok` returned FALSE).")
      }
    }

    if (ok_value && (!retry_on_warning || is.null(warn_msg))) {
      return(res)
    }

    # report and sleep if more attempts remain
    if (!quiet) {
      base_msg <- if (!is.null(last_err)) {
        sprintf("Attempt %d/%d failed: %s", attempt, max_attempts, conditionMessage(last_err))
      } else if (!is.null(warn_msg) && retry_on_warning) {
        sprintf("Attempt %d/%d warning (retrying): %s", attempt, max_attempts, warn_msg)
      } else {
        sprintf("Attempt %d/%d did not meet success criteria.", attempt, max_attempts)
      }
      message(base_msg)
    }

    if (attempt < max_attempts) {
      # exponential backoff with jitter
      wait <- wait_sec * (backoff^(attempt - 1))
      if (jitter > 0) {
        wait <- wait * (1 + stats::runif(1, min = -jitter, max = jitter))
      }
      if (wait > 0) Sys.sleep(wait)
    }
  }

  stop(
    if (!is.null(last_err)) {
      paste0("Maximum attempts exhausted. Last error: ", conditionMessage(last_err))
    } else if (!is.null(last_warn) && retry_on_warning) {
      paste0("Maximum attempts exhausted. Last warning: ", last_warn)
    } else {
      "Maximum attempts exhausted."
    }
  )
}
