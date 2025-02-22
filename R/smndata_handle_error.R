#' Handle Errors with Retries
#'
#' Evaluates an expression and, if an error occurs, retries execution up to a maximum
#' number of attempts. If all attempts fail, the function stops with an error.
#'
#' @param expr An expression to evaluate.
#' @param max_attempts Maximum number of attempts (default is 3).
#'
#' @return The result of the evaluated expression if successful.
#'
#' @examples
#' # Example 1: A simple successful evaluation.
#' result <- smndata_handle_error({ 1 + 1 })
#' print(result)  # Expected output: 2
#'
#' # Example 2: A failing expression.
#' # This will try to divide by zero and ultimately stop with an error after 2 attempts.
#' \dontrun{
#'   smndata_handle_error({ 1 / 0 }, max_attempts = 2)
#' }
#'
#' @export
smndata_handle_error <- function(expr, max_attempts = 3) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      expr,
      error = function(e) {
        message(sprintf("Error on attempt %d: %s", attempt, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
  stop("Maximum attempts exhausted.")
}
