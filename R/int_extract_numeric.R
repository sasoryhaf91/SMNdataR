#' Extract First Decimal Number from Text (internal, vectorized)
#'
#' Extracts the **first** decimal number from each element of a character
#' vector, assuming a dot (`.`) as the decimal separator. Supports an optional
#' leading sign and decimals with or without a leading zero (e.g., `.5`).
#' Scientific notation and comma decimals are intentionally **not** supported.
#'
#' @param text Character vector containing numeric information.
#'
#' @return A numeric vector of the same length as `text`. Elements are
#'   `NA_real_` when no decimal number is found.
#'
#' @examples
#' smn_int_extract_numeric("Latitude: 19.4567")       # 19.4567
#' smn_int_extract_numeric("Lon = -99.1332 deg")      # -99.1332
#' smn_int_extract_numeric(c("a", "x 10", "y -2.5"))  # NA, 10, -2.5
#' smn_int_extract_numeric(".5 is half")              # 0.5
#'
#' @keywords internal
#' @noRd
smn_int_extract_numeric <- function(text) {
  if (is.null(text)) return(NA_real_)
  text <- as.character(text)

  # Require at least one digit; allow:
  #  - integers:          10
  #  - decimals:          10.5
  #  - leading-dot dec.:  .5
  # Optional leading sign for all cases.
  pattern <- "[-+]?(?:\\d+\\.\\d+|\\d+|\\.\\d+)"

  # Get the first match per element (keeps alignment with input length)
  mm  <- gregexpr(pattern, text, perl = TRUE)
  lst <- regmatches(text, mm)
  tok <- vapply(lst, function(v) if (length(v)) v[1L] else NA_character_, character(1))

  out <- suppressWarnings(as.numeric(tok))
  names(out) <- NULL
  out
}
