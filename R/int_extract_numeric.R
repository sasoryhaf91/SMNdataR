#' Extract First Numeric Value from Text (internal, decimal only)
#'
#' Quickly extracts the first decimal number from each element of a character
#' vector. Supports an optional leading sign and a dot (`.`) as the decimal
#' separator. This helper intentionally does **not** handle scientific notation
#' or comma-decimals, matching SMN pages that publish coordinates in simple
#' decimal form.
#'
#' @param text Character vector containing numeric information.
#'
#' @return A numeric vector of the same length as `text`. Elements are
#'   `NA_real_` when no decimal number is present.
#'
#' @examples
#' smn_int_extract_numeric("Latitude: 19.4567")   # 19.4567
#' smn_int_extract_numeric("Lon = -99.1332 deg")  # -99.1332
#' smn_int_extract_numeric(c("a", "x 10", "y -2.5"))
#'
#' @keywords internal
#' @noRd
smn_int_extract_numeric <- function(text) {
  if (is.null(text)) return(NA_real_)
  text <- as.character(text)

  # "[-+]?(?:\\d*\\.\\d+|\\d+)"  ->  optional sign + (decimals like .5 or 0.5) OR integer
  pattern <- "[-+]?(?:\\d*\\.\\d+|\\d+)"

  mm   <- gregexpr(pattern, text, perl = TRUE)
  lst  <- regmatches(text, mm)
  tok  <- vapply(lst, function(v) if (length(v)) v[1L] else NA_character_, character(1))

  out <- suppressWarnings(as.numeric(tok))
  names(out) <- NULL
  out
}


