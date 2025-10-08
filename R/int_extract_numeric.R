#' Extract First Numeric Value from Text (internal, vectorized)
#'
#' Extracts the first numeric token from each element of a character vector.
#' Supports optional sign and scientific notation (e.g., `-1.23e-4`). The
#' function tolerates either `.` or `,` as decimal marks and removes likely
#' thousands separators before conversion.
#'
#' @param text Character vector containing numeric information.
#'
#' @return A numeric vector of the same length as `text`. Elements are
#'   `NA_real_` when no numeric token is present.
#'
#' @examples
#' smn_int_extract_numeric("Latitude: 19.4567")          # 19.4567
#' smn_int_extract_numeric("Temp = -2,3 °C")             # -2.3
#' smn_int_extract_numeric("Value: 1,234.56 m")          # 1234.56
#' smn_int_extract_numeric(c("x", "y 10", "z -1.2e3"))   # NA, 10, -1200
#'
#' @keywords internal
#' @noRd
smn_int_extract_numeric <- function(text) {
  if (is.null(text)) return(NA_real_)
  text <- as.character(text)

  # Regex for the FIRST numeric token:
  # optional sign, digits with optional decimal mark, optional exponent.
  pattern <- "[-+]?\\d*(?:[\\.,]?\\d+)?(?:[eE][+-]?\\d+)?"

  # Base R extraction (first match only)
  m <- regexpr(pattern, text, perl = TRUE)
  tokens <- ifelse(m > 0L, regmatches(text, m), NA_character_)

  # Normalize tokens to parse as numeric with '.' decimal mark
  normalize_token <- function(tok) {
    if (is.na(tok) || tok == "") return(NA_real_)

    # If both '.' and ',' appear, decide which is decimal:
    # heuristic: the LAST occurrence is the decimal mark (common in US/EU formats).
    last_dot <- regexpr("\\.[^\\.]*$", tok, perl = TRUE)
    last_com <- regexpr(",[^,]*$", tok, perl = TRUE)
    has_dot  <- grepl("\\.", tok, perl = TRUE)
    has_com  <- grepl(",", tok, perl = TRUE)

    if (has_dot && has_com) {
      # Pick the rightmost as decimal; remove the other as thousands sep
      dec_is_dot <- (last_dot > last_com)
      if (dec_is_dot) {
        tok <- gsub(",", "", tok, fixed = TRUE)
      } else {
        tok <- gsub("\\.", "", tok, perl = TRUE)
        tok <- sub(",", ".", tok, fixed = TRUE)
      }
    } else if (has_com && !has_dot) {
      # Only comma present → treat as decimal mark
      tok <- sub(",", ".", tok, fixed = TRUE)
    } else {
      # Only dot or none → nothing special
      # Additionally, drop obvious thousands separators like "1.234.567"
      tok <- gsub("(?<=\\d)[\\.,](?=\\d{3}(\\D|$))", "", tok, perl = TRUE)
    }

    # Final parse
    as.numeric(tok)
  }

  vapply(tokens, normalize_token, numeric(1))
}

