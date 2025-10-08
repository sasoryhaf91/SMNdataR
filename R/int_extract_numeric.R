#' Extract First Numeric Value from Text (internal, vectorized)
#'
#' Extracts the first numeric token from each element of a character vector.
#' Supports optional sign, thousands separators (`,` or `.`), decimal mark
#' (`,` or `.`), and scientific notation (e.g., `-1.23e-4`).
#'
#' @param text Character vector containing numeric information.
#' @return A numeric vector of the same length as `text`. `NA_real_` when no number is found.
#' @keywords internal
#' @noRd
smn_int_extract_numeric <- function(text) {
  if (is.null(text)) return(NA_real_)
  text <- as.character(text)

  # Require at least one digit; capture thousands/decimal/scientific
  pattern <- "[-+]?(?:\\d{1,3}(?:[\\.,]\\d{3})+|\\d+)(?:[\\.,]\\d+)?(?:[eE][+-]?\\d+)?"

  m <- regexpr(pattern, text, perl = TRUE)
  tokens <- ifelse(m > 0L, regmatches(text, m), NA_character_)

  normalize_token <- function(tok) {
    if (is.na(tok) || tok == "") return(NA_real_)

    has_dot  <- grepl("\\.", tok)
    has_com  <- grepl(",", tok)
    n_com    <- length(gregexpr(",", tok, fixed = TRUE)[[1]])
    n_com    <- if (n_com == 1L && gregexpr(",", tok, fixed = TRUE)[[1]][1L] == -1L) 0L else n_com
    # thousands pattern: any comma followed by exactly 3 digits, possibly repeated
    looks_thousands_comma <- grepl(",\\d{3}(?:\\D|$)", tok) && (n_com > 1L || grepl(",\\d{3}(?:,\\d{3})+$", tok))

    if (has_dot && has_com) {
      # decimal = rightmost of dot/comma; remove the other as thousands
      last_dot <- regexpr("\\.[^\\.]*$", tok, perl = TRUE)
      last_com <- regexpr(",[^,]*$", tok, perl = TRUE)
      dec_is_dot <- (last_dot > last_com)
      if (dec_is_dot) {
        tok <- gsub(",", "", tok, fixed = TRUE)
      } else {
        tok <- gsub("\\.", "", tok, perl = TRUE)
        tok <- sub(",", ".", tok, fixed = TRUE)
      }
    } else if (has_com && !has_dot) {
      if (looks_thousands_comma) {
        tok <- gsub(",", "", tok, fixed = TRUE)   # treat as thousands
      } else {
        tok <- gsub(",", ".", tok, fixed = TRUE)  # single decimal comma
      }
    } else if (has_dot && !has_com) {
      # remove dot used as thousands: 1.234.567 or trailing groups of .000
      tok <- gsub("(?<=\\d)\\.(?=\\d{3}(\\D|$))", "", tok, perl = TRUE)
    }
    as.numeric(tok)
  }

  out <- vapply(tokens, normalize_token, numeric(1))
  names(out) <- NULL
  out
}

