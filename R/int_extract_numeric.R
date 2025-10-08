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
#' @keywords internal
#' @noRd
smn_int_extract_numeric <- function(text) {
  if (is.null(text)) return(NA_real_)
  text <- as.character(text)

  # Regex: optional sign, digits with optional decimal, optional exponent
  pattern <- "[-+]?\\d*(?:[\\.,]?\\d+)?(?:[eE][+-]?\\d+)?"

  m <- regexpr(pattern, text, perl = TRUE)

  # first match as character; NA when no match (m == -1)
  tokens <- ifelse(m > 0L, regmatches(text, m), NA_character_)
  tokens[ tokens == "" ] <- NA_character_  # normalize empty string to NA

  normalize_token <- function(tok) {
    if (is.na(tok)) return(NA_real_)

    last_dot <- regexpr("\\.[^\\.]*$", tok, perl = TRUE)
    last_com <- regexpr(",[^,]*$", tok, perl = TRUE)
    has_dot  <- grepl("\\.", tok, perl = TRUE)
    has_com  <- grepl(",", tok, perl = TRUE)

    if (has_dot && has_com) {
      dec_is_dot <- (last_dot > last_com)
      if (dec_is_dot) {
        tok <- gsub(",", "", tok, fixed = TRUE)
      } else {
        tok <- gsub("\\.", "", tok, perl = TRUE)
        tok <- sub(",", ".", tok, fixed = TRUE)
      }
    } else if (has_com && !has_dot) {
      tok <- sub(",", ".", tok, fixed = TRUE)
    } else {
      # quitar separadores de miles tipo 1,234 o 1.234
      tok <- gsub("(?<=\\d)[\\.,](?=\\d{3}(\\D|$))", "", tok, perl = TRUE)
    }

    as.numeric(tok)
  }

  out <- vapply(tokens, normalize_token, numeric(1))
  # quitar nombres para evitar fallos en expect_identical()
  names(out) <- NULL
  out
}


  vapply(tokens, normalize_token, numeric(1))
}

