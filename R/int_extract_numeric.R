#' Extract First Numeric Value from Text (internal, vectorized)
#'
#' Extracts the first numeric token from each element of a character vector.
#' Supports optional sign, thousands separators (`,` or `.`), decimal mark
#' (`,` or `.`), and scientific notation (e.g., `-1.23e-4`).
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

  # Regex que EXIGE al menos un dígito y captura miles/decimal/científica.
  # Dos ramas:
  #  1) Con miles:   d{1,3} (sep d{3})* (dec)? (exp)?
  #  2) Simple:      d+ (dec)? (exp)?   OR   d+ (exp)?
  pattern <- paste0(
    "(?<!\\d)",                           # no estar pegado a otro dígito a la izquierda
    "(?:",
    "[-+]?\\d{1,3}(?:[\\.,]\\d{3})*(?:[\\.,]\\d+)?(?:[eE][+-]?\\d+)?", # con miles
    "|",
    "[-+]?\\d+(?:[\\.,]\\d+)?(?:[eE][+-]?\\d+)?",                      # simple con decimal/exp
    ")"
  )

  m <- regexpr(pattern, text, perl = TRUE)
  tokens <- ifelse(m > 0L, regmatches(text, m), NA_character_)

  normalize_token <- function(tok) {
    if (is.na(tok) || tok == "") return(NA_real_)

    # Averiguar si hay '.' o ',' y cuál es decimal
    last_dot <- regexpr("\\.[^\\.]*$", tok, perl = TRUE)
    last_com <- regexpr(",[^,]*$", tok, perl = TRUE)
    has_dot  <- grepl("\\.", tok, perl = TRUE)
    has_com  <- grepl(",", tok, perl = TRUE)

    if (has_dot && has_com) {
      # El separador DECIMAL suele ser el más a la derecha
      dec_is_dot <- (last_dot > last_com)
      if (dec_is_dot) {
        tok <- gsub(",", "", tok, fixed = TRUE)          # quita miles ','
      } else {
        tok <- gsub("\\.", "", tok, perl = TRUE)         # quita miles '.'
        tok <- sub(",", ".", tok, fixed = TRUE)          # coma → punto
      }
    } else if (has_com && !has_dot) {
      # Solo coma: tratar como decimal
      tok <- sub(",", ".", tok, fixed = TRUE)
    } else {
      # Solo punto (o ninguno): quitar miles tipo 1.234.567 ó 1,234,567
      tok <- gsub("(?<=\\d)[\\.,](?=\\d{3}(\\D|$))", "", tok, perl = TRUE)
    }

    as.numeric(tok)
  }

  out <- vapply(tokens, normalize_token, numeric(1))
  names(out) <- NULL
  out
}

