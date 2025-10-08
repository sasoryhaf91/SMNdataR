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

  # Requiere al menos un dígito; captura miles/decimal/científica.
  pattern <- "[-+]?(?:\\d{1,3}(?:[\\.,]\\d{3})+|\\d+)(?:[\\.,]\\d+)?(?:[eE][+-]?\\d+)?"

  # Lista de coincidencias por elemento (alineada)
  mm   <- gregexpr(pattern, text, perl = TRUE)
  lst  <- regmatches(text, mm)
  toks <- vapply(lst, function(v) if (length(v) >= 1L) v[1L] else NA_character_, character(1))

  # Normaliza separadores / coma decimal y convierte
  normalize_token <- function(tok) {
    if (is.na(tok) || tok == "") return(NA_real_)

    has_dot <- grepl("\\.", tok, perl = TRUE)
    has_com <- grepl(",", tok, perl = TRUE)

    if (has_dot && has_com) {
      # el separador decimal es el más a la derecha
      last_dot <- regexpr("\\.[^\\.]*$", tok, perl = TRUE)
      last_com <- regexpr(",[^,]*$", tok, perl = TRUE)
      dec_is_dot <- (last_dot > last_com)
      if (dec_is_dot) {
        tok <- gsub(",", "", tok, fixed = TRUE)         # comas como miles
      } else {
        tok <- gsub("\\.", "", tok, perl = TRUE)        # puntos como miles
        tok <- sub(",", ".", tok, fixed = TRUE)         # coma decimal → punto
      }
    } else if (has_com && !has_dot) {
      # ¿comas como miles o decimal?
      # miles si hay >1 coma o patrón de grupos de 3
      ncom <- length(gregexpr(",", tok, fixed = TRUE)[[1]])
      ncom <- if (ncom == 1L && gregexpr(",", tok, fixed = TRUE)[[1]][1L] == -1L) 0L else ncom
      looks_thousands <- grepl(",\\d{3}(?:,\\d{3})*(?:\\D|$)", tok)
      if (ncom > 1L || looks_thousands) {
        tok <- gsub(",", "", tok, fixed = TRUE)
      } else {
        tok <- sub(",", ".", tok, fixed = TRUE)
      }
    } else if (has_dot && !has_com) {
      # quitar puntos como miles (1.234.567)
      tok <- gsub("(?<=\\d)\\.(?=\\d{3}(\\D|$))", "", tok, perl = TRUE)
    }
    as.numeric(tok)
  }

  out <- vapply(toks, normalize_token, numeric(1))
  names(out) <- NULL
  out
}

