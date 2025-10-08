#' Download Raw Daily Data for a Station (robust header detection)
#'
#' Downloads and parses the raw daily data file from SMN using the internal URL.
#' The function is resilient to changes in the header length by detecting the
#' first data row via a date pattern (`YYYY-MM-DD`) rather than assuming a fixed
#' number of header lines.
#'
#' @param station Station code (character or numeric).
#' @param add_meta_cols Logical. If `TRUE`, includes `station` and `source_url`
#'   as explicit columns in the returned data (default `FALSE` to keep the
#'   original column schema). Metadata is always attached as attributes.
#' @param max_attempts Integer. Maximum retry attempts via `smn_int_handle_error`
#'   (default `10`).
#' @param encoding Character. Encoding passed to `readLines()` (default `"UTF-8"`).
#'
#' @return A base `data.frame` with columns: `date`, `prec`, `evap`, `tmax`, `tmin`.
#'   Attributes `station` and `source_url` are attached for provenance.
#'
#' @examples
#' \dontrun{
#' df <- smn_dl_daily_raw("15021")
#' attr(df, "source_url")
#' }
#'
#' @export
smn_dl_daily_raw <- function(station,
                             add_meta_cols = FALSE,
                             max_attempts  = 10,
                             encoding      = "UTF-8") {
  # ---- validate --------------------------------------------------------------
  if (missing(station) || !nzchar(as.character(station))) {
    stop("`station` must be a non-empty station code.")
  }
  station <- as.character(station)

  # Build URL (internal helper)
  url <- smn_int_get_url(station)

  # ---- download lines with retry & encoding ---------------------------------
  # IMPORTANT: pass `envir = environment()` so the expression sees `url` and `encoding`
  lines <- smn_int_handle_error(
    {
      readLines(url, warn = FALSE, encoding = encoding)
    },
    max_attempts = max_attempts,
    quiet        = TRUE,
    envir        = environment()
  )

  if (is.null(lines) || length(lines) == 0L) {
    stop("Empty response for station ", station, " (url: ", url, ")")
  }

  # remove UTF-8 BOM and trim whitespace; drop fully empty lines
  lines <- sub("^\ufeff", "", lines)
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0L]

  # ---- detect first data row by date pattern --------------------------------
  is_data <- grepl("^\\d{4}-\\d{2}-\\d{2}\\b", lines)
  if (!any(is_data)) {
    stop("No data rows detected for station ", station, " (url: ", url, ")")
  }
  first_row  <- which(is_data)[1]
  data_lines <- lines[first_row:length(lines)]

  # ---- parse with readr if present; otherwise base R ------------------------
  na_tokens <- "NULO"  # SMN sentinel
  if (requireNamespace("readr", quietly = TRUE)) {
    df <- readr::read_table2(
      file = I(data_lines),
      col_names = c("date", "prec", "evap", "tmax", "tmin"),
      col_types = readr::cols(
        date = readr::col_date(format = ""),
        prec = readr::col_double(),
        evap = readr::col_double(),
        tmax = readr::col_double(),
        tmin = readr::col_double()
      ),
      na = na_tokens,
      progress = FALSE
    )
    df <- as.data.frame(df)
  } else {
    con <- textConnection(data_lines)
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(
      con,
      header     = FALSE,
      col.names  = c("date", "prec", "evap", "tmax", "tmin"),
      colClasses = c("character", rep("numeric", 4)),
      na.strings = na_tokens,
      fill       = TRUE,
      strip.white= TRUE
    )
    df$date <- as.Date(df$date)
  }

  # ---- sanity checks ---------------------------------------------------------
  if (!nrow(df)) {
    stop("Parsed zero data rows for station ", station, " (url: ", url, ")")
  }
  if (any(df$date < as.Date("1900-01-01"), na.rm = TRUE)) {
    warning("Unusual dates detected in station ", station, ". Please inspect the source file.")
  }

  # ---- optional meta columns; always attributes ------------------------------
  if (isTRUE(add_meta_cols)) {
    df$station    <- station
    df$source_url <- url
    df <- df[, c("date","prec","evap","tmax","tmin","station","source_url")]
  }
  attr(df, "station")    <- station
  attr(df, "source_url") <- url

  rownames(df) <- NULL
  df
}
