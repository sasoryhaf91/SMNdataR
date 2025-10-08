#' Download Raw Daily Data for a Station (robust header detection)
#'
#' Downloads and parses the raw daily data file from SMN using the internal URL.
#' The function is resilient to changes in the header length by detecting the
#' first data row via a date pattern (YYYY-MM-DD), rather than assuming a fixed
#' number of header lines.
#'
#' @param station Station code (character or numeric).
#' @param add_meta_cols Logical. If `TRUE`, include `station` and `source_url`
#'   as explicit columns in the returned data (default `FALSE` to keep the
#'   original column schema). Metadata is always attached as attributes.
#'
#' @return A data frame with columns: `date`, `prec`, `evap`, `tmax`, `tmin`.
#'   Attributes `station` and `source_url` are attached for provenance.
#'
#' @examples
#' \dontrun{
#' df <- smn_dl_daily_raw("15021")
#' attr(df, "source_url")
#' }
#'
#' @export
smn_dl_daily_raw <- function(station, add_meta_cols = FALSE) {
  # ---- validate --------------------------------------------------------------
  if (missing(station) || !nzchar(as.character(station))) {
    stop("`station` must be a non-empty station code.")
  }
  station <- as.character(station)

  # Internal helper must exist in your package
  url <- smn_int_get_url(station)

  # ---- download lines with retry & UTF-8 ------------------------------------
  lines <- smn_int_handle_error(
    {
      # quiet read, UTF-8, tolerate incomplete final line
      readLines(url, warn = FALSE, encoding = "UTF-8")
    },
    max_attempts = 10
  )

  if (is.null(lines) || length(lines) == 0L) {
    stop("Empty response for station ", station, " (url: ", url, ")")
  }

  # remove UTF-8 BOM and trim whitespace; drop fully empty lines
  lines <- sub("^\ufeff", "", lines)
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0L]

  # ---- detect first data row by date pattern --------------------------------
  # Expected SMN format: YYYY-MM-DD <spaces> prec evap tmax tmin
  is_data <- grepl("^\\d{4}-\\d{2}-\\d{2}\\b", lines)
  if (!any(is_data)) {
    stop("No data rows detected for station ", station, " (url: ", url, ")")
  }
  first_row <- which(is_data)[1]
  data_lines <- lines[first_row:length(lines)]

  # ---- parse with readr if present; otherwise base R ------------------------
  na_tokens <- c("NULO")  # SMN missing sentinel
  df <- NULL

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
    # coerce to base data.frame for minimal deps outward
    df <- as.data.frame(df)
  } else {
    # Base R fallback (slower but dependency-free)
    con <- textConnection(data_lines)
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(
      con,
      header = FALSE,
      col.names = c("date", "prec", "evap", "tmax", "tmin"),
      colClasses = c("character", rep("numeric", 4)),
      na.strings = na_tokens,
      fill = TRUE,
      strip.white = TRUE
    )
    # parse dates safely
    df$date <- as.Date(df$date)
  }

  # ---- minimal sanity checks -------------------------------------------------
  if (!nrow(df)) {
    stop("Parsed zero data rows for station ", station, " (url: ", url, ")")
  }
  if (any(df$date < as.Date("1900-01-01"), na.rm = TRUE)) {
    warning("Unusual dates detected in station ", station, ". Please inspect the source file.")
  }

  # ---- optionally add meta columns; always set attributes --------------------
  if (isTRUE(add_meta_cols)) {
    df$station    <- station
    df$source_url <- url
    # keep standard columns first
    df <- df[, c("date","prec","evap","tmax","tmin","station","source_url")]
  }
  attr(df, "station")    <- station
  attr(df, "source_url") <- url

  rownames(df) <- NULL
  df
}
