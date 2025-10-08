#' Extract Station Coordinates (internal, decimal-only, fast)
#'
#' Reads a station's metadata page and extracts `latitude`, `longitude`, and
#' `altitude` assuming the values are already provided as **decimal numbers
#' using a dot** (e.g., `19.4567`, `-99.1332`). This helper does **not** attempt
#' to normalize comma decimals, parse DMS (degrees–minutes–seconds), or infer
#' hemispheres from text; the numeric sign is taken as-is from the page.
#'
#' A short timeout and a limited number of retries are used to keep calls fast
#' and predictable. On failure or missing fields, the corresponding value is
#' returned as `NA_real_`.
#'
#' @param station Character or numeric station code.
#' @param max_attempts Integer. Maximum retries when reading the page
#'   (default `2`).
#' @param timeout_sec Numeric. Connection timeout in seconds (default `4`).
#' @param encoding Character. Encoding for `readLines()` (default `"UTF-8"`).
#' @param add_meta_cols Logical. If `TRUE`, append `station` and `source_url`
#'   as explicit output columns (in addition to always attaching them as
#'   attributes). Default `FALSE`.
#' @param warn_on_na Logical. If `TRUE`, emit a warning when any of the
#'   coordinates is missing (`NA`). Default `FALSE`.
#'
#' @return A base `data.frame` with one row and columns:
#'   \itemize{
#'     \item `latitude`  — station latitude (numeric; `NA` if missing),
#'     \item `longitude` — station longitude (numeric; `NA` if missing),
#'     \item `altitude`  — station altitude (numeric; `NA` if missing).
#'   }
#'   The attributes `station` and `source_url` are always attached for
#'   provenance. If `add_meta_cols = TRUE`, these are also included as columns.
#'
#' @details
#' The function searches the fetched text for lines containing latitude,
#' longitude, and altitude labels in Spanish/English (e.g., `"Latitud"`,
#' `"Latitude"`, `"Longitud"`, `"Longitude"`, `"Altitud"`, `"Altitude"`,
#' including `Lat.`, `Lon.`, `Alt.`). It then extracts the **first** decimal
#' number from each line using \code{smn_int_extract_numeric()}.
#'
#' @seealso \code{\link{smn_int_get_url}}, \code{\link{smn_int_handle_error}},
#'   \code{\link{smn_int_extract_numeric}}
#'
#' @examples
#' \dontrun{
#' # Simple usage (fast defaults):
#' smn_int_extract_coordinates("15021")
#'
#' # Include provenance as columns:
#' smn_int_extract_coordinates("15021", add_meta_cols = TRUE)
#' }
#'
#' @keywords internal
#' @noRd
smn_int_extract_coordinates <- function(station,
                                        max_attempts  = 2,
                                        timeout_sec   = 4,
                                        encoding      = "UTF-8",
                                        add_meta_cols = FALSE,
                                        warn_on_na    = FALSE) {
  # small helper for consistent NA-shaped output + provenance
  .na_out <- function(st, url) {
    out <- data.frame(
      latitude  = NA_real_,
      longitude = NA_real_,
      altitude  = NA_real_
    )
    attr(out, "station")    <- st
    attr(out, "source_url") <- url
    if (isTRUE(add_meta_cols)) {
      out$station    <- st
      out$source_url <- url
      out <- out[, c("latitude","longitude","altitude","station","source_url"), drop = FALSE]
    }
    rownames(out) <- NULL
    out
  }

  # ---- validate input --------------------------------------------------------
  if (missing(station) || !nzchar(as.character(station))) {
    stop("`station` must be a non-empty station code.")
  }
  station <- as.character(station)

  # ---- build URL (tolerant to bad IDs) --------------------------------------
  url <- tryCatch(
    smn_int_get_url(station),
    error = function(e) NA_character_
  )
  if (is.na(url) || !nzchar(url)) {
    return(.na_out(station, NA_character_))
  }

  # ---- short timeout + limited retries for fast behavior --------------------
  old_to <- getOption("timeout")
  on.exit(options(timeout = old_to), add = TRUE)
  if (is.numeric(timeout_sec) && timeout_sec > 0) {
    options(timeout = timeout_sec)
  }

  lines <- tryCatch(
    smn_int_handle_error(
      {
        readLines(url, warn = FALSE, encoding = encoding)
      },
      max_attempts = max_attempts,
      quiet        = TRUE,
      envir        = environment()
    ),
    error = function(e) NULL
  )
  if (is.null(lines) || !length(lines)) {
    if (isTRUE(warn_on_na)) warning("No metadata available for station ", station)
    return(.na_out(station, url))
  }

  lines <- trimws(lines)

  # ---- locate labeled lines (ES/EN + abbreviations) -------------------------
  lat_idx <- grep("\\b(latitud|latitude|lat\\.?)(\\b|:)",  lines, ignore.case = TRUE, perl = TRUE)
  lon_idx <- grep("\\b(longitud|longitude|lon\\.?)(\\b|:)", lines, ignore.case = TRUE, perl = TRUE)
  alt_idx <- grep("\\b(altitud|altitude|alt\\.?)(\\b|:)",  lines, ignore.case = TRUE, perl = TRUE)
  if (!length(lat_idx)) lat_idx <- grep("\\blat", lines, ignore.case = TRUE)
  if (!length(lon_idx)) lon_idx <- grep("\\blon", lines, ignore.case = TRUE)
  if (!length(alt_idx)) alt_idx <- grep("\\balt", lines, ignore.case = TRUE)

  lat_line <- if (length(lat_idx)) lines[lat_idx[1L]] else NA_character_
  lon_line <- if (length(lon_idx)) lines[lon_idx[1L]] else NA_character_
  alt_line <- if (length(alt_idx)) lines[alt_idx[1L]] else NA_character_

  # ---- decimal-only extraction (dot as decimal separator) -------------------
  latitude  <- if (!is.na(lat_line)) smn_int_extract_numeric(lat_line)  else NA_real_
  longitude <- if (!is.na(lon_line)) smn_int_extract_numeric(lon_line) else NA_real_
  altitude  <- if (!is.na(alt_line)) smn_int_extract_numeric(alt_line)  else NA_real_

  # ---- basic range checks ----------------------------------------------------
  if (!is.na(latitude)  && (latitude  < -90  || latitude  >  90))  latitude  <- NA_real_
  if (!is.na(longitude) && (longitude < -180 || longitude > 180))  longitude <- NA_real_

  if (isTRUE(warn_on_na) && (is.na(latitude) || is.na(longitude) || is.na(altitude))) {
    warning("Incomplete coordinates for station ", station, " (one or more NA values).")
  }

  # ---- build result ----------------------------------------------------------
  out <- data.frame(latitude = latitude, longitude = longitude, altitude = altitude)
  attr(out, "station")    <- station
  attr(out, "source_url") <- url

  if (isTRUE(add_meta_cols)) {
    out$station    <- station
    out$source_url <- url
    out <- out[, c("latitude","longitude","altitude","station","source_url"), drop = FALSE]
  }

  rownames(out) <- NULL
  out
}
