#' Download and Structure Daily Station Data
#'
#' Downloads raw daily data for an SMN station, filters by date, attaches the
#' station coordinates, and returns a cleaned data frame in either *full* or
#' *reduced* format.
#'
#' @param station Character or numeric SMN station code.
#' @param start_date Start date (inclusive). Default `"1961-01-01"`.
#' @param end_date End date (inclusive). Default `Sys.Date()`.
#' @param output_format Output format: `"full"` (default) or `"reduced"`.
#'   * `"full"` columns: `station`, `latitude`, `longitude`, `altitude`,
#'   `date`, `prec`, `evap`, `tmax`, `tmin`.
#'   * `"reduced"` columns: `date`, `prec`, `evap`, `tmax`, `tmin`.
#' @param del_na `"yes"` to drop rows with missing values (in the returned
#'   columns), `"no"` (default) to keep them.
#'
#' @return A base `data.frame`. If no rows fall within the date range (or raw
#'   data is unavailable), an empty data frame with the appropriate columns for
#'   the chosen `output_format` is returned. Coordinate fields may be `NA` when
#'   unavailable.
#'
#' @details
#' Internally uses:
#' - [smn_dl_daily_raw()] to retrieve the raw daily series.
#' - [smn_int_extract_coordinates()] to retrieve `latitude`, `longitude`,
#'   and `altitude` for the station.
#'
#' @examples
#' \dontrun{
#' smn_dl_daily_single("15101", start_date = "2020-01-01", end_date = "2020-12-31")
#' smn_dl_daily_single("15101", output_format = "reduced", del_na = "yes")
#' }
#'
#' @export
smn_dl_daily_single <- function(station,
                                start_date = "1961-01-01",
                                end_date   = Sys.Date(),
                                output_format = c("full", "reduced"),
                                del_na = "no") {
  output_format <- match.arg(output_format)
  station <- as.character(station)

  # ---- date handling ---------------------------------------------------------

  .parse_date <- function(x, name) {
    d <- tryCatch(as.Date(x),
                  error = function(e) NA,
                  warning = function(w) suppressWarnings(as.Date(x)))
    if (is.na(d)) {
      stop(sprintf("`%s` must be coercible to Date.", name), call. = FALSE)
    }
    d
  }

  start_date <- .parse_date(start_date, "start_date")
  end_date   <- .parse_date(end_date,   "end_date")

  if (end_date < start_date) {
    stop("`end_date` must be on or after `start_date`.", call. = FALSE)
  }

  # ---- expected empty shapes -------------------------------------------------
  empty_full <- data.frame(
    station   = character(),
    latitude  = numeric(),
    longitude = numeric(),
    altitude  = numeric(),
    date      = as.Date(character()),
    prec      = numeric(),
    evap      = numeric(),
    tmax      = numeric(),
    tmin      = numeric(),
    stringsAsFactors = FALSE
  )
  empty_reduced <- empty_full[, c("date","prec","evap","tmax","tmin"), drop = FALSE]

  # ---- download raw daily data ----------------------------------------------
  raw_df <- tryCatch(
    smn_dl_daily_raw(station),
    error = function(e) {
      warning("Failed to download raw data for station ", station, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(raw_df) || !nrow(raw_df)) {
    return(if (output_format == "full") empty_full else empty_reduced)
  }

  # Ensure expected columns/types
  if (!"date" %in% names(raw_df)) stop("Raw data has no 'date' column.")
  raw_df$date <- as.Date(raw_df$date)
  keep_vars <- intersect(c("prec","evap","tmax","tmin"), names(raw_df))
  # filter by date (inclusive)
  raw_df <- raw_df[!is.na(raw_df$date) & raw_df$date >= start_date & raw_df$date <= end_date, ]
  if (!nrow(raw_df)) {
    return(if (output_format == "full") empty_full else empty_reduced)
  }

  # ---- retrieve coordinates (fast defaults; tolerate failure) ---------------
  coords <- tryCatch(
    smn_int_extract_coordinates(
      station
    ),
    error = function(e) data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
  )
  lat <- coords$latitude[1]; lon <- coords$longitude[1]; alt <- coords$altitude[1]

  # ---- build result ----------------------------------------------------------
  n <- nrow(raw_df)
  res_full <- data.frame(
    station   = rep_len(station, n),
    latitude  = rep_len(lat,     n),
    longitude = rep_len(lon,     n),
    altitude  = rep_len(alt,     n),
    date      = raw_df$date,
    # include only variables that exist; fill absent ones with NA
    prec      = if ("prec" %in% keep_vars) raw_df$prec else rep_len(NA_real_, n),
    evap      = if ("evap" %in% keep_vars) raw_df$evap else rep_len(NA_real_, n),
    tmax      = if ("tmax" %in% keep_vars) raw_df$tmax else rep_len(NA_real_, n),
    tmin      = if ("tmin" %in% keep_vars) raw_df$tmin else rep_len(NA_real_, n),
    stringsAsFactors = FALSE
  )

  # optional NA removal on the *returned* columns
  if (identical(tolower(del_na), "yes")) {
    res_full <- res_full[stats::complete.cases(res_full), , drop = FALSE]
  }

  # order by date
  res_full <- res_full[order(res_full$date), , drop = FALSE]
  rownames(res_full) <- NULL

  if (output_format == "reduced") {
    return(res_full[, c("date","prec","evap","tmax","tmin"), drop = FALSE])
  }
  res_full
}
