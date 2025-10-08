#' Download and Structure Daily Station Data
#'
#' Downloads raw daily data for an SMN station, filters by date, attaches the
#' station coordinates, and returns a cleaned data frame in either *full* or
#' *reduced* format.
#'
#' @param station Character or numeric SMN station code (e.g., "15101").
#' @param start_date Start date (inclusive). Character or Date. Default "1961-01-01".
#' @param end_date End date (inclusive). Character or Date. Default Sys.Date().
#' @param output_format Output format: "full" (default) or "reduced".
#'   - "full": station, latitude, longitude, altitude, date, prec, evap, tmax, tmin
#'   - "reduced": date, prec, evap, tmax, tmin
#' @param del_na One of "no" (default) or "yes". If "yes", rows with NAs are dropped at the end.
#'
#' @return A base data.frame. When no rows fall in the date range or download fails,
#'   returns an empty data frame with the appropriate columns for the chosen format.
#' @export
smn_dl_daily_single <- function(station,
                                start_date = "1961-01-01",
                                end_date   = Sys.Date(),
                                output_format = c("full", "reduced"),
                                del_na = c("no", "yes")) {
  output_format <- match.arg(output_format)
  del_na        <- match.arg(del_na)

  if (missing(station) || !nzchar(as.character(station))) {
    stop("`station` must be a non-empty station code.")
  }
  station <- as.character(station)

  # robust date coercion with stable error message
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    val <- tryCatch(as.Date(x), error = function(e) NA)
    if (is.na(val)) stop("`start_date` and `end_date` must be coercible to Date.")
    val
  }
  start_date <- to_date(start_date)
  end_date   <- to_date(end_date)
  if (end_date < start_date) stop("`end_date` must be on or after `start_date`.")

  # expected empty shapes
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
    check.names = FALSE
  )
  empty_reduced <- empty_full[, c("date","prec","evap","tmax","tmin"), drop = FALSE]

  # download raw (best-effort)
  raw_df <- tryCatch(
    smn_dl_daily_raw(station),
    error = function(e) {
      warning("Failed to download raw data for station ", station, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw_df) || !nrow(raw_df)) {
    warning("No daily data available for station ", station, " in the requested date range.")
    return(if (output_format == "full") empty_full else empty_reduced)
  }

  # require only 'date'; other variables are optional and will be filled with NA
  if (!"date" %in% names(raw_df)) {
    warning("Raw daily data has no 'date' column. Returning empty result.")
    return(if (output_format == "full") empty_full else empty_reduced)
  }
  if (!inherits(raw_df$date, "Date")) raw_df$date <- as.Date(raw_df$date)

  # filter by date
  raw_df <- raw_df[!is.na(raw_df$date) & raw_df$date >= start_date & raw_df$date <= end_date, , drop = FALSE]
  if (!nrow(raw_df)) {
    warning("No daily data available for station ", station, " in the requested date range.")
    return(if (output_format == "full") empty_full else empty_reduced)
  }

  # ensure variables exist; fill missing with NA
  vars <- c("prec","evap","tmax","tmin")
  for (v in setdiff(vars, names(raw_df))) raw_df[[v]] <- NA_real_

  # coordinates (best-effort)
  coords <- tryCatch(
    smn_int_extract_coordinates(station),
    error = function(e) {
      warning("Coordinate extraction failed for station ", station, ": ", conditionMessage(e))
      data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
    }
  )
  if (!all(c("latitude","longitude","altitude") %in% names(coords))) {
    coords <- data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
  }

  n   <- nrow(raw_df)
  lat <- coords$latitude[1]; lon <- coords$longitude[1]; alt <- coords$altitude[1]

  res_full <- data.frame(
    station   = rep_len(station, n),
    latitude  = rep_len(lat,     n),
    longitude = rep_len(lon,     n),
    altitude  = rep_len(alt,     n),
    date      = raw_df$date,
    prec      = raw_df$prec,
    evap      = raw_df$evap,
    tmax      = raw_df$tmax,
    tmin      = raw_df$tmin,
    check.names = FALSE
  )

  if (del_na == "yes") {
    res_full <- stats::na.omit(res_full)
  }

  res_full <- res_full[order(res_full$date), , drop = FALSE]
  rownames(res_full) <- NULL

  if (output_format == "reduced") {
    return(res_full[, c("date","prec","evap","tmax","tmin"), drop = FALSE])
  }
  res_full
}

