#' Download and Structure Daily Station Data (SMN)
#'
#' Downloads raw daily data for a given SMN station, filters it by a date window,
#' attaches station coordinates, and returns a cleaned table in either *full* or
#' *reduced* format. This is the basic building block used by higher-level
#' functions (e.g., \code{\link{smn_dl_daily_batch}} or the hybrid workflow).
#'
#' @param station Character or numeric SMN station code (e.g., \code{"15101"}).
#' @param start_date Start date (inclusive). \code{Date} or \code{character}. Default \code{"1961-01-01"}.
#' @param end_date   End date (inclusive). \code{Date} or \code{character}. Default \code{Sys.Date()}.
#' @param output_format Output format: \code{"full"} (default) or \code{"reduced"}.
#'   \itemize{
#'     \item \strong{full}: \code{station, latitude, longitude, altitude, date, prec, evap, tmax, tmin}
#'     \item \strong{reduced}: \code{date, prec, evap, tmax, tmin}
#'   }
#' @param del_na \code{"no"} (default) or \code{"yes"}. If \code{"yes"}, rows containing \code{NA}
#'   are dropped at the end.
#'
#' @return
#' A \strong{data.frame} with the columns defined by \code{output_format}, ordered by date.
#' If no rows fall within the requested window or the download fails, returns an
#' empty data frame with the appropriate columns for the chosen format.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Downloads raw daily data via \code{\link{smn_dl_daily_raw}} (best effort).
#'   \item Filters by \code{start_date <= date <= end_date}.
#'   \item Ensures variables \code{prec, evap, tmax, tmin} exist (filling missing ones with \code{NA}).
#'   \item Attaches coordinates using \code{\link{smn_int_extract_coordinates}} (falls back to \code{NA} on failure).
#' }
#' The goal is to deliver a stable, tidy-ready table for downstream analyses and
#' for integration with auxiliary sources (e.g., NASA POWER) in hybrid workflows.
#'
#' @seealso
#' \code{\link{smn_dl_daily_raw}}, \code{\link{smn_int_extract_coordinates}},
#' \code{\link{smn_dl_daily_batch}}
#'
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' # Minimal example (short window) with 'full' output
#' df_full <- smn_dl_daily_single(
#'   station = "15101",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-01-07",
#'   output_format = "full",
#'   del_na = "no"
#' )
#' head(df_full)
#'
#' # Same interval but 'reduced' output
#' df_red <- smn_dl_daily_single(
#'   station = "15101",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-01-07",
#'   output_format = "reduced"
#' )
#' head(df_red)
#' }
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
