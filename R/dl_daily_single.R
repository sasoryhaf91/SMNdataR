#' Download and Structure Daily Station Data
#'
#' Downloads raw daily data for a given SMN station, filters it by date, merges with coordinates,
#' and returns a cleaned data frame in either full or reduced format.
#'
#' @param station Station code (character or numeric).
#' @param start_date Start date for the data (default is "1961-01-01").
#' @param end_date End date for the data (default is \code{Sys.Date()}).
#' @param output_format Format of the output: either "full" (default) or "reduced".
#' @param del_na Whether to remove rows with missing values: "yes" or "no" (default).
#'
#' @return A data frame containing daily station data.
#'
#' @details
#' This function combines data from:
#' \itemize{
#'   \item \code{smn_dl_daily_raw()}: for downloading raw daily data.
#'   \item \code{smn_int_extract_coordinates()}: for retrieving station coordinates.
#' }
#'
#' @examples
#' \dontrun{
#'   smn_dl_daily_single("15101", start_date = "2020-01-01", end_date = "2020-12-31")
#'   smn_dl_daily_single("15101", output_format = "reduced", del_na = "yes")
#' }
#'
#' @export
smn_dl_daily_single <- function(station,
                                start_date = "1961-01-01",
                                end_date = Sys.Date(),
                                output_format = c("full", "reduced"),
                                del_na = "no") {
  output_format <- match.arg(output_format)
  station <- as.character(station)

  # Download raw daily data
  df <- tryCatch({
    smn_dl_daily_raw(station)
  }, error = function(e) {
    stop("Failed to download raw data for station ", station, ": ", conditionMessage(e))
  })

  df$date <- as.Date(df$date)
  df <- df[df$date >= as.Date(start_date) & df$date <= as.Date(end_date), ]

  if (nrow(df) == 0) {
    warning("No available data for station ", station, " in the specified date range.")
    return(data.frame())
  }

  # Retrieve coordinates
  coords <- smn_int_extract_coordinates(station)
  n <- nrow(df)

  result <- data.frame(
    station   = rep(station, n),
    latitude  = rep(coords$latitude, n),
    longitude = rep(coords$longitude, n),
    altitude  = rep(coords$altitude, n),
    date      = df$date,
    prec      = df$prec,
    evap      = df$evap,
    tmax      = df$tmax,
    tmin      = df$tmin,
    stringsAsFactors = FALSE
  )

  if (del_na == "yes") {
    result <- na.omit(result)
  }

  if (output_format == "reduced") {
    result <- result[, c("date", "prec", "evap", "tmax", "tmin")]
  }

  return(result)
}
