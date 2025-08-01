#' Download Daily Station Data with Format and NA Removal Options
#'
#' Downloads the raw daily data for a given station, filters the data by the specified date range,
#' and extracts the station's coordinates from its metadata. The function returns a data frame in
#' one of two formats:
#'
#' \describe{
#'   \item{"full"}{Returns a data frame with columns: station, latitude, longitude, altitude, date,
#'                 prec, evap, tmax, and tmin. (Default)}
#'   \item{"reduced"}{Returns a data frame with only the daily climatic data: date, prec, evap,
#'                    tmax, and tmin.}
#' }
#'
#' Additionally, if \code{del_na} is set to \code{"yes"}, any rows containing NA values
#' in any of the columns are removed. By default, \code{del_na} is \code{"no"}.
#'
#' @param station Station code (character or numeric).
#' @param start_date Start date for data download (default "1961-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param output_format Output format: either "full" (default) or "reduced".
#' @param del_na Character string indicating whether to remove rows with NA values:
#'        \describe{
#'          \item{"yes"}{Remove any rows containing NA in any column.}
#'          \item{"no"}{Keep rows with NA values. (Default)}
#'        }
#'
#' @return A data frame with daily data in the specified format.
#'
#' @details
#' This function relies on two helper functions:
#' \itemize{smd
#'   \item \code{smndata_download_station_raw(station)}: Downloads the raw daily data with columns
#'         "date", "prec", "evap", "tmax", and "tmin".
#'   \item \code{smndata_extract_coordinates(station)}: Extracts the station's latitude, longitude, and altitude.
#' }
#' The function then merges the raw data with the station coordinates, filters by \code{start_date}
#' and \code{end_date}, optionally removes NA rows, and selects columns according to \code{output_format}.
#'
#' @examples
#' \dontrun{
#'   # Download full format data for station "15101" without removing NA rows
#'   df_full <- smndata_download_station(
#'     station = "15101",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     output_format = "full",
#'     del_na = "no"
#'   )
#'
#'   # Download reduced format data for station "15101" and remove rows with NA
#'   df_reduced <- smndata_download_station(
#'     station = "15101",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     output_format = "reduced",
#'     del_na = "yes"
#'   )
#' }
#'
#' @export
smndata_download_station <- function(station,
                                     start_date = "1961-01-01",
                                     end_date = Sys.Date(),
                                     output_format = c("full", "reduced"),
                                     del_na = "no") {
  output_format <- match.arg(output_format)

  # Download raw daily data using the optimized raw download function
  df <- tryCatch({
    smn_dl_download_station_raw(station)
  }, error = function(e) {
    stop("Error downloading raw data for station ", station, ": ", conditionMessage(e))
  })

  # Ensure the 'date' column is in Date format and filter by the specified date range
  df$date <- as.Date(df$date)
  df <- df[df$date >= as.Date(start_date) & df$date <= as.Date(end_date), ]

  if (nrow(df) == 0) {
    warning("No data available for station ", station, " in the specified date range.")
    return(data.frame())
  }

  # Extract station coordinates using the helper function
  coords <- smn_int_extract_coordinates(station)

  n <- nrow(df)
  res <- data.frame(
    station   = rep(as.character(station), n),
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

  # Optionally remove rows with NA values
  if (del_na == "yes") {
    res <- na.omit(res)
  }

  # Adjust output format if "reduced" is requested
  if (output_format == "reduced") {
    # Keep only date, prec, evap, tmax, tmin
    res <- res[, c("date", "prec", "evap", "tmax", "tmin")]
  }

  return(res)
}
