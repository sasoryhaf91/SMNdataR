#' Download Daily Station Data with Format Option
#'
#' Downloads the raw daily data for a given station, filters the data by the specified date range,
#' and extracts the station's coordinates from its metadata. The function returns a data frame in one of
#' two formats:
#'
#' \describe{
#'   \item{"full"}{Returns a data frame with columns: station, latitude, longitude, altitude, date,
#'                 prec, evap, tmax, and tmin. (Default)}
#'   \item{"reduced"}{Returns a data frame with only the daily climatic data: date, prec, evap,
#'                    tmax, and tmin.}
#' }
#'
#' @param station Station code (character or numeric).
#' @param start_date Start date for data download (default "1961-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param output_format Output format: either "full" (default) or "reduced".
#'
#' @return A data frame with daily data in the specified format.
#'
#' @examples
#' \dontrun{
#'   # Download full format data for station "15101"
#'   df_full <- smndata_download_station("15101",
#'                                       start_date = "2020-01-01",
#'                                       end_date = "2020-12-31",
#'                                       output_format = "full")
#'
#'   # Download reduced format data for station "15101"
#'   df_reduced <- smndata_download_station("15101",
#'                                          start_date = "2020-01-01",
#'                                          end_date = "2020-12-31",
#'                                          output_format = "reduced")
#' }
#'
#' @export
smndata_download_station <- function(station,
                                     start_date = "1961-01-01",
                                     end_date = "2023-12-31",
                                     output_format = c("full", "reduced")) {
  output_format <- match.arg(output_format)

  # Download raw daily data using the optimized raw download function
  df <- tryCatch({
    smndata_download_station_raw(station)
  }, error = function(e) {
    stop("Error downloading raw data for station ", station, ": ", conditionMessage(e))
  })

  # Ensure the Date column is in Date format and filter by the specified date range
  df$date <- as.Date(df$date)
  df <- df[df$date >= as.Date(start_date) & df$date <= as.Date(end_date), ]

  if (nrow(df) == 0) {
    warning("No data available for station ", station, " in the specified date range.")
    return(data.frame())
  }

  # Extract station coordinates using the helper function
  coords <- smndata_extract_coordinates(station)

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
  res <- na.omit(res)

  # Adjust output format if "reduced" is requested
  if (output_format == "reduced") {
    res <- res %>% dplyr::select(date, prec, evap, tmax, tmin)
  }

  return(res)
}
