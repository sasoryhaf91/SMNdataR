#' Download and Store Daily Station Data with Optimized DuckDB Queries
#'
#' This function retrieves raw daily data for a given meteorological station,
#' filters it based on the specified date range, and extracts station coordinates.
#' The function returns a data frame and allows efficient storage and querying via DuckDB.
#'
#' @param station Character or numeric. The station code.
#' @param start_date Character in "YYYY-MM-DD" format. Start date for data retrieval.
#' @param end_date Character in "YYYY-MM-DD" format. End date for data retrieval.
#' @param output_format Character. Output format: `"full"` (default) or `"reduced"`.
#' @param save_format Character. Storage format: `"none"` (default, no saving), `"csv"`, or `"duckdb"`.
#' @param file_path Character. File path for saving data if `save_format` is `"csv"` or `"duckdb"`. Default is `NULL`.
#' @param query_duckdb Logical. If `TRUE`, retrieves data directly from an existing DuckDB database.
#'
#' @return A data frame with daily climatic data in the specified format.
#'
#' @examples
#' \dontrun{
#'   # Save station data in DuckDB
#'   smndata_download_station("15101",
#'                            start_date = "2020-01-01",
#'                            end_date = "2020-12-31",
#'                            save_format = "duckdb",
#'                            file_path = "stations_db")
#'
#'   # Query data efficiently from DuckDB without re-downloading
#'   df_duckdb <- smndata_download_station("15101",
#'                                         start_date = "2020-01-01",
#'                                         end_date = "2020-12-31",
#'                                         file_path = "stations_db",
#'                                         query_duckdb = TRUE)
#' }
#'
#' @export
smndata_download_station_db <- function(station,
                                     start_date = "1961-01-01",
                                     end_date = Sys.Date(),
                                     output_format = c("full", "reduced"),
                                     save_format = c("none", "csv", "duckdb"),
                                     file_path = NULL,
                                     query_duckdb = FALSE) {

  output_format <- match.arg(output_format)
  save_format <- match.arg(save_format)

  # Function to check and install required packages
  ensure_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }

  # Ensure required packages are installed
  ensure_package("dplyr")
  ensure_package("readr")

  if (save_format == "duckdb" || query_duckdb) {
    ensure_package("duckdb")
  }

  # Validate file path requirement
  if (save_format != "none" && is.null(file_path)) {
    stop("A 'file_path' must be provided when saving data.")
  }

  # Query from DuckDB if requested
  if (query_duckdb) {
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = paste0(file_path, ".duckdb"))

    query <- sprintf(
      "SELECT * FROM station_data WHERE station = '%s' AND date BETWEEN '%s' AND '%s'",
      station, start_date, end_date
    )

    res <- duckdb::dbGetQuery(con, query)
    duckdb::dbDisconnect(con, shutdown = TRUE)

    if (output_format == "reduced") {
      res <- res %>% dplyr::select(date, prec, evap, tmax, tmin)
    }

    return(res)
  }

  # Download raw station data if not querying from DuckDB
  df <- tryCatch({
    smndata_download_station_raw(station)
  }, error = function(e) {
    stop("Error retrieving data for station ", station, ": ", conditionMessage(e))
  })

  # Convert date column to Date format and filter by date range
  df$date <- as.Date(df$date)
  df <- df[df$date >= as.Date(start_date) & df$date <= as.Date(end_date), ]

  if (nrow(df) == 0) {
    warning("No data available for station ", station, " in the specified date range.")
    return(data.frame())
  }

  # Extract station coordinates
  coords <- smndata_extract_coordinates(station)

  # Construct the final data frame
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

  # Remove rows with missing values
  res <- na.omit(res)

  # Adjust output format if "reduced" is selected
  if (output_format == "reduced") {
    res <- res %>% dplyr::select(date, prec, evap, tmax, tmin)
  }

  # Save in CSV format if requested
  if (save_format == "csv") {
    readr::write_csv(res, paste0(file_path, ".csv"))
    message("Data saved as CSV: ", file_path, ".csv")
  }

  # Save in DuckDB format if requested
  if (save_format == "duckdb") {
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = paste0(file_path, ".duckdb"))

    # Create table only if it does not exist
    duckdb::dbExecute(con, "CREATE TABLE IF NOT EXISTS station_data (
                    station TEXT, latitude DOUBLE, longitude DOUBLE, altitude DOUBLE,
                    date DATE, prec DOUBLE, evap DOUBLE, tmax DOUBLE, tmin DOUBLE)")

    # Insert new data
    duckdb::dbWriteTable(con, "station_data", res, append = TRUE)

    # Create an index to speed up queries
    duckdb::dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_station_date ON station_data (station, date)")

    duckdb::dbDisconnect(con, shutdown = TRUE)
    message("Data saved in DuckDB format: ", file_path, ".duckdb")
  }

  return(res)
}
