#' Download Raw Daily Data for a Station
#'
#' Downloads and parses the raw daily data file from SMN using the internal URL
#' and `readLines()`, similar to how metadata is retrieved.
#'
#' @param station Station code (character or numeric).
#'
#' @return A data frame with columns: date, prec, evap, tmax, tmin.
#'
#' @export
smn_dl_daily_raw <- function(station) {
  station <- as.character(station)
  url <- smn_int_get_url(station)

  lines <- smn_int_handle_error({
    readLines(url)
  }, max_attempts = 10)

  if (is.null(lines) || length(lines) < 26) {
    stop("Data file is empty or malformed for station ", station)
  }

  data_lines <- lines[-(1:25)]

  df <- readr::read_table2(
    file = I(data_lines),
    col_names = c("date", "prec", "evap", "tmax", "tmin"),
    col_types = readr::cols(
      date = readr::col_date(format = ""),
      prec  = readr::col_double(),
      evap  = readr::col_double(),
      tmax  = readr::col_double(),
      tmin  = readr::col_double()
    ),
    na = "NULO"
  )

  return(df)
}



