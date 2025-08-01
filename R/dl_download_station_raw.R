#' Download Raw Daily Data for a Station (Resilient via readLines)
#'
#' Downloads and parses the raw daily data file from SMN using the internal URL
#' and `readLines()`, similar to how metadata is retrieved.
#'
#' @param station Station code (character or numeric).
#'
#' @return A data frame with columns: date, prec, evap, tmax, tmin.
#'
#' @export
smn_dl_download_station_raw <- function(station) {
  station <- as.character(station)
  url <- smn_int_get_url(station)

  # Leer líneas con manejo de error robusto
  lines <- smn_int_handle_error({
    readLines(url)
  }, max_attempts = 10)

  # Validar contenido
  if (is.null(lines) || length(lines) < 26) {
    stop("Archivo de datos vacío o mal formado para estación ", station)
  }

  # Extraer solo las líneas de datos (después de la cabecera, usualmente 25)
  data_lines <- lines[-(1:25)]

  # Leer como tabla a partir de las líneas
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

