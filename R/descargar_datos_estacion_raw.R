#' Descargar datos raw de una estación
#'
#' Descarga y retorna los datos sin procesar de una estación utilizando la URL generada por
#' \code{obtener_url}. Se emplea \code{readr::read_table2} para una lectura eficiente y se definen
#' los tipos de columna apropiados. En caso de error durante la descarga o lectura, se detiene la
#' ejecución mostrando un mensaje descriptivo.
#'
#' @param estacion Código de la estación (puede ser numérico o carácter).
#' @return Data frame con las columnas: \code{Fecha}, \code{Prec}, \code{Evap}, \code{Tmax} y \code{Tmin}.
#' @export
descargar_datos_estacion_raw <- function(estacion) {
  url <- obtener_url(estacion)

  df <- tryCatch({
    readr::read_table2(
      url,
      col_names = c("Fecha", "Prec", "Evap", "Tmax", "Tmin"),
      col_types = readr::cols(
        Fecha = readr::col_date(format = ""),
        Prec  = readr::col_double(),
        Evap  = readr::col_double(),
        Tmax  = readr::col_double(),
        Tmin  = readr::col_double()
      ),
      skip = 25,
      na = "NULO"
    )
  }, error = function(e) {
    stop("Error al descargar o leer los datos: ", conditionMessage(e))
  })

  return(df)
}
