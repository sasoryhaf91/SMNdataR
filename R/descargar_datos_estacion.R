#' Descargar datos de una estación
#'
#' Descarga y procesa los datos de una estación. La función realiza lo siguiente:
#' \enumerate{
#'   \item Obtiene la URL correspondiente a la estación.
#'   \item Lee los datos utilizando \code{read.table()}.
#'   \item Identifica y rellena las fechas faltantes en la serie.
#'   \item Ordena y filtra los datos según el rango de fechas especificado.
#' }
#'
#' @param estacion Código de la estación.
#' @param fecha_inicio Fecha de inicio (por defecto "1961-01-01").
#' @param fecha_final Fecha final (por defecto "2023-12-31").
#' @return Data frame con los datos completos y filtrados por fecha.
#' @export
descargar_datos_estacion <- function(estacion, fecha_inicio = "1961-01-01", fecha_final = "2023-12-31") {
  url <- obtener_url(estacion)
  df <- read.table(url, header = FALSE, na.strings = "NULO",
                   col.names = c("Fecha", "Prec", "Evap", "Tmax", "Tmin"),
                   colClasses = c("Date", "numeric", "numeric", "numeric", "numeric"),
                   skip = 25)

  df %>%
    identificar_fechas_faltantes("Fecha", fecha_inicio, fecha_final) %>%
    dplyr::arrange(Fecha) %>%
    extraer_por_rango_fechas("Fecha", fecha_inicio, fecha_final)
}
