#' Descargar datos de una estación e informar sobre datos faltantes
#'
#' Descarga y procesa los datos de una estación en un rango de fechas determinado,
#' completando las fechas faltantes y ordenando la información. Posteriormente, muestra
#' un resumen en consola del número de datos faltantes para cada variable (precipitación,
#' evaporación, temperatura máxima y mínima).
#'
#' @param estacion Código de la estación.
#' @param fecha_inicio Fecha de inicio (por defecto "1961-01-01").
#' @param fecha_final Fecha final (por defecto "2023-12-31").
#' @return Data frame con los datos completos de la estación.
#' @export
descargar_datos_estacion_info <- function(estacion,
                                          fecha_inicio = "1961-01-01",
                                          fecha_final = "2023-12-31") {
  # Descargar y procesar los datos utilizando la función ya optimizada
  df <- descargar_datos_estacion(estacion, fecha_inicio, fecha_final)

  # Calcular la cantidad de valores faltantes en cada columna
  falt <- colSums(is.na(df))

  # Convertir las fechas de entrada a Date para formatearlas correctamente
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_final  <- as.Date(fecha_final)

  # Mostrar información en consola
  message("Estación: ", estacion)
  message("Fecha de datos: ", format(fecha_inicio, "%d/%m/%Y"), " -- ", format(fecha_final, "%d/%m/%Y"))
  message("Variables descargadas: 4")
  message("Datos faltantes:")
  message("  Precipitación: ", falt["Prec"], " - Evaporación: ", falt["Evap"])
  message("  Temperatura Máxima: ", falt["Tmax"], " - Temperatura Mínima: ", falt["Tmin"])

  return(df)
}
