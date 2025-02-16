#' Extraer Datos por Rango de Fechas
#'
#' Filtra un data frame para incluir únicamente las filas cuyas fechas en la columna especificada
#' se encuentran dentro del rango definido por \code{fecha_inicio} y \code{fecha_final}.
#'
#' @param df Data frame que contiene la columna de fechas.
#' @param columna_fecha Cadena con el nombre de la columna que contiene las fechas.
#' @param fecha_inicio Fecha inicial (cadena o de tipo Date).
#' @param fecha_final Fecha final (cadena o de tipo Date).
#' @return Data frame filtrado que solo incluye las filas con fechas en el rango especificado.
#' @export
extraer_por_rango_fechas <- function(df, columna_fecha, fecha_inicio, fecha_final) {
  # Verificar que la columna exista en el data frame
  if (!columna_fecha %in% names(df)) {
    stop("La columna especificada no existe en el data frame.")
  }

  # Convertir la columna de fecha y los parámetros a tipo Date
  df[[columna_fecha]] <- as.Date(df[[columna_fecha]])
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_final <- as.Date(fecha_final)

  # Validar que fecha_inicio no sea mayor que fecha_final
  if (fecha_inicio > fecha_final) {
    stop("La fecha_inicio debe ser menor o igual que la fecha_final.")
  }

  # Filtrar el data frame utilizando dplyr::between y tidy evaluation
  df_filtrado <- dplyr::filter(
    df,
    dplyr::between(!!rlang::sym(columna_fecha), fecha_inicio, fecha_final)
  )

  return(df_filtrado)
}
