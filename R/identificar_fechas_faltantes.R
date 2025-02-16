#' Identificar Fechas Faltantes en una Serie de Datos
#'
#' Completa un data frame de datos agregando filas para las fechas que no están registradas,
#' generando una secuencia completa de fechas entre \code{fecha_inicio} y \code{fecha_final}.
#'
#' Si el data frame está vacío, se retornará un data frame que contiene únicamente la secuencia
#' de fechas. En ese caso, es obligatorio especificar valores válidos para \code{fecha_inicio} y \code{fecha_final}.
#'
#' @param datos Data frame que contiene una columna de fechas.
#' @param columna_fecha Cadena con el nombre de la columna que contiene las fechas.
#' @param fecha_inicio Fecha de inicio. Por defecto se utiliza \code{min(datos[[columna_fecha]], na.rm = TRUE)}.
#' @param fecha_final Fecha final. Por defecto se utiliza \code{max(datos[[columna_fecha]], na.rm = TRUE)}.
#' @return Data frame con la columna de fechas completada; las filas correspondientes a las fechas faltantes
#' tendrán \code{NA} en las demás variables.
#' @export
identificar_fechas_faltantes <- function(datos, columna_fecha,
                                         fecha_inicio = min(datos[[columna_fecha]], na.rm = TRUE),
                                         fecha_final = max(datos[[columna_fecha]], na.rm = TRUE)) {
  # Convertir a tibble para trabajar de forma consistente con tidyverse
  datos <- dplyr::as_tibble(datos)

  # Convertir la columna de fecha a Date (si no lo es)
  datos[[columna_fecha]] <- as.Date(datos[[columna_fecha]])

  # Si el data frame está vacío, asegurarse de que fecha_inicio y fecha_final sean válidos
  if (nrow(datos) == 0) {
    if (is.null(fecha_inicio) || is.null(fecha_final) ||
        is.infinite(fecha_inicio) || is.infinite(fecha_final)) {
      stop("El data frame está vacío y no se han especificado valores válidos para fecha_inicio y fecha_final.")
    }
    return(tibble::tibble(!!rlang::sym(columna_fecha) := seq(as.Date(fecha_inicio), as.Date(fecha_final), by = "day")))
  }

  # Convertir fecha_inicio y fecha_final a Date
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_final <- as.Date(fecha_final)

  # Completar la secuencia de fechas utilizando tidyr::complete
  datos_completos <- tidyr::complete(
    datos,
    !!rlang::sym(columna_fecha) := seq(fecha_inicio, fecha_final, by = "day")
  )

  return(datos_completos)
}
