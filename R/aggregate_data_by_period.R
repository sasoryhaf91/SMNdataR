#' Agregar Datos por Período
#'
#' Agrega los datos diarios a nivel mensual o anual utilizando una función de agregación.
#'
#' @param datos Data frame que contiene la columna de fecha y variables numéricas.
#' @param fecha_col Cadena con el nombre de la columna de fecha (por defecto "Fecha").
#' @param period Período de agregación: "monthly" o "annual" (por defecto "monthly").
#' @param fun Función de agregación (por defecto, mean).
#' @param na.rm Valor lógico; si es TRUE se omiten los NA en la agregación (por defecto TRUE).
#' @return Data frame con los datos agregados por período.
#' @export
aggregate_data_by_period <- function(datos, fecha_col = "Fecha", period = c("monthly", "annual"), fun = sum, na.rm = TRUE) {
  period <- match.arg(period)

  # Verificar que la columna de fecha exista
  if (!fecha_col %in% names(datos)) {
    stop("La columna de fecha especificada no existe en el data frame.")
  }

  fecha_sym <- rlang::sym(fecha_col)

  # Convertir la columna de fecha a Date sin modificar el data frame original
  datos <- datos %>%
    dplyr::mutate(!!fecha_sym := as.Date(!!fecha_sym))

  if (period == "monthly") {
    aggregated <- datos %>%
      dplyr::mutate(
        year = lubridate::year(!!fecha_sym),
        month = lubridate::month(!!fecha_sym)
      ) %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise(across(where(is.numeric), ~ fun(.x, na.rm = na.rm)), .groups = "drop")
  } else {
    aggregated <- datos %>%
      dplyr::mutate(year = lubridate::year(!!fecha_sym)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(across(where(is.numeric), ~ fun(.x, na.rm = na.rm)), .groups = "drop")
  }

  return(aggregated)
}
