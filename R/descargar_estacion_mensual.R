#' Descargar datos mensuales de una estación
#'
#' Esta función descarga los datos diarios de la estación especificada y los
#' agrega a nivel mensual, retornando un data frame con las columnas:
#' \code{AÑO}, \code{ENE}, \code{FEB}, \code{MAR}, \code{ABR}, \code{MAY},
#' \code{JUN}, \code{JUL}, \code{AGO}, \code{SEP}, \code{OCT}, \code{NOV},
#' \code{DIC}, \code{ACUM}, \code{PROM} y \code{MESES}.
#'
#' @param estacion Código de la estación (carácter o numérico).
#' @param fecha_inicio Fecha inicial (por defecto "1961-01-01").
#' @param fecha_final Fecha final (por defecto "2023-12-31").
#' @return Un data frame con la precipitación mensual por año y columnas
#' adicionales para el acumulado anual (\code{ACUM}), el promedio mensual
#' (\code{PROM}) y la cantidad de meses con datos (\code{MESES}).
#' @export
descargar_estacion_mensual <- function(estacion,
                                       fecha_inicio = "1961-01-01",
                                       fecha_final = "2023-12-31") {
  # Asegúrate de tener cargados los paquetes que usas en tu paquete:
  # dplyr, tidyr, lubridate
  # (Si estás en un paquete, esto va en Imports y se hace con el namespace: dplyr::, tidyr::, etc.)

  # 1. Descarga los datos diarios de la estación
  df_diario <- descargar_datos_estacion(estacion, fecha_inicio, fecha_final)

  # Vector con nombres de meses en español
  meses <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
             "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")

  # 2. Agregar precipitación por mes y reestructurar la tabla
  df_mensual <- df_diario %>%
    dplyr::mutate(AÑO = lubridate::year(Fecha),
                  MES = lubridate::month(Fecha)) %>%
    dplyr::group_by(AÑO, MES) %>%
    # Sumamos la precipitación (Prec) mensual
    dplyr::summarise(Prec = sum(Prec, na.rm = TRUE), .groups = "drop") %>%
    # Convertimos a formato ancho: cada mes será una columna
    tidyr::pivot_wider(names_from = MES, values_from = Prec) %>%
    # Renombramos las columnas 1..12 con ENE..DIC
    dplyr::rename_at(dplyr::vars(as.character(1:12)), ~ meses) %>%
    # Creamos columnas adicionales:
    # ACUM (acumulado anual), PROM (promedio), MESES (meses con datos)
    dplyr::mutate(
      ACUM = rowSums(dplyr::across(dplyr::all_of(meses)), na.rm = TRUE),
      PROM = rowMeans(dplyr::across(dplyr::all_of(meses)), na.rm = TRUE),
      MESES = rowSums(!is.na(dplyr::across(dplyr::all_of(meses))))
    ) %>%
    dplyr::arrange(AÑO)

  # Retornamos el data frame mensual
  return(df_mensual)
}
