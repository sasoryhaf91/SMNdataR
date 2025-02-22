#' Resumen de datos faltantes para un bloque de estaciones
#'
#' Para cada estación del vector proporcionado, esta función descarga los datos y
#' calcula el resumen de valores faltantes (conteo de NA) para las variables: Precipitación,
#' Evaporación, Temperatura Máxima y Temperatura Mínima. Los resultados se combinan en un data frame.
#'
#' @param estaciones Vector con los códigos de estación (carácter o numérico).
#' @param fecha_inicio Fecha de inicio para la descarga de datos (por defecto "1961-01-01").
#' @param fecha_final Fecha final para la descarga de datos (por defecto "2023-12-31").
#' @param intentos Número máximo de intentos para descargar los datos de cada estación (por defecto 3).
#' @return Data frame con las columnas:
#' \describe{
#'   \item{CLAVE}{Código de la estación.}
#'   \item{Prec}{Cantidad de valores faltantes en la variable de precipitación.}
#'   \item{Evap}{Cantidad de valores faltantes en la variable de evaporación.}
#'   \item{Tmax}{Cantidad de valores faltantes en la variable de temperatura máxima.}
#'   \item{Tmin}{Cantidad de valores faltantes en la variable de temperatura mínima.}
#' }
#' @export
des_bloq_falt <- function(estaciones,
                          fecha_inicio = "1961-01-01",
                          fecha_final = "2023-12-31",
                          intentos = 3) {

  resultados <- lapply(estaciones, function(est) {
    # Intentar obtener el resumen de datos faltantes para la estación
    falt <- tryCatch(
      manejar_error(
        expr = des_est_falt(est, fecha_inicio, fecha_final),
        max_attempts = intentos
      ),
      error = function(e) {
        message("No se pudo obtener datos faltantes para la estación ", est, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(falt)) {
      # Se asume que 'des_est_falt' retorna un vector nombrado con las variables: Prec, Evap, Tmax, Tmin.
      data.frame(
        CLAVE = as.character(est),
        Prec  = falt["Prec"],
        Evap  = falt["Evap"],
        Tmax  = falt["Tmax"],
        Tmin  = falt["Tmin"],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  # Filtrar estaciones que fallaron (resultados nulos)
  resultados <- Filter(Negate(is.null), resultados)

  # Si no se obtuvo ningún resultado, retorna un data frame vacío con la estructura esperada
  if (length(resultados) == 0) {
    return(data.frame(
      CLAVE = character(),
      Prec  = numeric(),
      Evap  = numeric(),
      Tmax  = numeric(),
      Tmin  = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Combinar todos los data frames en uno solo
  df_resultado <- do.call(rbind, resultados)

  return(df_resultado)
}
