#' Resumen de datos faltantes configurable para un bloque de estaciones con mensajes de progreso
#'
#' Para cada estación en el vector proporcionado, esta función descarga los datos diarios
#' en el rango especificado y calcula el resumen de valores faltantes para la variable
#' indicada. Puedes elegir entre los siguientes tipos de resumen:
#'
#' \describe{
#'   \item{overall}{Suma total de NA para toda la serie. (por defecto)}
#'   \item{monthly}{Faltantes por mes (se generan columnas de [variable]_M01 a [variable]_M12).}
#'   \item{yearly}{Faltantes por año (se generan columnas de [variable]_[AÑO]).}
#'   \item{doy}{Faltantes por día del año (se generan columnas de [variable]_D001 a [variable]_D366).}
#' }
#'
#' Por defecto, se calcula el resumen "overall" para la variable "Prec" (precipitación).
#' La función muestra en la consola un mensaje para cada estación procesada y, al final,
#' el tiempo total de descarga.
#'
#' @param estaciones Vector con los códigos de estación (carácter o numérico).
#' @param fecha_inicio Fecha de inicio para la descarga de datos (por defecto "1961-01-01").
#' @param fecha_final Fecha final para la descarga de datos (por defecto "2023-12-31").
#' @param intentos Número máximo de intentos para descargar los datos de cada estación (por defecto 3).
#' @param variable Nombre de la variable a analizar (por defecto "Prec").
#' @param resumen Tipo de resumen a calcular; opciones: "overall", "monthly", "yearly", "doy". Por defecto "overall".
#' @return Un data frame con una fila por estación y columnas según el tipo de resumen seleccionado.
#' @export
des_bloq_falt_all <- function(estaciones,
                              fecha_inicio = "1961-01-01",
                              fecha_final = "2023-12-31",
                              intentos = 3,
                              variable = "Prec",
                              resumen = c("overall", "monthly", "yearly", "doy")) {
  resumen <- match.arg(resumen)
  library(dplyr)
  library(tidyr)
  library(lubridate)

  start_time <- Sys.time()  # Inicia la medición del tiempo

  results <- lapply(estaciones, function(est) {
    df <- tryCatch(
      manejar_error(
        expr = descargar_datos_estacion(est, fecha_inicio, fecha_final),
        max_attempts = intentos
      ),
      error = function(e) {
        message("No se pudo obtener datos para la estación ", est, ": ", e$message)
        return(NULL)
      }
    )
    if (is.null(df)) return(NULL)

    if (resumen == "overall") {
      total_missing <- sum(is.na(df[[variable]]))
      res_df <- data.frame(CLAVE = as.character(est),
                           missing_total = total_missing,
                           stringsAsFactors = FALSE)
      colnames(res_df)[2] <- paste0(variable, "_total")
    } else if (resumen == "monthly") {
      df_month <- df %>% mutate(MES = month(Fecha))
      monthly <- df_month %>%
        group_by(MES) %>%
        summarise(missing = sum(is.na(.data[[variable]])), .groups = "drop") %>%
        complete(MES = 1:12, fill = list(missing = 0))
      res_df <- pivot_wider(monthly,
                            names_from = MES,
                            values_from = missing,
                            names_prefix = paste0(variable, "_M"),
                            values_fill = list(missing = 0))
      res_df <- cbind(CLAVE = as.character(est), res_df)
    } else if (resumen == "yearly") {
      df_year <- df %>% mutate(AÑO = year(Fecha))
      yearly <- df_year %>%
        group_by(AÑO) %>%
        summarise(missing = sum(is.na(.data[[variable]])), .groups = "drop")
      res_df <- pivot_wider(yearly,
                            names_from = AÑO,
                            values_from = missing,
                            names_prefix = paste0(variable, "_"),
                            values_fill = list(missing = 0))
      res_df <- cbind(CLAVE = as.character(est), res_df)
    } else if (resumen == "doy") {
      df_doy <- df %>% mutate(DOY = yday(Fecha))
      doy <- df_doy %>%
        group_by(DOY) %>%
        summarise(missing = sum(is.na(.data[[variable]])), .groups = "drop") %>%
        complete(DOY = 1:366, fill = list(missing = 0))
      res_df <- pivot_wider(doy,
                            names_from = DOY,
                            values_from = missing,
                            names_prefix = paste0(variable, "_D"),
                            values_fill = list(missing = 0))
      res_df <- cbind(CLAVE = as.character(est), res_df)
    }
    message("Estación ", est, " descargado.")
    return(res_df)
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) {
    return(data.frame())
  }

  final_df <- do.call(rbind, results)

  total_time <- Sys.time() - start_time
  message("Tiempo total de descarga: ", round(as.numeric(total_time, units = "secs"), 2), " segundos")

  return(final_df)
}
