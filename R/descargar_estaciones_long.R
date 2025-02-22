#' Descargar datos de múltiples estaciones en formato largo filtrados por fecha
#'
#' Descarga los datos diarios de cada estación del vector proporcionado utilizando la función
#' optimizada \code{des_df_xy} (que no admite parámetros de fecha) y, posteriormente, filtra la
#' información para conservar únicamente las filas cuyo valor en la columna \code{Fecha} se encuentre
#' dentro del rango especificado (fecha_inicio a fecha_final). Para cada estación se reintenta la descarga
#' utilizando \code{manejar_error} hasta alcanzar el número máximo de intentos (parámetro \code{intentos}).
#'
#' Se muestra un mensaje en consola para cada estación descargada exitosamente y, al finalizar, se imprime
#' el tiempo total invertido en la descarga. Opcionalmente, se puede guardar el resultado en un archivo CSV.
#'
#' @param estaciones Vector con los códigos de estación (carácter o numérico).
#' @param fecha_inicio Fecha de inicio para filtrar los datos (por defecto "1961-01-01").
#' @param fecha_final Fecha final para filtrar los datos (por defecto \code{Sys.Date()}).
#' @param intentos Número máximo de intentos para descargar los datos de cada estación (por defecto 3).
#' @param csv_file (Opcional) Ruta y nombre del archivo CSV donde se guardará el data frame resultante.
#'
#' @return Data frame en formato largo con las observaciones de las estaciones, filtradas según el rango de fechas.
#'         Se incluye una columna "estacion" que identifica cada registro.
#' @export
descargar_estaciones_long <- function(estaciones,
                                      fecha_inicio = "1961-01-01",
                                      fecha_final = "2023-12-31",
                                      intentos = 3,
                                      csv_file = NULL) {
  library(dplyr)

  start_time <- Sys.time()
  results_list <- vector("list", length(estaciones))

  for (i in seq_along(estaciones)) {
    est <- estaciones[i]
    message("Iniciando descarga de la estación: ", est, " ...")

    # Descargar datos sin filtrar, ya que des_df_xy no admite parámetros de fecha
    df_est <- tryCatch(
      manejar_error(expr = des_df_xy(est), max_attempts = intentos),
      error = function(e) {
        message("Error en la estación ", est, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(df_est)) {
      # Filtrar los datos según el rango de fechas solicitado
      df_est <- df_est %>%
        filter(fecha >= as.Date(fecha_inicio) & fecha <= as.Date(fecha_final))

      if (nrow(df_est) > 0) {
        df_est <- df_est %>% mutate(estacion = as.character(est))
        results_list[[i]] <- df_est
        message("Estación ", est, " descargada exitosamente.")
      } else {
        message("Estación ", est, " no tiene datos en el rango especificado.")
        results_list[[i]] <- NULL
      }
    } else {
      results_list[[i]] <- NULL
    }

    gc()  # Liberar memoria en cada iteración
  }

  results_list <- Filter(Negate(is.null), results_list)

  if (length(results_list) == 0) {
    warning("No se descargaron datos para ninguna estación en el rango especificado.")
    return(data.frame())
  }

  final_df <- bind_rows(results_list)

  total_time <- Sys.time() - start_time
  message("Descarga completada. Tiempo total: ", round(as.numeric(total_time, units = "secs"), 2), " segundos.")

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("Archivo CSV guardado en: ", csv_file)
  }

  return(final_df)
}
