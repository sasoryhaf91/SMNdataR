#' Descargar coordenadas de múltiples estaciones con salida detallada y tiempo total
#'
#' Esta función descarga las coordenadas (latitud, longitud y altitud) para un conjunto de
#' estaciones utilizando internamente la función \code{extraer_coordenadas}. Se implementa
#' manejo de errores con \code{manejar_error} y se imprime un mensaje por cada estación descargada,
#' así como el tiempo total invertido en la descarga.
#'
#' @param estaciones Vector con los códigos de estación (carácter o numérico).
#' @param intentos Número máximo de intentos para cada estación (por defecto, 3).
#' @return Data frame con las columnas:
#' \describe{
#'   \item{Estacion}{Código de la estación}
#'   \item{Latitud}{Latitud de la estación (numérico)}
#'   \item{Longitud}{Longitud de la estación (numérico)}
#'   \item{Altitud}{Altitud de la estación (numérico)}
#' }
#' @export
descargar_coordenadas_bloque <- function(estaciones, intentos = 3) {
  start_time <- Sys.time()
  results_list <- list()

  for (est in estaciones) {
    coord <- tryCatch(
      manejar_error(
        expr = extraer_coordenadas(est),
        max_attempts = intentos
      ),
      error = function(e) {
        message("No se pudo obtener coordenadas de la estación '", est, "': ", e$message)
        return(NULL)
      }
    )

    if (!is.null(coord)) {
      results_list[[as.character(est)]] <- coord
      message("Estación ", est, " descargada: Latitud = ", coord$Latitud,
              ", Longitud = ", coord$Longitud, ", Altitud = ", coord$Altitud)
    }
  }

  total_time <- Sys.time() - start_time
  message("Tiempo total de descarga: ", round(as.numeric(total_time, units = "secs"), 2), " segundos")

  if (length(results_list) == 0) {
    return(data.frame(
      Estacion = character(0),
      Latitud  = numeric(0),
      Longitud = numeric(0),
      Altitud  = numeric(0)
    ))
  }

  coords_df <- do.call(rbind, results_list)
  return(coords_df)
}

