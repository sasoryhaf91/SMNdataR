#' Extraer coordenadas de una estación
#'
#' Lee el archivo de metadatos de la estación (a partir de la URL generada por \code{obtener_url}),
#' y extrae la latitud, longitud y altitud. Devuelve un \code{data.frame} con una fila y las columnas:
#' \code{Estacion}, \code{Latitud}, \code{Longitud}, \code{Altitud}.
#'
#' @param estacion Código de la estación (carácter o numérico).
#' @return Data frame con una fila y las columnas:
#' \describe{
#'   \item{Estacion}{Código de la estación}
#'   \item{Latitud}{Latitud de la estación (numérico)}
#'   \item{Longitud}{Longitud de la estación (numérico)}
#'   \item{Altitud}{Altitud de la estación (numérico)}
#' }
#' @export
extraer_coordenadas <- function(estacion) {
  # Construir la URL
  url <- obtener_url(estacion)

  # Leer 9 líneas a partir de la línea 10 (skip = 9) donde se ubican los metadatos
  coords_df <- tryCatch({
    utils::read.table(
      file = url,
      sep = ":",
      header = FALSE,
      nrows = 9,
      skip = 9,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    stop("Error al leer la URL para la estación ", estacion, ": ", conditionMessage(e))
  })

  # Verificar que se hayan leído al menos 9 filas
  if (nrow(coords_df) < 9) {
    stop("No se encontraron las líneas esperadas para extraer las coordenadas de la estación ", estacion, ".")
  }

  # Función auxiliar para extraer el valor numérico de la cadena
  parse_coord <- function(text) {
    # Elimina caracteres que no sean dígitos, punto o signo
    numeric_text <- gsub("[^0-9\\.-]", "", text)
    as.numeric(numeric_text)
  }

  # Extraer latitud, longitud y altitud
  latitud  <- parse_coord(coords_df$V2[7])
  longitud <- parse_coord(coords_df$V2[8])
  altitud  <- parse_coord(coords_df$V2[9])

  # Construir el resultado
  data.frame(
    Estacion = estacion,
    Latitud  = latitud,
    Longitud = longitud,
    Altitud  = altitud
  )
}
