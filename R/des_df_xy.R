#' Extraer datos y coordenadas de una estación (sin distancias)
#'
#' Descarga los datos diarios de una estación a partir de su URL y extrae las coordenadas
#' (latitud, longitud y altitud) leyendo los metadatos asociados. Retorna un data frame que
#' combina la información climática con las coordenadas de la estación.
#'
#' @param estacion Código de la estación (numérico o carácter).
#' @return Data frame con las columnas:
#' \describe{
#'   \item{estacion}{Código de la estación.}
#'   \item{latitud}{Latitud de la estación (numérico).}
#'   \item{longitud}{Longitud de la estación (numérico).}
#'   \item{altitud}{Altitud de la estación (numérico).}
#'   \item{fecha}{Fecha del registro.}
#'   \item{tmax}{Temperatura máxima.}
#'   \item{tmin}{Temperatura mínima.}
#'   \item{evap}{Evaporación.}
#'   \item{precipitacion}{Precipitación.}
#' }
#' @export
des_df_xy <- function(estacion) {
  # Genera la URL de descarga para la estación
  urldesc <- obtener_url(estacion)

  # Descargar los datos diarios (líneas de datos comienzan después de 25 líneas de metadatos)
  df <- read.table(urldesc, header = FALSE, na.strings = "NULO",
                   col.names = c("Fecha", "Prec", "Evap", "Tmax", "Tmin"),
                   colClasses = c("Date", "numeric", "numeric", "numeric", "numeric"),
                   skip = 25)

  # Descargar los metadatos (se asume que están en las siguientes 9 líneas, a partir de la línea 10)
  df_meta <- read.table(urldesc, sep = ":", header = FALSE, nrows = 9, skip = 9)

  # Extraer las coordenadas utilizando substrings
  latitud <- as.numeric(substr(df_meta$V2[7], 2, nchar(df_meta$V2[7]) - 2))
  longitu <- as.numeric(substr(df_meta$V2[8], 2, nchar(df_meta$V2[8]) - 2))
  altitud <- as.numeric(substr(df_meta$V2[9], 2, 5))

  # Crear el data frame final con la información diaria y las coordenadas
  res <- data.frame(
    estacion      = rep(estacion, nrow(df)),
    latitud       = rep(latitud, nrow(df)),
    longitud      = rep(longitu, nrow(df)),
    altitud       = rep(altitud, nrow(df)),
    fecha         = df$Fecha,
    tmax          = df$Tmax,
    tmin          = df$Tmin,
    evap          = df$Evap,
    precipitacion = df$Prec,
    stringsAsFactors = FALSE
  )

  # Eliminar filas con datos faltantes
  res <- na.omit(res)

  return(res)
}
