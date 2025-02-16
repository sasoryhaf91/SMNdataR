#' Obtener URL de Datos de una Estación
#'
#' Construye la URL para descargar los datos de una estación a partir de su código.
#'
#' La función extrae un identificador de la estación (quitando los tres últimos caracteres)
#' y utiliza un vector de mapeo para determinar la abreviatura del estado correspondiente.
#' Además, define el prefijo "dia0" para identificadores de un solo dígito y "dia" para los demás.
#'
#' @param estacion Código de la estación (puede ser numérico o carácter).
#' @return Una cadena de texto con la URL completa para descargar los datos.
#' @export
obtener_url <- function(estacion) {
  # Convertir el código de la estación a carácter
  estacion <- as.character(estacion)

  # Definir la URL base
  urlbase <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/"

  # Extraer el identificador (quitando los 3 últimos caracteres)
  id <- substr(estacion, 1, nchar(estacion) - 3)

  # Vector de mapeo de IDs a abreviaturas de estado
  state_mapping <- c(
    "1" = "ags", "2" = "bc", "3" = "bcs", "4" = "camp",
    "5" = "coah", "6" = "col", "7" = "chis", "8" = "chih",
    "9" = "df", "10" = "dgo", "11" = "gto", "12" = "gro",
    "13" = "hgo", "14" = "jal", "15" = "mex", "16" = "mich",
    "17" = "mor", "18" = "nay", "19" = "nl", "20" = "oax",
    "21" = "pue", "22" = "qro", "23" = "qroo", "24" = "slp",
    "25" = "sin", "26" = "son", "27" = "tab", "28" = "tamps",
    "29" = "tlax", "30" = "ver", "31" = "yuc", "32" ="zac"
  )

  # Determinar el estado usando el mapeo; si no se encuentra, usar "zac" por defecto
  estado <- state_mapping[[id]]
  if (is.null(estado)) estado <- "zac"

  # Seleccionar el prefijo: "dia0" para un solo dígito, "dia" para más de uno
  prefix <- if (nchar(id) == 1) "dia0" else "dia"

  # Construir y devolver la URL completa
  url_completa <- paste0(urlbase, estado, "/", prefix, estacion, ".txt")
  return(url_completa)
}
