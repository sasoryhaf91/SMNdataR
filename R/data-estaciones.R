#' Lista de estaciones disponibles en el SMN
#'
#' Este dataset contiene la información básica de las estaciones disponibles en el Servicio Meteorológico Nacional (SMN).
#'
#' @format Un data frame con N filas y M variables:
#' \describe{
#'   \item{codigo}{Código de la estación.}
#'   \item{nombre}{Nombre o descripción de la estación.}
#'   \item{estado}{Estado donde se encuentra la estación.}
#'   \item{latitud}{Latitud de la estación.}
#'   \item{longitud}{Longitud de la estación.}
#'   \item{altitud}{Altitud de la estación.}
#'   ...
#' }
#' @source \url{https://smn.conagua.gob.mx/} (o la fuente original de los datos)
"estaciones"
