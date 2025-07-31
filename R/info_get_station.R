#' Get Station Metadata from Internal Package Data
#'
#' Retrieves available metadata for a given station ID from the `estaciones` dataset
#' stored in the `data/` folder of the package.
#'
#' @param station_id A numeric or character code representing the station ID.
#'
#' @return A data frame with the metadata of the station, or an empty data frame if not found.
#'
#' @examples
#' smn_info_get_station(7116)
#' smn_info_get_station("30129")
#'
#' @export
smn_info_get_station <- function(station_id) {
  # Cargar la base de datos desde el entorno del paquete
  if (!exists("stations", where = .GlobalEnv)) {
    data("stations", package = "SMNdataR", envir = environment())
  }

  # Convertir station_id a carácter y eliminar decimales innecesarios
  station_id <- as.character(as.integer(station_id))

  # Verificar si la estación está presente
  if (!"station" %in% names(stations)) {
    stop("The 'stations' dataset does not contain a 'station' column.")
  }

  result <- dplyr::filter(stations, as.character(station) == station_id)

  if (nrow(result) == 0) {
    message("No information found for station ID: ", station_id)
    return(data.frame())
  }

  return(result)
}
