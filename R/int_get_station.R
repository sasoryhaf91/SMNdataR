#' Get Metadata for a Single Station (Internal)
#'
#' @param station_id A character or numeric station ID.
#' @return A data frame with the metadata for the given station.
#' @keywords internal
smn_int_get_station <- function(station_id) {
  if (!exists("stations", where = .GlobalEnv)) {
    data("stations", package = "SMNdataR", envir = environment())
  }

  station_id <- as.character(as.integer(station_id))

  if (!"station" %in% names(stations)) {
    stop("The dataset does not contain a 'station' column.")
  }

  result <- dplyr::filter(stations, as.character(station) == station_id)

  if (nrow(result) == 0) {
    return(data.frame())
  }

  return(result)
}
