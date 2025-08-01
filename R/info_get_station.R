#' Get Station Metadata
#'
#' Retrieves station metadata for one or multiple station IDs or from a data frame.
#'
#' @param input A single station ID, a vector of IDs, or a data frame containing a 'CLAVE' column.
#'
#' @return A data frame with station metadata.
#' @export
smn_info_get_station <- function(input) {
  if (!exists("stations", where = .GlobalEnv)) {
    data("stations", package = "SMNdataR", envir = environment())
  }

  # Case 1: data.frame with station info
  if (is.data.frame(input)) {
    if ("station" %in% names(input)) {
      station_ids <- unique(input$CLAVE)
    } else {
      stop("The input data frame must contain a 'station' column.")
    }
  } else {
    # Case 2: vector or single ID
    station_ids <- as.character(as.integer(input))
  }

  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(station_ids), style = 3)
  result <- lapply(seq_along(station_ids), function(i) {
    utils::setTxtProgressBar(pb, i)
    smn_int_get_station(station_ids[i])
  })
  close(pb)

  output <- do.call(rbind, result)
  return(output)
}

