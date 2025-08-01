#' Extract Coordinates for Multiple Stations
#'
#' Iterates over a vector of station codes and retrieves metadata coordinates
#' using \code{\link{smn_int_extract_coordinates}}. If extraction fails for any station,
#' the row will be filled with \code{NA} values.
#'
#' @param stations A character or numeric vector of station codes.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{station}{Station code.}
#'   \item{latitude}{Station latitude (numeric).}
#'   \item{longitude}{Station longitude (numeric).}
#'   \item{altitude}{Station altitude (numeric).}
#' }
#'
#' @seealso \code{\link{smn_int_extract_coordinates}}
#'
#' @examples
#' \dontrun{
#' stations <- c(15020, 15171, 12345)  # 12345 likely fails
#' coords_df <- smn_info_extract_coordinates(stations)
#' head(coords_df)
#' }
#'
#' @export
smn_info_extract_coordinates <- function(stations) {
  stations <- as.character(stations)

  pb <- txtProgressBar(min = 0, max = length(stations), style = 3)

  results <- vector("list", length(stations))

  for (i in seq_along(stations)) {
    st <- stations[i]

    coords <- tryCatch(
      smn_int_extract_coordinates(st),
      error = function(e) {
        warning("Failed to extract data for station ", st, ": ", conditionMessage(e))
        return(data.frame(latitude = NA, longitude = NA, altitude = NA))
      }
    )

    results[[i]] <- data.frame(
      station = st,
      latitude = coords$latitude,
      longitude = coords$longitude,
      altitude = coords$altitude,
      stringsAsFactors = FALSE
    )

    setTxtProgressBar(pb, i)
  }

  close(pb)
  return(do.call(rbind, results))
}

