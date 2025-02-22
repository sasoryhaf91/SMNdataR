#' Get Station Data URL
#'
#' Constructs the full URL for downloading data for a given station based on its code.
#'
#' The function extracts an identifier from the station code (by removing the last three characters)
#' and uses a mapping vector to determine the corresponding state abbreviation. It then assigns the prefix
#' "dia0" if the identifier has one character, or "dia" otherwise, and builds the URL.
#'
#' @param station Station code (character or numeric).
#' @return A character string with the complete URL to download the station data.
#' @examples
#' # Example: Get the URL for station "15101"
#' url <- smndata_get_url("15101")
#' print(url)
#' @export
smndata_get_url <- function(station) {
  # Ensure the station code is a character string
  station <- as.character(station)

  # Define the base URL
  url_base <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/"

  # Extract the identifier (remove the last three characters)
  id <- substr(station, 1, nchar(station) - 3)

  # Mapping vector for state abbreviations
  state_mapping <- c(
    "1" = "ags", "2" = "bc", "3" = "bcs", "4" = "camp",
    "5" = "coah", "6" = "col", "7" = "chis", "8" = "chih",
    "9" = "df", "10" = "dgo", "11" = "gto", "12" = "gro",
    "13" = "hgo", "14" = "jal", "15" = "mex", "16" = "mich",
    "17" = "mor", "18" = "nay", "19" = "nl", "20" = "oax",
    "21" = "pue", "22" = "qro", "23" = "qroo", "24" = "slp",
    "25" = "sin", "26" = "son", "27" = "tab", "28" = "tamps",
    "29" = "tlax", "30" = "ver", "31" = "yuc", "32" = "zac"
  )

  # Determine the state abbreviation; default to "zac" if not found
  state <- state_mapping[[id]]
  if (is.null(state)) state <- "zac"

  # Determine the prefix: "dia0" for single-digit identifiers, "dia" otherwise
  prefix <- if (nchar(id) == 1) "dia0" else "dia"

  # Construct and return the complete URL
  url_complete <- paste0(url_base, state, "/", prefix, station, ".txt")
  return(url_complete)
}
