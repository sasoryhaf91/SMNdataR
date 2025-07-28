#' Construct Station Data URL
#'
#' Constructs the full URL for downloading data for a given station based on its code.
#'
#' @param station Station code (character or numeric).
#' @return A character string representing the URL for the station data.
#' @examples
#' url <- smndata_get_url("15101")
#' print(url)
#' @export
smndata_get_url <- function(station) {
  station <- as.character(station)
  url_base <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/"
  # Remove the last 3 characters to get the identifier.
  id <- substr(station, 1, nchar(station) - 3)
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
  state <- state_mapping[[id]]
  if (is.null(state)) state <- "zac"
  prefix <- if (nchar(id) == 1) "dia0" else "dia"
  url_complete <- paste0(url_base, state, "/", prefix, station, ".txt")
  return(url_complete)
}
