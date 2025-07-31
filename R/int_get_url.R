#' Construct URL to Download Station Data
#'
#' Internal function to construct the full URL for downloading climate data from the SMN website.
#'
#' @param station Character or numeric code of the station (e.g. "15101").
#'
#' @return A character string with the full URL to access the station data.
#' @noRd
smn_int_get_url <- function(station) {
  station <- as.character(station)
  url_base <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/"

  # Extract state code (first digits minus last three)
  id <- substr(station, 1, nchar(station) - 3)

  state_mapping <- c(
    "1" = "ags",  "2" = "bc",   "3" = "bcs",  "4" = "camp", "5" = "coah",
    "6" = "col",  "7" = "chis", "8" = "chih", "9" = "df",   "10" = "dgo",
    "11" = "gto", "12" = "gro", "13" = "hgo", "14" = "jal", "15" = "mex",
    "16" = "mich","17" = "mor", "18" = "nay", "19" = "nl",  "20" = "oax",
    "21" = "pue", "22" = "qro", "23" = "qroo","24" = "slp", "25" = "sin",
    "26" = "son", "27" = "tab", "28" = "tamps","29" = "tlax","30" = "ver",
    "31" = "yuc", "32" = "zac"
  )

  state <- state_mapping[[id]]
  if (is.null(state)) state <- "zac"  # fallback

  prefix <- if (nchar(id) == 1) "dia0" else "dia"
  url_complete <- paste0(url_base, state, "/", prefix, station, ".txt")

  return(url_complete)
}

