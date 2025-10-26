#' Construct URL to Download Station Data (internal)
#'
#' Builds the absolute URL for the SMN raw daily text file of a given station.
#' The state "slug" is inferred from the leading digits of the station code
#' (all but the last three), and the file path is composed as:
#' `.../Normales_Climatologicas/Diarios/<state>/<prefix><station>.txt`,
#' where `<prefix>` is `"dia0"` for 1-digit state ids and `"dia"` otherwise.
#'
#' @param station Character or numeric code of the station (e.g., "15101").
#'
#' @return A length-1 character with the full URL to access the station data.
#'
#' @keywords internal
#' @noRd
smn_int_get_url <- function(station) {
  # ---- validate --------------------------------------------------------------
  if (missing(station) || !nzchar(as.character(station))) {
    stop("`station` must be a non-empty station code.")
  }
  station <- as.character(station)

  if (!grepl("^[0-9]+$", station)) {
    stop("`station` must contain digits only (e.g., '15101').")
  }
  if (nchar(station) < 4L) {
    stop("`station` looks too short: expected at least 4 digits (got '", station, "').")
  }

  # ---- constants -------------------------------------------------------------
  url_base <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/"

  # Extract state code (leading digits minus last three)
  id <- substr(station, 1L, nchar(station) - 3L)

  state_mapping <- c(
    "1"  = "ags",  "2"  = "bc",   "3"  = "bcs",  "4"  = "camp", "5"  = "coah",
    "6"  = "col",  "7"  = "chis", "8"  = "chih", "9"  = "df",   "10" = "dgo",
    "11" = "gto",  "12" = "gro",  "13" = "hgo",  "14" = "jal",  "15" = "mex",
    "16" = "mich", "17" = "mor",  "18" = "nay",  "19" = "nl",   "20" = "oax",
    "21" = "pue",  "22" = "qro",  "23" = "qroo", "24" = "slp",  "25" = "sin",
    "26" = "son",  "27" = "tab",  "28" = "tamps","29" = "tlax", "30" = "ver",
    "31" = "yuc",  "32" = "zac"
  )

  state <- state_mapping[[id]]
  if (is.null(state)) {
    stop("Unknown state id parsed from `station`: ", id,
         ". Expected 1-2 digit state code followed by a 3-digit station id.")
  }


  # 'dia0' for 1-digit state ids; 'dia' otherwise
  prefix <- if (nchar(id) == 1L) "dia0" else "dia"

  # Compose URL
  paste0(url_base, state, "/", prefix, station, ".txt")
}


