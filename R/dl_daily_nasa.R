#' Download and Merge Daily SMN Data with NASA POWER Variables
#'
#' Downloads daily SMN data for a given station within the specified date range and
#' merges it with daily NASA POWER data using latitude and longitude of the station.
#' The NASA POWER variables include multiple atmospheric and radiation variables.
#' These variables are returned in a single data frame joined by date.
#'
#' @param station Station code (character or numeric).
#' @param start_date Start date for data download (default "1981-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param output_format Output format: either "all" (default) or "reduce".
#'
#' @return A data frame with the merged SMN and NASA POWER variables.
#'         "all" format includes all variables, "reduce" only adds NASA POWER variables.
#'
#' @examples
#' \dontrun{
#'   merged_df <- smn_dl_daily_nasa("15101",
#'                                  start_date = "2020-01-01",
#'                                  end_date = "2020-12-31",
#'                                  output_format = "all")
#'   head(merged_df)
#' }
#' @export
smn_dl_daily_nasa <- function(station,
                              start_date = "1981-01-01",
                              end_date = Sys.Date(),
                              output_format = c("all", "reduce")) {

  output_format <- match.arg(output_format)
  library(httr)
  library(dplyr)
  library(jsonlite)

  # Validar formato de fechas
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (start_date < as.Date("1984-01-01")) {
    message("Adjusting start_date to NASA POWER coverage (1984-01-01).")
    start_date <- as.Date("1984-01-01")
  }

  # Obtener coordenadas
  coords <- smn_int_extract_coordinates(station)
  if (is.null(coords) || any(is.na(coords))) {
    stop("Coordinates not found for station ", station)
  }
  lat <- coords$latitude
  lon <- coords$longitude

  # Descargar datos SMN
  smn_data <- smn_dl_daily_single(station, start_date, end_date)
  if (nrow(smn_data) == 0) {
    warning("No SMN data available for station ", station)
    return(data.frame())
  }

  # Formatear fechas para la API
  date_start <- format(start_date, "%Y%m%d")
  date_end <- format(end_date, "%Y%m%d")

  # Lista filtrada de variables válidas en NASA POWER
  vars <- c("T2M_MAX", "T2M_MIN", "RH2M", "PRECTOTCORR", "WS2M",
            "PS", "ALLSKY_SFC_SW_DWN",
            "QV2M")
  var_string <- paste(vars, collapse = ",")
  url <- paste0("https://power.larc.nasa.gov/api/temporal/daily/point?parameters=",
                var_string,
                "&start=", date_start,
                "&end=", date_end,
                "&latitude=", lat,
                "&longitude=", lon,
                "&community=AG&format=JSON")

  # Llamada GET directa con reintento
  resp <- NULL
  attempts <- 0
  while (is.null(resp) && attempts < 5) {
    attempts <- attempts + 1
    message("Attempt ", attempts, ": requesting NASA POWER data...")
    resp <- tryCatch({
      r <- httr::GET(url)
      if (httr::status_code(r) != 200) stop("Status ", httr::status_code(r))
      r
    }, error = function(e) {
      message("Attempt ", attempts, " failed: ", conditionMessage(e))
      Sys.sleep(5)
      NULL
    })
  }

  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning("NASA POWER API request failed. Status code: ", httr::status_code(resp))
    return(smn_data)
  }

  # Parsear JSON y extraer datos
  content_json <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content_json, flatten = TRUE)
  param_data <- parsed$properties$parameter

  nasa_df <- bind_rows(lapply(names(param_data[[1]]), function(date) {
    row <- data.frame(date = as.Date(date, format = "%Y%m%d"))
    for (var in names(param_data)) {
      value <- param_data[[var]][[date]]
      row[[var]] <- ifelse(is.null(value) || value == -999, NA, value)
    }
    row
  }))

  # Unir con SMN
  merged <- left_join(smn_data, nasa_df, by = "date")

  if (output_format == "reduce") {
    merged <- merged %>% select(date, all_of(vars))
  }

  return(merged)
}
