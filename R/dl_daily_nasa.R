#' Download and Merge Daily SMN Data with NASA POWER Variables
#'
#' Downloads daily data for a given point and merges SMN daily data (if a station is provided)
#' with NASA POWER daily variables by date. Call it in two ways:
#'   (1) Provide `station` (coords are resolved internally), or
#'   (2) Provide `lat` and `lon` directly (NASA-only; SMN step is skipped).
#'
#' @param station Station code (character or numeric). Optional if `lat` and `lon` are provided.
#' @param lat Numeric latitude (decimal degrees). Optional if `station` is provided.
#' @param lon Numeric longitude (decimal degrees). Optional if `station` is provided.
#' @param start_date Start date (default "1981-01-01"). Will be adjusted to 1984-01-01 for NASA POWER.
#' @param end_date End date (default \code{Sys.Date()}).
#' @param output_format Output format: either "all" (default) or "reduce".
#'
#' @return A data frame with a `date` column and NASA POWER variables; if `station` is provided
#'         and SMN data exists, returns SMN+NASA merged by date. When NASA-only, includes
#'         columns `station`, `latitude`, `longitude`.
#'
#' @examples
#' \dontrun{
#' # Mode 1: station (SMN + NASA)
#' merged_df <- smn_dl_daily_nasa(
#'   station = "15101",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-12-31",
#'   output_format = "all"
#' )
#'
#' # Mode 2: lat/lon directly (NASA only)
#' nasa_only <- smn_dl_daily_nasa(
#'   lat = 19.4326, lon = -99.1332,
#'   start_date = "2020-01-01",
#'   end_date   = "2020-12-31",
#'   output_format = "reduce"
#' )
#' }
#' @export
smn_dl_daily_nasa <- function(station = NULL,
                              lat = NULL,
                              lon = NULL,
                              start_date = "1981-01-01",
                              end_date   = Sys.Date(),
                              output_format = c("all", "reduce")) {

  output_format <- match.arg(output_format)

  # Imports (kept as in your original)
  library(httr)
  library(dplyr)
  library(jsonlite)

  # ---- Dates / POWER coverage ----
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (start_date < as.Date("1984-01-01")) {
    message("Adjusting start_date to NASA POWER coverage (1984-01-01).")
    start_date <- as.Date("1984-01-01")
  }
  if (end_date < start_date) stop("`end_date` must be on or after `start_date`.")

  # ---- Resolve point ----
  have_station <- !is.null(station) && !is.na(station) && nzchar(as.character(station))
  have_point   <- !is.null(lat) && !is.null(lon) && !is.na(lat) && !is.na(lon)
  station_chr  <- if (have_station) as.character(station) else NULL

  if (have_station) {
    crd <- tryCatch(smn_int_extract_coordinates(station_chr), error = function(e) NULL)
    if (is.null(crd) || any(is.na(crd))) {
      if (!have_point) {
        stop("Coordinates not found for station ", station_chr, " and no lat/lon provided as fallback.")
      }
      warning("Using provided lat/lon as fallback for station ", station_chr, ".")
      lat_res <- as.numeric(lat)
      lon_res <- as.numeric(lon)
    } else {
      lat_res <- as.numeric(crd$latitude)
      lon_res <- as.numeric(crd$longitude)
    }
    station_label <- station_chr
  } else if (have_point) {
    lat_res <- as.numeric(lat)
    lon_res <- as.numeric(lon)
    station_label <- sprintf("POINT_%.5f_%.5f", lat_res, lon_res)
  } else {
    stop("Provide either `station` OR both `lat` and `lon`.")
  }

  # ---- SMN (only when station) ----
  smn_data <- NULL
  if (have_station) {
    smn_data <- tryCatch(
      smn_dl_daily_single(station_label, start_date, end_date),
      error = function(e) { warning("SMN download failed: ", conditionMessage(e)); NULL }
    )
    if (is.data.frame(smn_data) && nrow(smn_data) > 0L) {
      if (!"date" %in% names(smn_data)) {
        warning("SMN data has no 'date' column; skipping SMN merge.")
        smn_data <- NULL
      } else {
        if (!inherits(smn_data$date, "Date")) smn_data$date <- as.Date(as.character(smn_data$date))
        if (!"station" %in% names(smn_data)) smn_data$station <- station_label
        smn_data$station <- as.character(smn_data$station)
      }
    } else {
      warning("No SMN data available for station ", station_label, " in the specified range. Returning NASA only.")
      smn_data <- NULL
    }
  }

  # ---- NASA POWER request ----
  date_start <- format(start_date, "%Y%m%d")
  date_end   <- format(end_date,   "%Y%m%d")

  # Daily variables (extend as needed)
  #vars <- c("T2M_MAX","T2M_MIN","RH2M","PRECTOTCORR","WS2M","PS","ALLSKY_SFC_SW_DWN","QV2M","EVLAND")
  vars <- c("T2M_MAX","T2M_MIN","RH2M","PRECTOTCORR","EVLAND")
  url <- paste0(
    "https://power.larc.nasa.gov/api/temporal/daily/point?parameters=",
    paste(vars, collapse = ","),
    "&start=", date_start,
    "&end=", date_end,
    "&latitude=", lat_res,
    "&longitude=", lon_res,
    "&community=AG&format=JSON"
  )

  resp <- NULL; attempts <- 0
  while (is.null(resp) && attempts < 5) {
    attempts <- attempts + 1
    message("Attempt ", attempts, ": requesting NASA POWER data...")
    resp <- tryCatch({
      r <- httr::GET(url)
      if (httr::status_code(r) != 200) stop("Status ", httr::status_code(r))
      r
    }, error = function(e) {
      message("Attempt ", attempts, " failed: ", conditionMessage(e))
      Sys.sleep(3)
      NULL
    })
  }

  if (is.null(resp) || httr::status_code(resp) != 200) {
    warning("NASA POWER API request failed. Status: ",
            if (!is.null(resp)) httr::status_code(resp) else "no response")
    if (is.data.frame(smn_data) && nrow(smn_data) > 0L) return(smn_data)
    return(data.frame())
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  param_data <- parsed$properties$parameter

  any_var   <- names(param_data)[1]
  date_keys <- names(param_data[[any_var]])

  nasa_df <- dplyr::bind_rows(lapply(date_keys, function(k) {
    row <- data.frame(date = as.Date(k, format = "%Y%m%d"))
    for (v in names(param_data)) {
      value <- param_data[[v]][[k]]
      row[[v]] <- ifelse(is.null(value) || value == -999, NA, value)
    }
    row
  })) %>%
    mutate(
      station   = station_label,
      latitude  = lat_res,
      longitude = lon_res
    )

  # ---- Merge (no duplicate .x/.y and keep join keys) ----
  if (is.data.frame(smn_data) && nrow(smn_data) > 0L) {
    # Keys first
    join_keys <- if ("station" %in% names(smn_data)) c("date","station") else "date"

    # Ensure same types
    nasa_df$station <- as.character(nasa_df$station)

    # Drop NASA metadata except keys (prevents .x/.y)
    meta_cols <- c("station","latitude","longitude","altitude")
    to_drop   <- setdiff(intersect(names(nasa_df), meta_cols), join_keys)
    nasa_df   <- nasa_df[, setdiff(names(nasa_df), to_drop), drop = FALSE]

    out <- dplyr::left_join(smn_data, nasa_df, by = join_keys)
  } else {
    out <- nasa_df
  }

  # ---- Output format ----
  if (output_format == "reduce") {
    keep <- c("date","station","latitude","longitude",
              intersect(c("prec","tmax","tmin","evap"), names(out)),
              intersect(vars, names(out)))
    keep <- unique(keep); keep <- keep[keep %in% names(out)]
    out  <- out[, keep, drop = FALSE]
  }

  # ---- Order & return ----
  if ("date" %in% names(out)) {
    out <- out[order(out$date), , drop = FALSE]
    rownames(out) <- NULL
  }

  return(out)
}
