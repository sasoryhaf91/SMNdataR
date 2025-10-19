#' Download Daily NASA POWER (MERRA-2) Data for a Named Point
#'
#' Downloads daily NASA POWER variables for a specific point (lat/lon) and tags
#' the result with a user-defined `station` (node) name so you can join/trace later.
#'
#' @param station Character. A label or node name to tag the time series (e.g., "Toluca_Node").
#' @param lat Numeric. Latitude in decimal degrees (WGS84).
#' @param lon Numeric. Longitude in decimal degrees (WGS84).
#' @param altitude Numeric (optional). Altitude of the point (m a.s.l.). Default `NA_real_`.
#' @param start_date Start date (YYYY-MM-DD). If earlier than 1984-01-01, it is
#'   automatically adjusted to `1984-01-01` (POWER coverage).
#' @param end_date End date (YYYY-MM-DD). Default `Sys.Date()`.
#' @param vars Character vector of POWER parameter short names. Defaults to a compact
#'   set: `c("T2M_MAX","T2M_MIN","RH2M","PRECTOTCORR","EVLAND")`.
#' @param community POWER community. Default `"AG"`. (Common alternatives: `"SSE"`, `"RE"`)
#' @param retries Integer. Max GET retries on failure (default 5).
#' @param sleep_sec Numeric. Seconds to sleep between retries (default 3).
#' @param output_format Either `"all"` (default) or `"reduce"`. The `"reduce"` option
#'   keeps only `station, latitude, longitude, altitude, date` and the requested variables.
#'
#' @return A `data.frame` ordered by `date` with columns:
#'   `station, latitude, longitude, altitude, date, <requested POWER variables>`.
#'   POWER missing values `-999` are returned as `NA`.
#'
#' @examples
#' \dontrun{
#' nasa_df <- smn_dl_daily_nasa(
#'   station = "Toluca_Node",
#'   lat = 19.289, lon = -99.657, altitude = 2670,
#'   start_date = "2020-01-01", end_date = "2020-12-31",
#'   vars = c("T2M_MAX","T2M_MIN","PRECTOTCORR"),
#'   output_format = "reduce"
#' )
#' }
#' @export
smn_dl_daily_nasa <- function(station,
                              lat,
                              lon,
                              altitude = NA_real_,
                              start_date = "1984-01-01",
                              end_date   = Sys.Date(),
                              vars = c("T2M_MAX","T2M_MIN","RH2M","PRECTOTCORR","EVLAND"),
                              community = "AG",
                              retries = 5,
                              sleep_sec = 3,
                              output_format = c("all","reduce")) {

  output_format <- match.arg(output_format)

  if (missing(station) || !nzchar(as.character(station)))
    stop("`station` (node name) is required.")
  if (missing(lat) || missing(lon) || any(is.na(c(lat,lon))))
    stop("Provide valid `lat` and `lon`.")

  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  cov_start  <- as.Date("1984-01-01")
  if (start_date < cov_start) {
    message("Adjusting start_date to NASA POWER coverage (1984-01-01).")
    start_date <- cov_start
  }
  if (end_date < start_date) stop("`end_date` must be on or after `start_date`.")

  date_start <- format(start_date, "%Y%m%d")
  date_end   <- format(end_date,   "%Y%m%d")

  params_qs <- if (length(vars) && all(nzchar(vars))) {
    paste0("parameters=", paste(vars, collapse = ","), "&")
  } else {
    ""
  }

  base_url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  url <- paste0(
    base_url, "?", params_qs,
    "start=", date_start,
    "&end=", date_end,
    "&latitude=", sprintf("%.6f", as.numeric(lat)),
    "&longitude=", sprintf("%.6f", as.numeric(lon)),
    "&community=", utils::URLencode(community, reserved = TRUE),
    "&format=JSON"
  )

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.", call. = FALSE)
  }

  attempts <- 0; resp <- NULL
  while (is.null(resp) && attempts < retries) {
    attempts <- attempts + 1
    resp <- tryCatch({
      r <- httr::GET(url)
      if (httr::status_code(r) != 200) stop("Status ", httr::status_code(r))
      r
    }, error = function(e) {
      if (attempts < retries) Sys.sleep(sleep_sec)
      NULL
    })
  }
  if (is.null(resp)) {
    warning("NASA POWER API request failed after ", retries, " attempts. URL: ", url)
    return(data.frame())
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if (is.null(parsed$properties$parameter) || length(parsed$properties$parameter) == 0) {
    warning("No parameter payload returned by POWER. URL: ", url)
    return(data.frame())
  }
  param_data <- parsed$properties$parameter

  first_var <- names(param_data)[1]
  if (is.null(first_var) || length(param_data[[first_var]]) == 0) {
    warning("POWER returned empty series for the requested period. URL: ", url)
    return(data.frame())
  }
  date_keys <- names(param_data[[first_var]])

  rows <- lapply(date_keys, function(k) {
    out <- list(date = as.Date(k, "%Y%m%d"))
    for (v in names(param_data)) {
      val <- param_data[[v]][[k]]
      out[[v]] <- if (is.null(val) || is.na(val) || val == -999) NA_real_ else as.numeric(val)
    }
    out
  })

  # build the data frame
  if (requireNamespace("dplyr", quietly = TRUE)) {
    df <- dplyr::bind_rows(rows)
  } else {
    df <- do.call(rbind, lapply(rows, as.data.frame))
  }

  # metadata
  df$station   <- as.character(station)
  df$latitude  <- as.numeric(lat)
  df$longitude <- as.numeric(lon)
  df$altitude  <- as.numeric(altitude)
  df$source_url <- url

  # colocar metadatos delante y ordenar
  front <- c("station","latitude","longitude","altitude","date")
  df <- cbind(df[front], df[setdiff(names(df), front)], stringsAsFactors = FALSE)

  if (output_format == "reduce") {
    keep <- unique(c("station","latitude","longitude","altitude","date", intersect(colnames(df), vars)))
    df <- df[, keep, drop = FALSE]
  }

  df <- df[order(df$date), , drop = FALSE]
  rownames(df) <- NULL
  df
}
