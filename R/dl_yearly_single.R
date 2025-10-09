#' Download and Aggregate Yearly Climate Data for a Single Station
#'
#' Downloads **daily** data for an SMN station using
#' [smn_dl_daily_single()] and aggregates to **yearly** values for one or more
#' variables, with quality control based on an allowed fraction of missing days
#' per year. Supports tidy (*long*) or compact (*reduce*) outputs.
#'
#' @param station Character or numeric. SMN station code (e.g., `"15101"`).
#' @param start_date Character or `Date`. Start of the daily window
#'   (default `"1961-01-01"`, inclusive).
#' @param end_date Character or `Date`. End of the daily window
#'   (default `Sys.Date()`, inclusive).
#' @param variables Character vector of variables to summarize. Any subset of
#'   `c("prec","evap","tmax","tmin")`. Use `"all"` to include all four
#'   (default `"all"`).
#' @param max_missing_frac Numeric in \[0, 1]. Maximum allowed **fraction of
#'   missing days per year** to keep that year's aggregate for a variable
#'   (default `0.2`). If the missing fraction exceeds the threshold, that
#'   variable's yearly value is set to `NA` for that year.
#' @param aggregator Either:
#'   - `NULL` (default): uses `sum` for `"prec","evap"` and `mean` for
#'     `"tmax","tmin"`, or
#'   - a **single function** (e.g., `median`) applied to all variables, or
#'   - a **named list** mapping variables to functions,
#'     e.g. `list(prec = sum, evap = sum, tmax = mean, tmin = mean)`.
#' @param output_format One of `c("long","reduce")`. In `"long"`, returns one
#'   row per `YEAR` and `variable` with columns:
#'   `station, latitude, longitude, altitude, YEAR, variable, value`.
#'   In `"reduce"`, returns one row per `YEAR` with the requested variables as
#'   columns (e.g., `prec, evap, tmax, tmin`) plus the metadata columns
#'   `station, latitude, longitude, altitude`.
#' @param csv_file Optional file path. If non-`NULL`, the result is written to
#'   CSV (`row.names = FALSE`) and also returned.
#'
#' @return A base `data.frame` with yearly aggregates in the selected format.
#'   If no data are available, returns an empty table with the appropriate schema.
#'
#' @details
#' Internally this function calls:
#' - [smn_dl_daily_single()] with `output_format = "full"` to fetch daily data
#'   and station metadata (`station, latitude, longitude, altitude`).
#'
#' **Missing-data rule:** the missing fraction is computed per year and per
#' variable as `#(NA) / #(days in that year window)`. If that fraction exceeds
#' `max_missing_frac`, the yearly value for that variable is set to `NA`.
#'
#' @examples
#' \dontrun{
#' # All variables, tidy output
#' y1 <- smn_dl_yearly_single("15101",
#'         start_date = "2000-01-01", end_date = "2005-12-31",
#'         variables = "all", output_format = "long")
#'
#' # Only precipitation and evaporation, compact output
#' y2 <- smn_dl_yearly_single("15101",
#'         start_date = "1990-01-01", end_date = "1999-12-31",
#'         variables = c("prec","evap"), output_format = "reduce")
#'
#' # Custom aggregator for all vars
#' y3 <- smn_dl_yearly_single("15101",
#'         start_date = "2010-01-01", end_date = "2020-12-31",
#'         variables = "all", aggregator = median, output_format = "long")
#' }
#'
#' @export
smn_dl_yearly_single <- function(station,
                                 start_date = "1961-01-01",
                                 end_date   = Sys.Date(),
                                 variables = "all",
                                 max_missing_frac = 0.2,
                                 aggregator = NULL,
                                 output_format = c("long","reduce"),
                                 csv_file = NULL) {
  output_format <- match.arg(output_format)

  # -- resolve variable set ----------------------------------------------------
  all_vars <- c("prec","evap","tmax","tmin")
  if (identical(tolower(variables), "all")) variables <- all_vars
  variables <- intersect(all_vars, variables)
  if (!length(variables)) {
    stop("`variables` must include at least one of: ", paste(all_vars, collapse = ", "))
  }

  # -- fetch daily with metadata -----------------------------------------------
  daily <- tryCatch(
    smn_dl_daily_single(station,
                        start_date = start_date,
                        end_date   = end_date,
                        output_format = "full",
                        del_na = "no"),
    error = function(e) {
      warning("Failed to obtain daily data for station ", station, ": ", conditionMessage(e))
      return(data.frame())
    }
  )

  # empty schemas depending on output
  empty_long <- data.frame(
    station   = character(),
    latitude  = numeric(),
    longitude = numeric(),
    altitude  = numeric(),
    YEAR      = integer(),
    variable  = character(),
    value     = numeric(),
    check.names = FALSE
  )
  empty_reduce <- data.frame(
    station   = character(),
    latitude  = numeric(),
    longitude = numeric(),
    altitude  = numeric(),
    YEAR      = integer(),
    check.names = FALSE
  )
  for (v in variables) empty_reduce[[v]] <- numeric()

  if (!nrow(daily)) {
    return(if (output_format == "long") empty_long else empty_reduce)
  }

  # -- ensure date and needed columns -----------------------------------------
  if (!inherits(daily$date, "Date")) daily$date <- as.Date(daily$date)
  # if any requested variable is missing, create NA column to keep schema
  for (v in setdiff(variables, names(daily))) daily[[v]] <- NA_real_

  # -- metadata (constant across rows) ----------------------------------------
  meta <- list(
    station   = if ("station" %in% names(daily))   daily$station[1]   else as.character(station),
    latitude  = if ("latitude" %in% names(daily))  daily$latitude[1]  else NA_real_,
    longitude = if ("longitude" %in% names(daily)) daily$longitude[1] else NA_real_,
    altitude  = if ("altitude" %in% names(daily))  daily$altitude[1]  else NA_real_
  )

  # -- choose aggregator per variable -----------------------------------------
  default_agg <- list(prec = sum, evap = sum, tmax = mean, tmin = mean)
  agg_fun <- function(var) {
    if (is.null(aggregator)) return(default_agg[[var]])
    if (is.function(aggregator)) return(aggregator)
    if (is.list(aggregator) && !is.null(aggregator[[var]]) && is.function(aggregator[[var]])) {
      return(aggregator[[var]])
    }
    # fallback sensible default
    default_agg[[var]]
  }

  # -- compute YEAR and aggregate ---------------------------------------------
  YEAR <- lubridate::year(daily$date)

  # build per-variable yearly table with missing-fraction QC
  build_one <- function(var) {
    df <- data.frame(YEAR = YEAR, value = daily[[var]])
    # n days and missing fraction per year
    agg <- dplyr::summarise(
      dplyr::group_by(df, YEAR),
      total_days   = dplyr::n(),
      missing_days = sum(is.na(value)),
      value        = agg_fun(var)(value, na.rm = TRUE),
      .groups = "drop"
    )
    agg$missing_frac <- with(agg, ifelse(total_days > 0, missing_days / total_days, NA_real_))
    agg$value[agg$missing_frac > max_missing_frac] <- NA_real_

    agg$variable <- var
    agg
  }

  parts <- lapply(variables, build_one)
  yearly_long <- dplyr::bind_rows(parts)

  if (!nrow(yearly_long)) {
    return(if (output_format == "long") empty_long else empty_reduce)
  }

  # -- attach metadata and format ---------------------------------------------
  if (output_format == "long") {
    out <- yearly_long[, c("YEAR","variable","value"), drop = FALSE]
    out <- cbind(
      station   = meta$station,
      latitude  = meta$latitude,
      longitude = meta$longitude,
      altitude  = meta$altitude,
      out,
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
  } else {
    # reduce: one row per YEAR, variables as columns + metadata
    wide <- tidyr::pivot_wider(
      yearly_long[, c("YEAR","variable","value"), drop = FALSE],
      names_from = "variable", values_from = "value"
    )
    # ensure all requested variables exist as columns (even if absent in data)
    for (v in setdiff(variables, names(wide))) wide[[v]] <- NA_real_
    # order columns: meta, YEAR, variables...
    wide <- wide[order(wide$YEAR), , drop = FALSE]
    wide <- wide[, c("YEAR", variables), drop = FALSE]

    out <- cbind(
      station   = meta$station,
      latitude  = meta$latitude,
      longitude = meta$longitude,
      altitude  = meta$altitude,
      wide,
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
  }

  # optional CSV
  if (!is.null(csv_file)) {
    utils::write.csv(out, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  out
}
