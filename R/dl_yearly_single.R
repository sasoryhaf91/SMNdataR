#' Download and Aggregate **Yearly** Climate Data (Reduce Format)
#'
#' Downloads **daily** data for an SMN station using
#' [smn_dl_daily_single()] and aggregates to **yearly** values for one or more
#' variables, with quality control based on an allowed fraction of missing days
#' per year. The output is always in **reduce (wide)** format: one row per year
#' with the requested variables as columns plus station metadata.
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
#'   (default `0.2`). When the missing fraction exceeds the threshold, that
#'   yearly value is set to `NA`.
#' @param aggregator Either:
#'   - `NULL` (default): uses `sum` for `"prec","evap"` and `mean` for
#'     `"tmax","tmin"`, or
#'   - a **single function** (e.g., `median`) applied to all variables, or
#'   - a **named list** mapping variables to functions,
#'     e.g. `list(prec = sum, evap = sum, tmax = mean, tmin = mean)`.
#' @param csv_file Optional file path. If non-`NULL`, the result is written to
#'   CSV (`row.names = FALSE`) and also returned.
#'
#' @return A base `data.frame` in **reduce** (wide) format with columns:
#'   `station, latitude, longitude, altitude, YEAR, <variables...>`.
#'   If no data are available, returns an empty table with the same schema.
#'
#' @details
#' Internally calls [smn_dl_daily_single()] with `output_format = "full"` to
#' fetch daily data and station metadata. The missing-data rule is computed
#' **per year and per variable** as `#(NA) / #(days in year)`.
#'
#' @examples
#' \dontrun{
#' # All variables in reduce format (default)
#' y1 <- smn_dl_yearly_single(
#'   station = "15101",
#'   start_date = "2000-01-01",
#'   end_date   = "2005-12-31",
#'   variables  = "all"
#' )
#'
#' # Subset of variables and custom aggregator
#' y2 <- smn_dl_yearly_single(
#'   station = "15101",
#'   start_date = "1990-01-01",
#'   end_date   = "1999-12-31",
#'   variables  = c("prec","tmax"),
#'   aggregator = list(prec = sum, tmax = median)
#' )
#' }
#'
#' @export
smn_dl_yearly_single <- function(station,
                                 start_date = "1961-01-01",
                                 end_date   = Sys.Date(),
                                 variables = "all",
                                 max_missing_frac = 0.2,
                                 aggregator = NULL,
                                 csv_file = NULL) {
  # -- resolve variable set ----------------------------------------------------
  all_vars <- c("prec","evap","tmax","tmin")
  if (identical(tolower(variables), "all")) variables <- all_vars
  variables <- intersect(all_vars, variables)
  if (!length(variables)) {
    stop("`variables` must include at least one of: ", paste(all_vars, collapse = ", "))
  }

  # -- fetch daily data with metadata -----------------------------------------
  daily <- tryCatch(
    smn_dl_daily_single(
      station     = station,
      start_date  = start_date,
      end_date    = end_date,
      output_format = "full",
      del_na        = "no"
    ),
    error = function(e) {
      warning("Failed to obtain daily data for station ", station, ": ", conditionMessage(e))
      return(data.frame())
    }
  )

  # Build empty reduce schema (used for early return when no data)
  empty_reduce <- {
    df <- data.frame(
      station   = character(),
      latitude  = numeric(),
      longitude = numeric(),
      altitude  = numeric(),
      YEAR      = integer(),
      check.names = FALSE
    )
    for (v in variables) df[[v]] <- numeric()
    df
  }

  if (!nrow(daily)) return(empty_reduce)

  # -- ensure date and requested variables exist ------------------------------
  if (!inherits(daily$date, "Date")) daily$date <- as.Date(daily$date)
  for (v in setdiff(variables, names(daily))) daily[[v]] <- NA_real_

  # -- metadata (first row; tolerated as NA if absent) ------------------------
  meta <- list(
    station   = if ("station" %in% names(daily))   daily$station[1]   else as.character(station),
    latitude  = if ("latitude" %in% names(daily))  daily$latitude[1]  else NA_real_,
    longitude = if ("longitude" %in% names(daily)) daily$longitude[1] else NA_real_,
    altitude  = if ("altitude" %in% names(daily))  daily$altitude[1]  else NA_real_
  )

  # -- select aggregator per variable -----------------------------------------
  default_agg <- list(prec = sum, evap = sum, tmax = mean, tmin = mean)
  agg_fun <- function(var) {
    if (is.null(aggregator)) return(default_agg[[var]])
    if (is.function(aggregator)) return(aggregator)
    if (is.list(aggregator) && !is.null(aggregator[[var]]) && is.function(aggregator[[var]])) {
      return(aggregator[[var]])
    }
    default_agg[[var]]  # fallback
  }

  # -- yearly aggregation with QC ---------------------------------------------
  YEAR <- lubridate::year(daily$date)

  build_one <- function(var) {
    df <- data.frame(YEAR = YEAR, value = daily[[var]])
    agg <- dplyr::summarise(
      dplyr::group_by(df, YEAR),
      total_days   = dplyr::n(),
      missing_days = sum(is.na(value)),
      value        = agg_fun(var)(value, na.rm = TRUE),
      .groups = "drop"
    )
    agg$missing_frac <- with(agg, ifelse(total_days > 0, missing_days / total_days, NA_real_))
    agg$value[agg$missing_frac > max_missing_frac] <- NA_real_
    names(agg)[names(agg) == "value"] <- var
    agg[, c("YEAR", var), drop = FALSE]
  }

  parts <- lapply(variables, build_one)

  # full outer join by YEAR across variables to keep union of years
  yearly <- Reduce(function(x, y) dplyr::full_join(x, y, by = "YEAR"), parts)
  yearly <- yearly[order(yearly$YEAR), , drop = FALSE]

  # ensure all requested variables are present as columns
  for (v in setdiff(variables, names(yearly))) yearly[[v]] <- NA_real_

  # -- assemble reduce output --------------------------------------------------
  out <- cbind(
    station   = meta$station,
    latitude  = meta$latitude,
    longitude = meta$longitude,
    altitude  = meta$altitude,
    yearly[, c("YEAR", variables), drop = FALSE],
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL

  # optional CSV
  if (!is.null(csv_file)) {
    utils::write.csv(out, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  out
}
