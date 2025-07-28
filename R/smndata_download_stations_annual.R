#' Download and Summarize Annual Station Data for a Single Variable (Multiple Stations)
#'
#' Downloads daily data for one or more stations over a specified date range, aggregates the data
#' at an annual level for a chosen variable, and returns the result in one of two formats.
#'
#' For variables "prec" or "evap", annual values are aggregated by summing daily data;
#' for "tmax" or "tmin", they are aggregated by taking the mean (or a custom aggregator if provided).
#' If a year's fraction of missing daily data exceeds \code{max_missing_frac} (default 0.2),
#' that year's aggregated value is set to NA.
#'
#' The output format can be specified as follows:
#'
#' \describe{
#'   \item{"all"}{Tidy (long) format: one row per station-year with columns:
#'               station, latitude, longitude, altitude, year, and the aggregated variable value.}
#'   \item{"reduce"}{Wide format: one row per year, with one column per station–variable combination
#'                   named "est{station}{variable}" (e.g., "est1041prec").}
#' }
#'
#' Optionally, the final data frame can be saved as a CSV file.
#'
#' @param stations A vector of station codes (character or numeric).
#' @param variable A character string specifying the variable to summarize (e.g., "prec", "evap", "tmax", or "tmin").
#'        For "prec" or "evap", annual values are summed; for "tmax" or "tmin", they are averaged.
#' @param start_date Start date for daily data download (default "1961-01-01").
#' @param end_date End date for daily data download (default "2023-12-31").
#' @param max_missing_frac Maximum fraction of missing daily data allowed per year (default 0.2).
#'        If a year's missing fraction exceeds this threshold, that year's aggregated value is set to NA.
#' @param output_format Output format: either "all" (tidy/long format) or "reduce" (wide format).
#' @param aggregator (Optional) A custom function for annual aggregation (e.g., sum or mean).
#'        If NULL, the function automatically chooses sum for "prec"/"evap" and mean for "tmax"/"tmin".
#' @param csv_file (Optional) File path to save the resulting data frame as CSV. If NULL, no CSV is written.
#'
#' @return A data frame with annual-aggregated data in the chosen format.
#'
#' @examples
#' \dontrun{
#'   # Example 1: Summarize annual precipitation data for stations "1041" and "15101" in tidy format.
#'   df_long <- smndata_download_stations_annual(
#'     stations = c("1041", "15101"),
#'     variable = "prec",
#'     start_date = "1980-01-01",
#'     end_date = "2000-12-31",
#'     output_format = "all"
#'   )
#'   head(df_long)
#'
#'   # Example 2: Summarize annual maximum temperature data for stations "1041" and "15101" in wide format.
#'   df_wide <- smndata_download_stations_annual(
#'     stations = c("1041", "15101"),
#'     variable = "tmax",
#'     start_date = "1980-01-01",
#'     end_date = "2000-12-31",
#'     output_format = "reduce"
#'   )
#'   head(df_wide)
#'
#'   # Example 3: Use a custom aggregator (e.g., median) for precipitation and save the result to CSV.
#'   df_custom <- smndata_download_stations_annual(
#'     stations = c("1041", "15101"),
#'     variable = "prec",
#'     aggregator = median,
#'     csv_file = "annual_prec_custom.csv"
#'   )
#' }
#'
#' @export
smndata_download_stations_annual <- function(stations,
                                             variable,
                                             start_date = "1961-01-01",
                                             end_date = "2023-12-31",
                                             max_missing_frac = 0.2,
                                             output_format = c("all", "reduce"),
                                             aggregator = NULL,
                                             csv_file = NULL) {
  output_format <- match.arg(output_format)
  library(dplyr)
  library(tidyr)
  library(lubridate)

  results_list <- vector("list", length(stations))

  for (i in seq_along(stations)) {
    st <- stations[i]
    message("Processing station: ", st)

    # Download daily data for the station.
    df_daily <- smndata_download_station(st, start_date, end_date)
    if (nrow(df_daily) == 0) {
      message("No daily data for station ", st)
      results_list[[i]] <- NULL
      next
    }
    if (!variable %in% names(df_daily)) {
      message("Variable '", variable, "' not found for station ", st)
      results_list[[i]] <- NULL
      next
    }

    df_daily$date <- as.Date(df_daily$date)

    # Aggregate daily data to annual data.
    df_annual <- df_daily %>%
      mutate(YEAR = year(date)) %>%
      group_by(YEAR) %>%
      summarise(
        total_days = n(),
        missing_days = sum(is.na(.data[[variable]])),
        annual_val = if (is.null(aggregator)) {
          if (variable %in% c("prec", "evap")) sum(.data[[variable]], na.rm = TRUE)
          else if (variable %in% c("tmax", "tmin")) mean(.data[[variable]], na.rm = TRUE)
          else mean(.data[[variable]], na.rm = TRUE)
        } else {
          aggregator(.data[[variable]], na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      # Optionally, you could complete the years in the range if needed.
      mutate(
        missing_frac = ifelse(total_days > 0, missing_days / total_days, NA),
        annual_val = ifelse(missing_frac > max_missing_frac, NA, annual_val),
        station = as.character(st),
        # Retrieve station coordinates from daily data (assumed constant)
        latitude = first(df_daily$latitude),
        longitude = first(df_daily$longitude),
        altitude = first(df_daily$altitude)
      )

    results_list[[i]] <- df_annual
  }

  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0) {
    warning("No annual data available for any station in the specified date range.")
    return(data.frame())
  }

  df_all <- bind_rows(results_list) %>% mutate(station = as.character(station))

  if (output_format == "all") {
    # Tidy (long) format: one row per station-year.
    final_df <- df_all %>%
      mutate(year = YEAR) %>%
      select(station, latitude, longitude, altitude, year, annual_val) %>%
      rename(!!variable := annual_val) %>%
      arrange(station, year)
  } else {
    # "reduce": Wide format: one row per year, with one column per station aggregated value.
    df_all <- df_all %>%
      mutate(year = YEAR)
    df_wide <- df_all %>%
      select(station, year, annual_val) %>%
      pivot_wider(names_from = station, values_from = annual_val, names_prefix = "est")

    # Rename columns to append the variable name (e.g., "est1041prec")
    new_names <- sapply(names(df_wide)[-1], function(x) paste0(x, variable))
    names(df_wide)[-1] <- new_names
    final_df <- df_wide %>% arrange(year)
  }

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  return(final_df)
}
