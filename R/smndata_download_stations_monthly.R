#' Download and Summarize Monthly Station Data for a Single Variable (Multiple Stations)
#'
#' Downloads daily data for one or more stations over a specified date range, aggregates the data
#' at a monthly level for a chosen variable, and returns the result in one of two formats.
#'
#' For variables "prec" or "evap", monthly values are aggregated by summing daily data;
#' for "tmax" or "tmin", they are aggregated by taking the mean (or a custom aggregator if provided).
#' Any month with a fraction of missing daily data greater than \code{max_missing_frac} (default 0.2)
#' is set to NA.
#'
#' The output format can be specified as follows:
#'
#' \describe{
#'   \item{"all"}{Tidy (long) format: one row per station-year-month with columns:
#'               station, latitude, longitude, altitude, date (formatted as "YYYY-MM"),
#'               and the aggregated variable value.}
#'   \item{"reduce"}{Wide format: one row per "YYYY-MM", with one column per station–variable combination
#'                   named "est{station}{variable}" (e.g., "est1041prec").}
#' }
#'
#' Optionally, the final data frame can be saved as a CSV file.
#'
#' @param stations A vector of station codes (character or numeric).
#' @param variable A character string specifying the variable to summarize (e.g., "prec", "evap", "tmax", or "tmin").
#'        For "prec" or "evap", monthly values are summed; for "tmax" or "tmin", they are averaged.
#' @param start_date Start date for daily data download (default "1961-01-01").
#' @param end_date End date for daily data download (default "2023-12-31").
#' @param max_missing_frac Maximum fraction of missing daily data allowed per month (default 0.2).
#'        If a month's missing fraction exceeds this threshold, that month's aggregated value is set to NA.
#' @param output_format Output format: either "all" (tidy/long format) or "reduce" (wide format).
#' @param aggregator (Optional) A custom function for monthly aggregation (e.g., sum or mean).
#'        If NULL, the function automatically chooses sum for "prec"/"evap" and mean for "tmax"/"tmin".
#' @param csv_file (Optional) File path to save the resulting data frame as CSV. If NULL, no CSV is written.
#'
#' @return A data frame with monthly-aggregated data in the chosen format.
#'
#' @examples
#' \dontrun{
#'   # Example 1: Summarize monthly precipitation data for stations "1041" and "15101" in tidy format.
#'   df_long <- smndata_download_stations_monthly(
#'     stations = c("1041", "15101"),
#'     variable = "prec",
#'     start_date = "1980-01-01",
#'     end_date = "2000-12-31",
#'     output_format = "all"
#'   )
#'   head(df_long)
#'
#'   # Example 2: Summarize monthly maximum temperature data for stations "1041" and "15101" in wide format.
#'   df_wide <- smndata_download_stations_monthly(
#'     stations = c("1041", "15101"),
#'     variable = "tmax",
#'     start_date = "1980-01-01",
#'     end_date = "2000-12-31",
#'     output_format = "reduce"
#'   )
#'   head(df_wide)
#'
#'   # Example 3: Use a custom aggregator (median) for precipitation and save the result to CSV.
#'   df_custom <- smndata_download_stations_monthly(
#'     stations = c("1041", "15101"),
#'     variable = "prec",
#'     aggregator = median,
#'     csv_file = "monthly_prec_custom.csv"
#'   )
#' }
#'
#' @export
smndata_download_stations_monthly <- function(stations,
                                              variable,
                                              start_date = "1961-01-01",
                                              end_date = "2023-12-31",
                                              max_missing_frac = 0.2,
                                              output_format = c("all", "reduce"),
                                              aggregator = NULL,
                                              del_na = "yes",
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

    df_daily$Date <- as.Date(df_daily$date)

    # Aggregate daily data to monthly.
    df_monthly <- df_daily %>%
      mutate(
        YEAR = year(date),
        MONTH = month(date)
      ) %>%
      group_by(YEAR, MONTH) %>%
      summarise(
        total_days = n(),
        missing_days = sum(is.na(.data[[variable]])),
        monthly_val = if (is.null(aggregator)) {
          if (variable %in% c("prec", "evap")) sum(.data[[variable]], na.rm = TRUE)
          else if (variable %in% c("tmax", "tmin")) mean(.data[[variable]], na.rm = TRUE)
          else mean(.data[[variable]], na.rm = TRUE)
        } else {
          aggregator(.data[[variable]], na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      complete(YEAR, MONTH = 1:12) %>%
      mutate(
        total_days = ifelse(is.na(total_days), 0, total_days),
        missing_days = ifelse(is.na(missing_days), 0, missing_days),
        missing_frac = ifelse(total_days > 0, missing_days / total_days, NA),
        monthly_val = ifelse(missing_frac > max_missing_frac, NA, monthly_val),
        station = as.character(st),
        latitude = first(df_daily$latitude),
        longitude = first(df_daily$longitude),
        altitude = first(df_daily$altitude)
      )

    results_list[[i]] <- df_monthly
  }

  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0) {
    warning("No monthly data available for any station in the specified date range.")
    return(data.frame())
  }

  df_all <- bind_rows(results_list) %>% mutate(station = as.character(station))

  if (output_format == "all") {
    # Tidy (long) format: one row per station-year-month.
    final_df <- df_all %>%
      mutate(date = paste(YEAR, sprintf("%02d", MONTH), sep = "-")) %>%
      select(station, latitude, longitude, altitude, date, monthly_val) %>%
      rename(!!variable := monthly_val) %>%
      arrange(station, date)

    # Optionally join station coordinates if needed (already included here).

  } else {
    # "reduce" wide format: one row per date with one column per station-variable combination.
    df_all <- df_all %>%
      mutate(date = paste(YEAR, sprintf("%02d", MONTH), sep = "-"))

    df_wide <- df_all %>%
      select(station, date, monthly_val) %>%
      pivot_wider(names_from = station, values_from = monthly_val, names_prefix = "est")

    new_names <- sapply(names(df_wide)[-1], function(x) paste0(x, variable))
    names(df_wide)[-1] <- new_names
    final_df <- df_wide %>% arrange(date)
  }

  # Optionally remove rows with NA values
  if (del_na == "yes") {
    final_df <- na.omit(final_df)
  }

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  return(final_df)
}
