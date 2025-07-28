#' Download and Summarize Monthly Station Data for a Single Variable
#'
#' Downloads daily data for a given station and date range, then aggregates it at a monthly level
#' for one chosen variable. By default, if the variable is "prec" or "evap", the monthly aggregation
#' is computed by summing; if it's "tmax" or "tmin", it is computed by taking the mean. You may also
#' provide a custom aggregator function. Any month with more than a certain fraction of missing daily data
#' (default 20%) is set to NA. The user can choose the output format:
#'
#' \describe{
#'   \item{"reduce"}{Wide format with monthly columns named JAN..DEC, plus ACUM (annual sum or mean),
#'                   PROM (annual average ignoring NA), and MONTHS (number of valid months).}
#'   \item{"long"}{Tidy format with columns YEAR, MONTH, and the monthly value.}
#' }
#'
#' Missing data is ignored when computing monthly values, except for months exceeding the missing
#' threshold, which become NA. Optionally, the final data frame can be exported to a CSV file.
#'
#' @param station Station code (character or numeric).
#' @param variable A character string specifying the variable to download and summarize
#'        (e.g., "prec", "evap", "tmax", or "tmin"). For "prec" or "evap", the monthly value is the sum;
#'        for "tmax" or "tmin", it is the mean.
#' @param start_date Start date for daily data download (default "1961-01-01").
#' @param end_date End date for daily data download (default "2023-12-31").
#' @param max_missing_frac Maximum fraction of missing daily data allowed per month (default 0.2).
#'        If a month's missing fraction exceeds this threshold, that month's value is set to NA.
#' @param output_format Output format: either "reduce" (wide format, default) or "long" (tidy format).
#' @param aggregator (Optional) A custom function for monthly aggregation (e.g., sum or mean).
#'        If NULL, the function chooses automatically: sum for "prec"/"evap", mean for "tmax"/"tmin".
#' @param csv_file (Optional) File path to save the resulting data frame as CSV. If NULL, no CSV is written.
#'
#' @return A data frame with monthly-aggregated data in the chosen format.
#'
#' @examples
#' \dontrun{
#'   # Summarize monthly precipitation data for station "1041" in wide format.
#'   df_monthly_prec <- smndata_download_station_monthly(
#'     station = "1041",
#'     variable = "prec",
#'     start_date = "1980-01-01",
#'     end_date = "2000-12-31",
#'     output_format = "reduce"
#'   )
#'   head(df_monthly_prec)
#'
#'   # Summarize monthly maximum temperature data in tidy format, with up to 25% missing allowed.
#'   df_monthly_tmax <- smndata_download_station_monthly(
#'     station = "1041",
#'     variable = "tmax",
#'     max_missing_frac = 0.25,
#'     output_format = "long"
#'   )
#'   head(df_monthly_tmax)
#'
#'   # Provide a custom aggregator (e.g., median) and save the wide-format result to a CSV.
#'   df_monthly_custom <- smndata_download_station_monthly(
#'     station = "1041",
#'     variable = "prec",
#'     aggregator = median,
#'     csv_file = "monthly_prec_1041.csv"
#'   )
#' }
#'
#' @export
smndata_download_station_monthly <- function(station,
                                             variable,
                                             start_date = "1961-01-01",
                                             end_date = "2023-12-31",
                                             max_missing_frac = 0.2,
                                             output_format = c("reduce", "long"),
                                             aggregator = NULL,
                                             csv_file = NULL) {
  output_format <- match.arg(output_format)
  library(dplyr)
  library(tidyr)
  library(lubridate)

  # 1) Download daily data (assumed to have columns 'Date' and the chosen variable).
  # Here, smndata_download_station is assumed to return a data frame with a 'Date' column (class Date)
  # and columns with lower-case names: e.g., "prec", "evap", "tmax", "tmin".
  df_daily <- smndata_download_station(station, start_date, end_date)
  if (nrow(df_daily) == 0) {
    warning("No daily data available for station ", station, " in the specified range.")
    return(data.frame())
  }

  if (!variable %in% names(df_daily)) {
    stop("The variable '", variable, "' is not found in the daily data.")
  }

  # 2) Determine aggregator function automatically if not provided
  if (is.null(aggregator)) {
    if (variable %in% c("prec", "evap")) {
      aggregator_fun <- sum
    } else if (variable %in% c("tmax", "tmin")) {
      aggregator_fun <- mean
    } else {
      warning("Variable '", variable, "' is not recognized. Defaulting aggregator to 'mean'.")
      aggregator_fun <- mean
    }
  } else {
    aggregator_fun <- aggregator
  }

  # 3) Create monthly summary:
  # Convert daily data: create YEAR and MONTH columns. Use complete() to ensure every month (1:12) appears for each year.
  df_monthly <- df_daily %>%
    mutate(YEAR = year(date),
           MONTH = month(date)) %>%
    group_by(YEAR, MONTH) %>%
    summarise(
      total_days = n(),
      missing_days = sum(is.na(.data[[variable]])),
      monthly_val = aggregator_fun(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(YEAR, MONTH = 1:12) %>%  # ensure all months appear for each year
    mutate(
      total_days = ifelse(is.na(total_days), 0, total_days),
      missing_days = ifelse(is.na(missing_days), 0, missing_days),
      missing_frac = ifelse(total_days > 0, missing_days / total_days, NA),
      monthly_val = ifelse(missing_frac > max_missing_frac, NA, monthly_val)
    )

  # 4) Return in "long" or "reduce" format.
  if (output_format == "long") {
    # Tidy format: columns YEAR, MONTH, and the monthly value
    df_long <- df_monthly %>%
      select(YEAR, MONTH, !!variable := monthly_val) %>%
      arrange(YEAR, MONTH)

    if (!is.null(csv_file)) {
      write.csv(df_long, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }
    return(df_long)
  } else {
    # "reduce": wide format.
    # Use English month abbreviations. We force the factor levels so the columns are in order JAN..DEC.
    eng_months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

    # Ensure MONTH is a factor with levels 1:12.
    df_monthly <- df_monthly %>%
      mutate(MONTH = factor(MONTH, levels = 1:12))

    df_wide <- df_monthly %>%
      select(YEAR, MONTH, monthly_val) %>%
      pivot_wider(names_from = MONTH, values_from = monthly_val) %>%
      # Rename columns from factor levels to eng_months. We assume that the pivot_wider
      # creates columns named as "1", "2", ..., "12". We force that order.
      rename_at(vars(matches("^[0-9]+$")), ~ eng_months[as.numeric(.)])

    # Compute annual summary columns:
    df_wide <- df_wide %>%
      mutate(
        ACUM = rowSums(across(all_of(eng_months)), na.rm = TRUE),
        PROM = rowMeans(across(all_of(eng_months)), na.rm = TRUE),
        MONTHS = rowSums(!is.na(across(all_of(eng_months))))
      ) %>%
      arrange(YEAR)

    if (!is.null(csv_file)) {
      write.csv(df_wide, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }
    return(df_wide)
  }
}
