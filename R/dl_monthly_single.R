#' Download and Aggregate Monthly Climate Data for a Single Station and Variable
#'
#' This function downloads daily climate data for a single meteorological station from the Mexican Weather Service
#' and aggregates it to a monthly scale for a specified variable (e.g., precipitation, temperature, evaporation).
#' It allows flexible customization of the aggregation method and output format, and includes quality control based
#' on a user-defined threshold of allowable missing data per month.
#'
#' For each month, if the fraction of missing daily records exceeds \code{max_missing_frac}, the monthly value is
#' set to \code{NA}. This helps ensure that monthly summaries are only reported when data completeness is sufficient.
#'
#' Aggregation is performed using a default rule unless a custom function is supplied:
#' \itemize{
#'   \item \code{sum} for \code{"prec"} (precipitation) and \code{"evap"} (evaporation)
#'   \item \code{mean} for \code{"tmax"} (maximum temperature) and \code{"tmin"} (minimum temperature)
#' }
#'
#' The output can be returned in either of two formats:
#' \describe{
#'   \item{"reduce"}{Wide format with one row per year, and one column per month (JAN..DEC), plus summary columns:
#'                  \code{ACUM} (annual sum or mean), \code{PROM} (annual average excluding NA), and
#'                  \code{MONTHS} (number of valid months).}
#'   \item{"long"}{Tidy long format with one row per month, and columns: \code{YEAR}, \code{MONTH}, and the value.}
#' }
#'
#' Optionally, the result can be saved to a CSV file using the \code{csv_file} argument.
#'
#' @param station Character or numeric. The station code from the Mexican Weather Service (SMN).
#' @param variable Character. The name of the variable to summarize: one of \code{"prec"}, \code{"evap"}, \code{"tmax"}, or \code{"tmin"}.
#' @param start_date Character or Date. Start date of the daily data to download (default = "1961-01-01").
#' @param end_date Character or Date. End date of the daily data to download (default = today).
#' @param max_missing_frac Numeric. Maximum allowed fraction of missing days per month (default = 0.2).
#' @param output_format Character. Output style: either \code{"reduce"} (default) or \code{"long"}.
#' @param aggregator Optional function. A custom aggregation function (e.g., \code{median}). If NULL, the default rule is applied.
#' @param csv_file Optional character. File path to export the result as CSV. If NULL, no file is written.
#'
#' @return A data frame containing monthly aggregated values in the selected format.
#'
#' @examples
#' \dontrun{
#'   # Default usage: get monthly precipitation data for station "1041" from 1980 to 2000
#'   smn_dl_monthly_single("1041", "prec", "1980-01-01", "2000-12-31")
#'
#'   # Use tidy (long) format and allow 25% missing data per month
#'   smn_dl_monthly_single("1041", "tmax", max_missing_frac = 0.25, output_format = "long")
#'
#'   # Use custom aggregation (e.g., median) and export result to CSV
#'   smn_dl_monthly_single("1041", "evap", aggregator = median, csv_file = "evap_1041_monthly.csv")
#' }
#'
#' @export
smn_dl_monthly_single <- function(station,
                                  variable,
                                  start_date = "1961-01-01",
                                  end_date = Sys.Date(),
                                  max_missing_frac = 0.2,
                                  output_format = c("reduce", "long"),
                                  aggregator = NULL,
                                  csv_file = NULL) {
  output_format <- match.arg(output_format)

  # Load daily data
  df_daily <- smn_dl_daily_single(station, start_date, end_date)
  if (nrow(df_daily) == 0) {
    warning("No daily data available for station ", station, " in the specified date range.")
    return(data.frame())
  }

  if (!variable %in% names(df_daily)) {
    stop("The variable '", variable, "' was not found in the daily dataset.")
  }

  # Define default aggregation method
  if (is.null(aggregator)) {
    aggregator_fun <- switch(variable,
                             prec = sum,
                             evap = sum,
                             tmax = mean,
                             tmin = mean,
                             {
                               warning("Unrecognized variable '", variable, "'. Using mean as default.")
                               mean
                             })
  } else {
    aggregator_fun <- aggregator
  }

  # Aggregate daily data into monthly
  df_monthly <- df_daily %>%
    dplyr::mutate(YEAR = lubridate::year(date),
                  MONTH = lubridate::month(date)) %>%
    dplyr::group_by(YEAR, MONTH) %>%
    dplyr::summarise(
      total_days = dplyr::n(),
      missing_days = sum(is.na(.data[[variable]])),
      monthly_val = aggregator_fun(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(YEAR, MONTH = 1:12) %>%
    dplyr::mutate(
      total_days = ifelse(is.na(total_days), 0, total_days),
      missing_days = ifelse(is.na(missing_days), 0, missing_days),
      missing_frac = ifelse(total_days > 0, missing_days / total_days, NA),
      monthly_val = ifelse(missing_frac > max_missing_frac, NA, monthly_val)
    )

  if (output_format == "long") {
    df_long <- df_monthly %>%
      dplyr::select(YEAR, MONTH, !!variable := monthly_val) %>%
      dplyr::arrange(YEAR, MONTH)

    if (!is.null(csv_file)) {
      utils::write.csv(df_long, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }

    return(df_long)

  } else {
    # Reduce: wide format
    eng_months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

    df_monthly <- df_monthly %>%
      dplyr::mutate(MONTH = factor(MONTH, levels = 1:12)) %>%
      dplyr::select(YEAR, MONTH, monthly_val)

    df_wide <- df_monthly %>%
      tidyr::pivot_wider(names_from = MONTH, values_from = monthly_val) %>%
      dplyr::rename_with(~ eng_months[as.numeric(.)], .cols = tidyselect::matches("^[0-9]+$")) %>%
      dplyr::mutate(
        ACUM = rowSums(dplyr::across(all_of(eng_months)), na.rm = TRUE),
        PROM = rowMeans(dplyr::across(all_of(eng_months)), na.rm = TRUE),
        MONTHS = rowSums(!is.na(dplyr::across(all_of(eng_months))))
      ) %>%
      dplyr::arrange(YEAR)

    if (!is.null(csv_file)) {
      utils::write.csv(df_wide, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }

    return(df_wide)
  }
}
