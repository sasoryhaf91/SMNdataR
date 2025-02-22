#' Download Multiple Stations Data with Flexible Output Formats
#'
#' Downloads daily data for one or more stations, filters the data by the specified date range,
#' and retains only the requested variable columns along with optional coordinate information.
#' The output format can be specified as:
#'
#' \describe{
#'   \item{"all"}{Returns a data frame with columns: station, latitude, longitude, altitude, Date,
#'                and the selected variable(s). (Default)}
#'   \item{"reduce"}{Returns a data frame in wide format with a single row per date.
#'                   The Date column is retained, and for each station and variable a column is created
#'                   with a name in the format "est{station}{variable}" (e.g., "est15101Prec").}
#' }
#'
#' @param stations A vector of station codes (character or numeric).
#' @param start_date Start date for data download (default "1961-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param max_attempts Maximum number of attempts for each station download (default 3).
#' @param variable A vector of variable names to retain (default \code{c("Prec", "Evap", "Tmax", "Tmin")}).
#' @param output_format Output format: either "all" (default) or "reduce".
#' @param csv_file (Optional) File path to save the resulting data frame as CSV.
#'
#' @return A data frame containing the downloaded data in the specified format.
#'
#' @examples
#' \dontrun{
#'   # Download full format data for stations "15101" and "15102"
#'   df_full <- smndata_download_stations(
#'     stations = c("15101", "15102"),
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     variable = c("Prec", "Evap"),
#'     output_format = "all"
#'   )
#'
#'   # Download reduced format data with wide-format columns, e.g., "est15101Prec"
#'   df_reduce <- smndata_download_stations(
#'     stations = c("15101", "15102"),
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     variable = c("Prec", "Evap"),
#'     output_format = "reduce"
#'   )
#' }
#'
#' @export
smndata_download_stations <- function(stations,
                                      start_date = "1961-01-01",
                                      end_date = Sys.Date(),
                                      max_attempts = 3,
                                      variable = c("prec", "evap", "tmax", "tmin"),
                                      output_format = c("all", "reduce"),
                                      csv_file = NULL) {
  output_format <- match.arg(output_format)
  library(dplyr)
  library(tidyr)
  library(lubridate)

  start_time <- Sys.time()
  results_list <- vector("list", length(stations))

  for (i in seq_along(stations)) {
    st <- stations[i]
    message("Starting download for station: ", st, " ...")

    # Download the station's daily data using smndata_download_station
    df_st <- tryCatch(
      smndata_handle_error(expr = smndata_download_station(st, start_date, end_date),
                           max_attempts = max_attempts),
      error = function(e) {
        message("Error for station ", st, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(df_st)) {
      # Ensure the data is filtered by date (if not already done)
      df_st <- df_st %>% filter(date >= as.Date(start_date) & date <= as.Date(end_date))

      if (nrow(df_st) > 0) {
        # Check that the requested variables exist in the data
        if (!all(variable %in% names(df_st))) {
          message("Not all requested variables found for station ", st)
          results_list[[i]] <- NULL
        } else {
          # In "all" format, retain coordinate columns as well.
          df_st <- df_st %>% mutate(station = as.character(st)) %>%
            select(station, latitude, longitude, altitude, date, all_of(variable))
          results_list[[i]] <- df_st
          message("Station ", st, " downloaded successfully.")
        }
      } else {
        message("Station ", st, " has no data in the specified date range.")
        results_list[[i]] <- NULL
      }
    } else {
      results_list[[i]] <- NULL
    }

    gc()  # Free memory after each iteration
  }

  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0) {
    warning("No data downloaded for any station in the specified date range.")
    return(data.frame())
  }

  # Combine data from all stations (default format: multiple rows per station)
  final_df <- bind_rows(results_list)

  # Process output format
  if (output_format == "reduce") {
    # In reduced format, pivot data so that each station-variable combination becomes a column.
    # We drop the coordinate columns and keep only Date and the variables, renaming the variable columns.
    final_df <- final_df %>%
      select(date, station, all_of(variable)) %>%
      pivot_longer(cols = all_of(variable), names_to = "var", values_to = "val") %>%
      mutate(new_col = paste0("est", station, var)) %>%
      select(-station, -var) %>%
      pivot_wider(names_from = new_col, values_from = val)
  }

  total_time <- Sys.time() - start_time
  message("Download completed. Total time: ", round(as.numeric(total_time, units = "secs"), 2), " seconds.")

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  return(final_df)
}
