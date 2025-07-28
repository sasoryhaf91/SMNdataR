#' Download Daily Data for Multiple Stations
#'
#' Downloads daily SMN data for multiple stations over a specified date range and combines
#' the results into a single data frame. The output format can be specified as follows:
#'
#' \describe{
#'   \item{"all"}{Returns full daily data including station, latitude, longitude, altitude, date,
#'                and the selected climatic variables. (Default)}
#'   \item{"reduced"}{Returns only the daily climatic variables (date and selected variables).}
#' }
#'
#' @param stations A vector of station codes (character or numeric).
#' @param start_date Start date for data download (default "1961-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param max_attempts Maximum number of attempts per station (default 3).
#' @param variable A vector of variable names to retain (default \code{c("prec", "evap", "tmax", "tmin")}).
#' @param output_format Output format: either "all" (default) or "reduced".
#' @param csv_file (Optional) File path to save the final data frame as CSV.
#'
#' @return A data frame with combined daily data for all stations.
#'
#' @examples
#' \dontrun{
#'   df_all <- smndata_download_stations_daily(
#'     stations = c("15101", "15102"),
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     variable = c("prec", "evap"),
#'     output_format = "all"
#'   )
#'   head(df_all)
#' }
#'
#' @export
smndata_download_stations_daily <- function(stations,
                                            start_date = "1961-01-01",
                                            end_date = "2023-12-31",
                                            max_attempts = 3,
                                            variable = c("prec", "evap", "tmax", "tmin"),
                                            output_format = c("all", "reduced"),
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

    # Wrap download, Date conversion, and filtering in a tryCatch to skip errors.
    df_st <- tryCatch({
      temp <- smndata_download_station(st, start_date, end_date)
      if (!is.null(temp) && "date" %in% names(temp)) {
        # Convert Date column to atomic Date vector
        temp$date <- as.Date(unlist(temp$date))
        names(temp)[names(temp) == "date"] <- "date"
      }
      temp <- temp %>% filter(date >= as.Date(start_date) & date <= as.Date(end_date))
      temp
    }, error = function(e) {
      message("Error processing station ", st, ": ", conditionMessage(e))
      return(NULL)
    })

    if (!is.null(df_st)) {
      if (!all(variable %in% names(df_st))) {
        message("Not all requested variables found for station ", st)
        results_list[[i]] <- NULL
      } else {
        if (output_format == "all") {
          df_st <- df_st %>% mutate(station = as.character(st)) %>%
            select(station, latitude, longitude, altitude, date, dplyr::all_of(variable))
        } else {
          df_st <- df_st %>% select(date, dplyr::all_of(variable))
        }
        results_list[[i]] <- df_st
        message("Station ", st, " downloaded successfully.")
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

  final_df <- bind_rows(results_list)
  total_time <- Sys.time() - start_time
  message("Download completed. Total time: ", round(as.numeric(total_time, units = "secs"), 2), " seconds.")

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("CSV file saved at: ", csv_file)
  }

  return(final_df)
}

