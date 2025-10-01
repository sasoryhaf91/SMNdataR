#' Download Daily Data for One or More Stations
#'
#' Downloads and compiles daily SMN data for one or more stations over a specified date range,
#' with control over output format and selected variables. Optionally merges with NASA POWER data.
#'
#' @param stations A character or numeric vector of station codes.
#' @param start_date Start date for the data (default is "1961-01-01").
#' @param end_date End date for the data (default is \code{Sys.Date()}).
#' @param variable A character vector of variable names to retain (default is all: prec, evap, tmax, tmin).
#' @param output_format Format of the output: "all" (default) includes coordinates; "reduced" only includes climatic variables.
#' @param csv_file Optional path to save the resulting data as CSV.
#' @param source Data source: either "single" (only SMN) or "nasa" (merge SMN with NASA POWER).
#'
#' @return A data frame with the combined daily data.
#'
#' @examples
#' \dontrun{
#'   smn_dl_daily_batch(
#'     stations = c("15101", "15102"),
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     variable = c("prec", "evap"),
#'     output_format = "all",
#'     source = "nasa"
#'   )
#' }
#'
#' @export
smn_dl_daily_batch <- function(stations,
                               start_date = "1984-01-01",
                               end_date = Sys.Date(),
                               variable = c("prec", "evap", "tmax", "tmin","T2M_MAX", "T2M_MIN", "RH2M", "PRECTOTCORR", "EVLAND",),
                               output_format = c("all", "reduced"),
                               csv_file = NULL,
                               source = c("single", "nasa")) {

  output_format <- match.arg(output_format)
  source <- match.arg(source)
  stations <- as.character(stations)
  results_list <- vector("list", length(stations))

  for (i in seq_along(stations)) {
    st <- stations[i]
    message("Downloading: ", st)

    df_st <- tryCatch({
      if (source == "single") {
        smn_dl_daily_single(st, start_date, end_date, output_format = "full", del_na = "no")
      } else {
        smn_dl_daily_nasa(st, start_date, end_date, output_format = output_format)
      }
    }, error = function(e) {
      message("Error at station ", st, ": ", conditionMessage(e))
      return(NULL)
    })

    if (!is.null(df_st)) {
      if (!all(variable %in% names(df_st))) {
        message("Missing variables for station ", st)
        next
      }

      if (output_format == "all") {
        df_st <- df_st[, c("station", "latitude", "longitude", "altitude", "date", variable)]
      } else {
        df_st <- df_st[, c("date", variable)]
      }

      results_list[[i]] <- df_st
    }

    gc()
  }

  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0) {
    warning("No valid data returned.")
    return(data.frame())
  }

  final_df <- do.call(rbind, results_list)

  if (!is.null(csv_file)) {
    write.csv(final_df, file = csv_file, row.names = FALSE)
    message("CSV saved to: ", csv_file)
  }

  return(final_df)
}

