#' Merge SMN Data with MODIS Satellite Data
#'
#' Downloads daily SMN data for a given station within the specified date range and merges it
#' with MODIS satellite data (LST and NDVI) from MODIS Terra products. For LST, the default product
#' is "MOD11A1" and for NDVI it is "MOD13Q1". MODIS data is available from 2000 onward; if the specified
#' start_date is earlier than "2000-01-01", the MODIS query will use "2000-01-01" as the start date.
#' The output format can be specified:
#'
#' \describe{
#'   \item{"all"}{Returns the full merged data including SMN columns (e.g., station, latitude, longitude,
#'                altitude, Date, etc.) along with MODIS_LST and MODIS_NDVI. (Default)}
#'   \item{"reduce"}{Returns only the Date, MODIS_LST, and MODIS_NDVI columns.}
#' }
#'
#' @param station Station code (character or numeric).
#' @param start_date Start date for SMN data download (default "1961-01-01").
#' @param end_date End date for data download (default \code{Sys.Date()}).
#' @param output_format Output format: either "all" (default) or "reduce".
#' @param modis_product_lst MODIS product for LST (default "MOD11A1").
#' @param modis_product_ndvi MODIS product for NDVI (default "MOD13Q1").
#' @param band_lst Satellite band for LST (default "LST").
#' @param band_ndvi Satellite band for NDVI (default "NDVI").
#'
#' @return A data frame with the merged SMN and MODIS data. In "all" format, the data frame includes
#'         SMN columns (e.g., station, latitude, longitude, altitude, Date, etc.) plus MODIS_LST and MODIS_NDVI;
#'         in "reduce" format, only Date, MODIS_LST, and MODIS_NDVI are returned.
#'
#' @examples
#' \dontrun{
#'   # Merge data for station "15101" from 2010-01-01 to 2010-12-31 in full format.
#'   merged_df_all <- smndata_merge_modis("15101",
#'                                        start_date = "2010-01-01",
#'                                        end_date = "2010-12-31",
#'                                        output_format = "all")
#'   head(merged_df_all)
#'
#'   # Merge data for station "15101" in reduced format.
#'   merged_df_reduce <- smndata_merge_modis("15101",
#'                                           start_date = "2010-01-01",
#'                                           end_date = "2010-12-31",
#'                                           output_format = "reduce")
#'   head(merged_df_reduce)
#' }
#'
#' @export
smndata_merge_modis <- function(station,
                                start_date = "1961-01-01",
                                end_date = Sys.Date(),
                                output_format = c("all", "reduce"),
                                modis_product_lst = "MOD11A1",
                                modis_product_ndvi = "MOD13Q1",
                                band_lst = "LST",
                                band_ndvi = "NDVI") {
  output_format <- match.arg(output_format)
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(MODISTools)

  # Download SMN data using the optimized SMN function.
  # (smndata_download_station is assumed to filter by date and include coordinates)
  smn_data <- smndata_download_station(station, start_date, end_date)
  if (nrow(smn_data) == 0) {
    warning("No SMN data available for station ", station, " in the specified range.")
    return(data.frame())
  }

  # Extract unique station coordinates (assumed constant for the station)
  lat <- unique(smn_data$latitude)
  lon <- unique(smn_data$longitude)
  if (length(lat) != 1 || length(lon) != 1) {
    warning("Station coordinates are inconsistent for station ", station)
  }

  # Adjust MODIS start date: MODIS data available from 2000-01-01 onward.
  modis_start <- as.Date(start_date)
  if (modis_start < as.Date("2000-01-01")) {
    modis_start <- as.Date("2000-01-01")
    message("MODIS data is available from 2000-01-01. Using that as the start date for MODIS queries.")
  }

  # Helper function to query MODIS data for a given date, product, and band.
  get_modis_value <- function(date, product, band) {
    result <- tryCatch({
      mod_data <- MODISTools::mt_subset(product = product,
                                        lat = lat, lon = lon,
                                        band = band,
                                        start = as.character(date),
                                        end = as.character(date))
      if (length(mod_data$value) == 0) return(NA)
      mean(mod_data$value, na.rm = TRUE)
    }, error = function(e) {
      NA
    })
    return(result)
  }

  # Query MODIS LST and NDVI for each date in the SMN dataset.
  modis_lst <- sapply(smn_data$date, function(d) get_modis_value(d, product = modis_product_lst, band = band_lst))
  # If the resulting vector is not the same length as smn_data, replace it with NA values.
  if (length(modis_lst) != nrow(smn_data)) {
    modis_lst <- rep(NA, nrow(smn_data))
  }
  modis_ndvi <- sapply(smn_data$date, function(d) get_modis_value(d, product = modis_product_ndvi, band = band_ndvi))
  if (length(modis_ndvi) != nrow(smn_data)) {
    modis_ndvi <- rep(NA, nrow(smn_data))
  }

  # Merge the MODIS data with the SMN data.
  merged_data <- smn_data %>% mutate(MODIS_LST = modis_lst,
                                     MODIS_NDVI = modis_ndvi)

  # Adjust output format based on user choice.
  if (output_format == "reduce") {
    # In reduced format, rename columns with the station prefix.
    # Create new column names for each record: "est{station}{variable}"
    merged_data <- merged_data %>%
      mutate(est_station = paste0("est", station)) %>%
      select(date, est_station, MODIS_LST, MODIS_NDVI)
    # Pivot the data so that the MODIS values are in separate columns with names based on the station.
    merged_data <- merged_data %>%
      pivot_longer(cols = c(MODIS_LST, MODIS_NDVI), names_to = "variable", values_to = "value") %>%
      mutate(variable = paste0(est_station, variable)) %>%
      select(-est_station) %>%
      pivot_wider(names_from = variable, values_from = value)
  }

  return(merged_data)
}
