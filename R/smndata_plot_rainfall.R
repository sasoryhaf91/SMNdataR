#' Plot Daily Rainfall Data with Monthly Axis, Skipping Excess Ticks
#'
#' Creates a base R plot of daily rainfall (precipitation) data as vertical red lines (type = "h").
#' The x-axis is labeled monthly in the format "YYYY-MM". If there are too many months to label
#' without overlap, the function automatically skips some labels. If \code{station_code} is provided,
#' the plot title becomes \code{"{station_code}-station"}; otherwise, it is "Daily Rainfall".
#'
#' After drawing the plot on the default device, if \code{pdf_file} is provided, the same plot is
#' saved to a PDF file. This way you can both see the plot in your R session and store it.
#'
#' @param data A data frame containing at least a date column and a precipitation column.
#' @param station_code A character or numeric station code for the plot title. If \code{NULL}, defaults to "Daily Rainfall".
#' @param date_col The name of the date column (default "Date").
#' @param precip_col The name of the precipitation column (default "Prec").
#' @param pdf_file (Optional) File path to save the plot as a PDF. If \code{NULL}, no PDF is created.
#' @param max_ticks Maximum number of x-axis ticks to display (default 24).
#' @param ylab A character string for the y-axis label (default "Daily rainfall [mm/day]").
#'
#' @return No return value. This function produces a base R plot on the current device and, if specified,
#'         saves the same plot to a PDF file.
#'
#' @examples
#' \dontrun{
#'   # Suppose 'rain_df' has columns "Date" and "Prec"
#'   smndata_plot_rainfall(rain_df, station_code = "4037")
#'
#'   # Also save the plot to a PDF
#'   smndata_plot_rainfall(rain_df, station_code = "4037", pdf_file = "rainfall_4037.pdf")
#' }
#'
#' @export
smndata_plot_rainfall <- function(data,
                                  station_code = NULL,
                                  date_col = "date",
                                  precip_col = "prec",
                                  pdf_file = NULL,
                                  max_ticks = 24,
                                  ylab = "Daily rainfall [mm/day]") {
  # Ensure the date column is in Date format
  data[[date_col]] <- as.Date(data[[date_col]])

  # Determine the plot title
  main_title <- if (!is.null(station_code)) {
    paste0(station_code, "-station")
  } else {
    "Daily Rainfall"
  }

  # Basic checks
  if (!all(c(date_col, precip_col) %in% names(data))) {
    stop("Could not find both the date column and the precipitation column in 'data'.")
  }

  # Determine min and max dates
  min_date <- min(data[[date_col]], na.rm = TRUE)
  max_date <- max(data[[date_col]], na.rm = TRUE)

  # Build a monthly sequence from min_date to max_date
  start_month <- as.Date(paste0(format(min_date, "%Y-%m"), "-01"))
  end_month   <- as.Date(paste0(format(max_date, "%Y-%m"), "-01"))
  monthly_seq <- seq(start_month, end_month, by = "month")
  monthly_labels <- format(monthly_seq, "%Y-%m")

  # Possibly skip ticks if too many
  if (length(monthly_seq) > max_ticks) {
    idx <- round(seq(1, length(monthly_seq), length.out = max_ticks))
    monthly_seq <- monthly_seq[idx]
    monthly_labels <- monthly_labels[idx]
  }

  # Determine y-range from the precipitation column
  y_range <- range(data[[precip_col]], na.rm = TRUE)
  if (!is.finite(y_range[1])) {
    warning("No valid precipitation data found. Plot will be empty.")
    y_range <- c(0, 1)
  }

  ########## PLOTTING ON THE CURRENT DEVICE ##########

  # Create the plot region (no points/lines yet)
  plot(
    x = data[[date_col]],
    y = data[[precip_col]],
    type = "n",
    xlab = "Date",
    ylab = ylab,
    main = main_title,
    ylim = y_range,
    axes = FALSE
  )

  # Draw vertical red lines for daily rainfall
  lines(data[[date_col]], data[[precip_col]], type = "h", col = "black")

  # Draw y-axis (left side)
  axis(2, las = 1)

  # Draw x-axis with monthly ticks labeled as "YYYY-MM"
  axis(1, at = monthly_seq, labels = monthly_labels, las = 2, cex.axis = 0.7)

  # Draw a box around the plot
  box()

  ########## IF PDF FILE IS REQUESTED, SAVE THE PLOT ##########

  if (!is.null(pdf_file)) {
    # Copy the current plot to a PDF file
    grDevices::dev.copy2pdf(file = pdf_file, width = 7, height = 5)
    message("PDF saved to: ", pdf_file)
  }
}
