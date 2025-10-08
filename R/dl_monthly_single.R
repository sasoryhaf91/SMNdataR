#' Download and Aggregate Monthly Climate Data (single station)
#'
#' Downloads daily SMN data for a station, aggregates it to monthly values for
#' one or more variables, applies a completeness rule per month, and returns
#' the result in either *long* (tidy) or *reduce* (wide) format.
#'
#' @param station Character or numeric station code.
#' @param variables Character vector of variables to aggregate, or `"all"`.
#'   Valid names: `"prec"`, `"evap"`, `"tmax"`, `"tmin"`. Default `"all"`.
#' @param start_date Start date (inclusive). Default `"1961-01-01"`.
#' @param end_date End date (inclusive). Default `Sys.Date()`.
#' @param max_missing_frac Numeric in `[0,1]`. If the fraction of missing days
#'   in a month exceeds this threshold, the monthly value is set to `NA`.
#'   Default `0.2` (i.e., allow up to 20% missing days).
#' @param output_format `"long"` or `"reduce"` (default `"reduce"`).
#'   - `"long"`: one tidy table with columns
#'     `station, latitude, longitude, altitude, variable, YEAR, MONTH, value`.
#'   - `"reduce"`: wide table(s) per variable, one row per YEAR, columns `JAN..DEC`
#'     plus `ACUM` (annual sum or mean, see Details), `PROM` (mean across valid
#'     months), and `MONTHS` (number of valid months). If multiple variables
#'     are requested, a named `list` of wide tables is returned.
#' @param aggregator Either a single function (applied to all variables) or a
#'   **named list** mapping variable name -> function (e.g.,
#'   `list(prec = sum, tmax = mean)`). If `NULL`, defaults are used:
#'   `sum` for `prec,evap`; `mean` for `tmax,tmin`.
#' @param csv_file Optional path. If provided:
#'   - For `"long"`: a single CSV is written at `csv_file`.
#'   - For `"reduce"` with multiple variables: one CSV per variable using
#'     `paste0(tools::file_path_sans_ext(csv_file), "_", var, ".csv")`.
#'
#' @return
#' - If `output_format = "long"`: a single tidy `data.frame`.
#' - If `output_format = "reduce"` and length(variables) == 1: a wide `data.frame`.
#' - If `output_format = "reduce"` and length(variables) > 1: a **named list**
#'   of wide `data.frame`s (one per variable).
#'
#' @details
#' Missing fraction is computed against the **expected number of days** in each
#' month within the requested date window; days not present in the raw data are
#' treated as missing. `ACUM` is the **row sum** for sum-type variables
#' (`prec, evap`) and the **row mean** for mean-type variables (`tmax, tmin`);
#' `PROM` is always the row mean across valid months.
#'
#' @examples
#' \dontrun{
#' # All variables, monthly, as a tidy table including station metadata:
#' m_long <- smn_dl_monthly_single(
#'   station = "15101",
#'   variables = "all",
#'   start_date = "1984-01-01",
#'   end_date   = "2020-12-31",
#'   output_format = "long"
#' )
#'
#' # Only precipitation, wide format:
#' m_prec <- smn_dl_monthly_single("15101", variables = "prec", output_format = "reduce")
#'
#' # Custom aggregation (median for all vars) and stricter completeness (<=10% missing):
#' m_med <- smn_dl_monthly_single("15101", variables = c("tmax","tmin"),
#'                                max_missing_frac = 0.10, aggregator = median,
#'                                output_format = "long")
#' }
#'
#' @export
smn_dl_monthly_single <- function(station,
                                  variables = "all",
                                  start_date = "1961-01-01",
                                  end_date   = Sys.Date(),
                                  max_missing_frac = 0.9,
                                  output_format = c("reduce", "long"),
                                  aggregator = NULL,
                                  csv_file   = NULL) {
  output_format <- match.arg(output_format)

  # ---- download once (full to carry metadata) -------------------------------
  daily <- smn_dl_daily_single(station, start_date, end_date, output_format = "full", del_na = "no")
  if (!nrow(daily)) {
    warning("No daily data available for station ", station, " in the specified date range.")
    return(if (output_format == "long") data.frame() else data.frame())
  }

  # ---- discover variables & validate ----------------------------------------
  all_vars <- intersect(c("prec","evap","tmax","tmin"), names(daily))
  if (identical(variables, "all")) variables <- all_vars
  variables <- unique(as.character(variables))
  if (!length(variables)) stop("No requested variables are available in the daily dataset.")
  if (!all(variables %in% all_vars)) {
    missing <- setdiff(variables, all_vars)
    stop("The following variables are not present in the daily data: ",
         paste(missing, collapse = ", "))
  }

  # ---- metadata (first non-NA row if possible) ------------------------------
  st_chr <- as.character(station)
  meta_row <- daily[stats::complete.cases(daily[, c("latitude","longitude","altitude")]), , drop = FALSE]
  if (!nrow(meta_row)) meta_row <- daily[1, , drop = FALSE]
  lat <- meta_row$latitude[1]; lon <- meta_row$longitude[1]; alt <- meta_row$altitude[1]

  # ---- aggregator map --------------------------------------------------------
  default_map <- list(prec = sum, evap = sum, tmax = mean, tmin = mean)
  agg_map <- default_map
  if (!is.null(aggregator)) {
    if (is.function(aggregator)) {
      agg_map <- setNames(rep(list(aggregator), length(default_map)), names(default_map))
    } else if (is.list(aggregator) && length(names(aggregator))) {
      for (nm in intersect(names(aggregator), names(default_map))) {
        if (is.function(aggregator[[nm]])) agg_map[[nm]] <- aggregator[[nm]]
      }
    } else {
      stop("`aggregator` must be a function or a named list of functions.")
    }
  }

  # ---- build complete calendar (so missing days are counted) ----------------
  start_date <- min(daily$date, na.rm = TRUE)
  end_date   <- max(daily$date, na.rm = TRUE)
  cal <- data.frame(date = seq(start_date, end_date, by = "day"))

  # ---- per-variable monthly aggregation -------------------------------------
  monthly_one <- function(var) {
    vdf <- merge(cal, daily[, c("date", var), drop = FALSE], by = "date", all.x = TRUE)
    names(vdf)[2] <- "val"
    vdf$YEAR  <- as.integer(format(vdf$date, "%Y"))
    vdf$MONTH <- as.integer(format(vdf$date, "%m"))

    out <- stats::aggregate(list(
      expected_days = vdf$val      # placeholder; we will replace
    ), by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = length)
    names(out)[3] <- "expected_days"

    miss <- stats::aggregate(list(missing_days = is.na(vdf$val)),
                             by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = sum)
    val  <- stats::aggregate(list(value = vdf$val),
                             by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH),
                             FUN = function(x) agg_map[[var]](x, na.rm = TRUE))

    out <- merge(out, miss, by = c("YEAR","MONTH"), all = TRUE)
    out <- merge(out, val,  by = c("YEAR","MONTH"), all = TRUE)
    out$missing_frac <- with(out, ifelse(expected_days > 0, missing_days / expected_days, NA_real_))
    out$value[out$missing_frac > max_missing_frac] <- NA_real_
    out$variable <- var
    out[order(out$YEAR, out$MONTH), c("YEAR","MONTH","variable","value")]
  }

  monthly_list <- lapply(variables, monthly_one)
  monthly_all  <- do.call(rbind, monthly_list)
  rownames(monthly_all) <- NULL

  # ---- outputs ---------------------------------------------------------------
  if (output_format == "long") {
    long <- monthly_all
    # attach station metadata as requested
    long$station   <- st_chr
    long$latitude  <- lat
    long$longitude <- lon
    long$altitude  <- alt
    names(long)[names(long) == "value"] <- "value"

    # order columns
    long <- long[, c("station","latitude","longitude","altitude",
                     "variable","YEAR","MONTH","value"), drop = FALSE]
    long <- long[order(long$variable, long$YEAR, long$MONTH), , drop = FALSE]
    rownames(long) <- NULL

    if (!is.null(csv_file)) {
      utils::write.csv(long, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }
    return(long)
  }

  # reduce (wide) per variable
  month_labels <- c("JAN","FEB","MAR","APR","MAY","JUN",
                    "JUL","AUG","SEP","OCT","NOV","DEC")

  reduce_one <- function(var) {
    df <- monthly_all[monthly_all$variable == var, c("YEAR","MONTH","value")]
    if (!nrow(df)) {
      return(data.frame(YEAR = integer(), matrix(numeric(), nrow = 0, ncol = 12,
                                                 dimnames = list(NULL, month_labels)),
                        ACUM = numeric(), PROM = numeric(), MONTHS = integer()))
    }
    df$MONTH <- factor(df$MONTH, levels = 1:12, labels = month_labels)
    wide <- reshape(df, idvar = "YEAR", timevar = "MONTH", direction = "wide")
    # clean colnames "value.JAN" -> "JAN"
    names(wide) <- sub("^value\\.", "", names(wide))
    # ensure all month columns present
    for (m in setdiff(month_labels, names(wide))) wide[[m]] <- NA_real_
    wide <- wide[, c("YEAR", month_labels), drop = FALSE]
    # ACUM and PROM
    is_sum <- var %in% c("prec","evap")
    month_mat <- as.matrix(wide[, month_labels, drop = FALSE])
    ACUM <- if (is_sum) rowSums(month_mat, na.rm = TRUE) else rowMeans(month_mat, na.rm = TRUE)
    PROM <- rowMeans(month_mat, na.rm = TRUE)
    MONTHS <- rowSums(!is.na(month_mat))
    out <- cbind(wide, ACUM = ACUM, PROM = PROM, MONTHS = MONTHS)
    out[order(out$YEAR), , drop = FALSE]
  }

  reduce_list <- setNames(lapply(variables, reduce_one), variables)

  if (!is.null(csv_file)) {
    base <- tools::file_path_sans_ext(csv_file)
    ext  <- sub("^.*\\.", "", ifelse(grepl("\\.", csv_file), csv_file, "csv"))
    ext  <- if (identical(ext, csv_file)) "csv" else ext
    if (length(reduce_list) == 1L) {
      utils::write.csv(reduce_list[[1]], file = paste0(base, ".", ext), row.names = FALSE)
      message("CSV file saved at: ", paste0(base, ".", ext))
    } else {
      for (v in names(reduce_list)) {
        f <- paste0(base, "_", v, ".", ext)
        utils::write.csv(reduce_list[[v]], file = f, row.names = FALSE)
        message("CSV file saved at: ", f)
      }
    }
  }

  if (length(reduce_list) == 1L) reduce_list[[1]] else reduce_list
}
