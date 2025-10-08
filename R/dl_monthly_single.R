#' Download and Aggregate Monthly Climate Data (single station)
#'
#' Downloads daily SMN data for a station and aggregates to monthly values for
#' one or more variables.
#'
#' **Long format (updated):**
#' - One row per month.
#' - Columns: `station, latitude, longitude, altitude, date, <vars...>`.
#'   If multiple variables are requested, columns include the requested subset
#'   among `prec, evap, tmin, tmax`. If a single variable is requested, only
#'   that variable column is included (plus metadata).
#'
#' **Reduce format (unchanged):**
#' - Wide per variable (one table per variable if multiple are requested) with
#'   `YEAR`, `JAN..DEC`, `ACUM`, `PROM`, `MONTHS`.
#'
#' @param station Character or numeric station code.
#' @param variables Character vector of variables to aggregate, or `"all"`.
#'   Valid names: `"prec"`, `"evap"`, `"tmax"`, `"tmin"`. Default `"all"`.
#' @param start_date Start date (inclusive). Default `"1961-01-01"`.
#' @param end_date End date (inclusive). Default `Sys.Date()`.
#' @param max_missing_frac Numeric in `[0,1]`. Fraction of missing days allowed
#'   per month before setting the monthly value to `NA`. Default `0.2`.
#' @param output_format `"long"` or `"reduce"` (default `"reduce"`).
#' @param aggregator Single function (applied to all variables) **or** a named
#'   list `list(var = fun, ...)`. Defaults: `sum` for `prec/evap`, `mean` for
#'   `tmax/tmin`.
#' @param csv_file Optional path to write a CSV. For `"long"` a single file; for
#'   `"reduce"` with multiple variables, one file per variable with a suffix.
#'
#' @return
#' - `"long"`: a single `data.frame` with columns
#'   `station, latitude, longitude, altitude, date, <vars...>`.
#' - `"reduce"`: a `data.frame` (one variable) or a named `list` of tables
#'   (multiple variables).
#'
#' @export
smn_dl_monthly_single <- function(station,
                                  variables = "all",
                                  start_date = "1961-01-01",
                                  end_date   = Sys.Date(),
                                  max_missing_frac = 0.2,
                                  output_format = c("reduce", "long"),
                                  aggregator = NULL,
                                  csv_file   = NULL) {
  output_format <- match.arg(output_format)

  # ---- download once (carry metadata) ---------------------------------------
  daily <- smn_dl_daily_single(station, start_date, end_date,
                               output_format = "full", del_na = "no")
  if (!nrow(daily)) {
    warning("No daily data available for station ", station, " in the specified date range.")
    return(if (output_format == "long") data.frame() else data.frame())
  }

  # ---- variables -------------------------------------------------------------
  all_vars <- intersect(c("prec","evap","tmax","tmin"), names(daily))
  if (identical(variables, "all")) variables <- all_vars
  variables <- unique(as.character(variables))
  if (!length(variables)) stop("No requested variables are available in the daily dataset.")
  if (!all(variables %in% all_vars)) {
    missing <- setdiff(variables, all_vars)
    stop("The following variables are not present in the daily data: ",
         paste(missing, collapse = ", "))
  }

  # ---- metadata (first complete row; fallback to first) ---------------------
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

  # ---- complete calendar (count missing days properly) ----------------------
  start_date <- min(daily$date, na.rm = TRUE)
  end_date   <- max(daily$date, na.rm = TRUE)
  cal <- data.frame(date = seq(start_date, end_date, by = "day"))

  # ---- per-variable monthly aggregation -------------------------------------
  monthly_one <- function(var) {
    vdf <- merge(cal, daily[, c("date", var), drop = FALSE], by = "date", all.x = TRUE)
    names(vdf)[2] <- "val"
    vdf$YEAR  <- as.integer(format(vdf$date, "%Y"))
    vdf$MONTH <- as.integer(format(vdf$date, "%m"))

    # expected (calendar) days
    expected <- stats::aggregate(list(expected_days = vdf$val),
                                 by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = length)
    # missing days
    missing  <- stats::aggregate(list(missing_days = is.na(vdf$val)),
                                 by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = sum)
    # aggregated value
    value    <- stats::aggregate(list(value = vdf$val),
                                 by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH),
                                 FUN = function(x) agg_map[[var]](x, na.rm = TRUE))

    out <- merge(merge(expected, missing, by = c("YEAR","MONTH"), all = TRUE),
                 value, by = c("YEAR","MONTH"), all = TRUE)
    out$missing_frac <- with(out, ifelse(expected_days > 0, missing_days / expected_days, NA_real_))
    out$value[out$missing_frac > max_missing_frac] <- NA_real_
    out$date <- as.Date(sprintf("%d-%02d-01", out$YEAR, out$MONTH))
    out$variable <- var
    out[order(out$YEAR, out$MONTH), c("date","variable","value")]
  }

  monthly_list <- lapply(variables, monthly_one)
  monthly_all  <- do.call(rbind, monthly_list)
  rownames(monthly_all) <- NULL

  # ---- LONG format (updated: one row per month, columns per variable) -------
  if (output_format == "long") {
    # pivot wider using base reshape
    df <- monthly_all[, c("date","variable","value")]
    wide <- reshape(df, idvar = "date", timevar = "variable", direction = "wide")
    # rename value.<var> -> <var>
    names(wide) <- sub("^value\\.", "", names(wide))
    # ensure only requested vars, in desired order
    keep_vars <- intersect(variables, c("prec","evap","tmin","tmax"))
    for (v in setdiff(keep_vars, names(wide))) wide[[v]] <- NA_real_
    wide <- wide[, c("date", keep_vars), drop = FALSE]
    # attach metadata columns
    wide$station   <- st_chr
    wide$latitude  <- lat
    wide$longitude <- lon
    wide$altitude  <- alt
    # order columns: station, lat, lon, alt, date, <vars...>
    wide <- wide[, c("station","latitude","longitude","altitude","date", keep_vars), drop = FALSE]
    wide <- wide[order(wide$date), , drop = FALSE]
    rownames(wide) <- NULL

    if (!is.null(csv_file)) {
      utils::write.csv(wide, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }
    return(wide)
  }

  # ---- REDUCE format (unchanged) --------------------------------------------
  month_labels <- c("JAN","FEB","MAR","APR","MAY","JUN",
                    "JUL","AUG","SEP","OCT","NOV","DEC")

  reduce_one <- function(var) {
    df <- monthly_all[monthly_all$variable == var, c("date","value")]
    if (!nrow(df)) {
      return(data.frame(YEAR = integer(), matrix(numeric(), nrow = 0, ncol = 12,
                                                 dimnames = list(NULL, month_labels)),
                        ACUM = numeric(), PROM = numeric(), MONTHS = integer()))
    }
    df$YEAR  <- as.integer(format(df$date, "%Y"))
    df$MONTH <- factor(as.integer(format(df$date, "%m")), levels = 1:12, labels = month_labels)

    wide <- reshape(df[, c("YEAR","MONTH","value")], idvar = "YEAR", timevar = "MONTH", direction = "wide")
    names(wide) <- sub("^value\\.", "", names(wide))
    for (m in setdiff(month_labels, names(wide))) wide[[m]] <- NA_real_
    wide <- wide[, c("YEAR", month_labels), drop = FALSE]

    is_sum <- var %in% c("prec","evap")
    month_mat <- as.matrix(wide[, month_labels, drop = FALSE])
    ACUM   <- if (is_sum) rowSums(month_mat, na.rm = TRUE) else rowMeans(month_mat, na.rm = TRUE)
    PROM   <- rowMeans(month_mat, na.rm = TRUE)
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
