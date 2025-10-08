#' Download and Aggregate Monthly Climate Data (Single Station)
#'
#' Downloads daily SMN data for a station using [smn_dl_daily_single()], then
#' aggregates it to monthly values for one or more variables applying a
#' completeness rule per month. The function can return either a *long* table
#' (one row per month and a column per requested variable), or *reduce* tables
#' (one per variable, with `YEAR`, `JAN..DEC`, and annual summaries).
#'
#' ## Long format
#' If `output_format = "long"`, the result contains one row per month with
#' columns:
#' `station, latitude, longitude, altitude, date, <vars...>`,
#' where `<vars...>` are the requested subset among `prec, evap, tmin, tmax`.
#' The `date` column is the first day of each month.
#'
#' ## Reduce format
#' If `output_format = "reduce"`, the result is a wide table (or a named list of
#' tables when multiple variables are requested) with columns:
#' `YEAR, JAN..DEC, ACUM, PROM, MONTHS`.
#' For sum-type variables (`prec`, `evap`) `ACUM` is the row **sum**;
#' for mean-type variables (`tmax`, `tmin`) `ACUM` is the row **mean**.
#' `PROM` is always the row mean across valid months; `MONTHS` is the count of
#' non-`NA` months.
#'
#' @param station Character or numeric. SMN station code (passed to
#'   [smn_dl_daily_single()]).
#' @param variables Character vector of variables to aggregate, or `"all"`.
#'   Valid names are `"prec"`, `"evap"`, `"tmax"`, `"tmin"`. Default `"all"`.
#' @param start_date Start date (inclusive). Coerced with `as.Date()`.
#'   Default `"1961-01-01"`. Passed to [smn_dl_daily_single()].
#' @param end_date End date (inclusive). Coerced with `as.Date()`.
#'   Default `Sys.Date()`. Passed to [smn_dl_daily_single()].
#' @param max_missing_frac Numeric in `[0, 1]`. If the fraction of missing days
#'   in a month exceeds this threshold, the monthly value is set to `NA`.
#'   Default `0.2`.
#' @param output_format Output format. Either `"long"` or `"reduce"`
#'   (default `"reduce"`).
#' @param aggregator Optional **single function** to apply to all variables
#'   (e.g., `median`) **or** a **named list** mapping variable → function,
#'   e.g., `list(prec = sum, tmax = mean)`. If `NULL`, defaults are used:
#'   `sum` for `prec,evap` and `mean` for `tmax,tmin`.
#' @param csv_file Optional path to write CSV output.
#'   - For `"long"`: a single CSV at `csv_file`.
#'   - For `"reduce"` with multiple variables: one file per variable, using
#'     `paste0(file_path_sans_ext(csv_file), "_", var, ".csv")`.
#' @param daily_del_na Character `"yes"` or `"no"`; forwarded to
#'   [smn_dl_daily_single()] to control row removal at the **daily** stage.
#'   Default `"no"`. Note: the monthly completeness rule is applied **after**
#'   downloading daily data; this argument is only about cleaning rows at the
#'   daily function.
#'
#' @return
#' - If `output_format = "long"`: a single `data.frame` with columns
#'   `station, latitude, longitude, altitude, date, <vars...>`.
#' - If `output_format = "reduce"` and a single variable was requested:
#'   a wide `data.frame`.
#' - If `output_format = "reduce"` and multiple variables were requested:
#'   a **named list** of wide `data.frame`s (one per variable).
#'
#' @details
#' Daily data are obtained via [smn_dl_daily_single()] with
#' `output_format = "full"` (to carry station metadata). Missing fraction is
#' computed against the **calendar** number of days per month within the
#' requested window; absent daily rows count as missing. The `date` in the long
#' table is set to the first day of each month (`YYYY-MM-01`).
#'
#' @examples
#' \dontrun{
#' # 1) All variables, monthly, long format (one row per month)
#' m_long <- smn_dl_monthly_single(
#'   station = "15101",
#'   variables = "all",
#'   start_date = "1984-01-01",
#'   end_date   = "2020-12-31",
#'   output_format = "long"
#' )
#'
#' # 2) Only precipitation, reduce format (wide)
#' m_prec <- smn_dl_monthly_single("15101", variables = "prec", output_format = "reduce")
#'
#' # 3) Two variables, strict completeness (<= 10% missing), custom aggregator
#' m_custom <- smn_dl_monthly_single(
#'   station = "15101",
#'   variables = c("tmax","tmin"),
#'   max_missing_frac = 0.10,
#'   aggregator = median,
#'   output_format = "long"
#' )
#'
#' # 4) Control daily cleaning from here (do not drop NAs before monthly)
#' m_cfg <- smn_dl_monthly_single(
#'   station = "15101",
#'   variables = "all",
#'   daily_del_na = "no",
#'   output_format = "long"
#' )
#' }
#'
#' @export
smn_dl_monthly_single <- function(station,
                                  variables = "all",
                                  start_date = "1961-01-01",
                                  end_date   = Sys.Date(),
                                  max_missing_frac = 0.2,
                                  output_format = c("reduce", "long"),
                                  aggregator = NULL,
                                  csv_file   = NULL,
                                  daily_del_na = "no") {
  output_format <- match.arg(output_format)

  # ---- get daily (force "full" to carry metadata) ---------------------------
  daily <- smn_dl_daily_single(
    station      = station,
    start_date   = start_date,
    end_date     = end_date,
    output_format = "full",
    del_na        = daily_del_na
  )
  if (!nrow(daily)) {
    warning("No daily data available for station ", station, " in the specified date range.")
    return(if (output_format == "long") data.frame() else data.frame())
  }

  # ---- variables: validate & normalize --------------------------------------
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
  md_ix <- stats::complete.cases(daily[, c("latitude","longitude","altitude")])
  meta_row <- if (any(md_ix)) daily[which(md_ix)[1], , drop = FALSE] else daily[1, , drop = FALSE]
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

  # ---- calendar to count missing days properly ------------------------------
  win_start <- min(as.Date(daily$date), na.rm = TRUE)
  win_end   <- max(as.Date(daily$date),  na.rm = TRUE)
  cal <- data.frame(date = seq(win_start, win_end, by = "day"))

  # ---- per-variable monthly aggregation -------------------------------------
  monthly_one <- function(var) {
    vdf <- merge(cal, daily[, c("date", var), drop = FALSE], by = "date", all.x = TRUE)
    names(vdf)[2] <- "val"
    vdf$YEAR  <- as.integer(format(vdf$date, "%Y"))
    vdf$MONTH <- as.integer(format(vdf$date, "%m"))

    expected <- stats::aggregate(list(expected_days = vdf$val),
                                 by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = length)
    missing  <- stats::aggregate(list(missing_days = is.na(vdf$val)),
                                 by = list(YEAR = vdf$YEAR, MONTH = vdf$MONTH), FUN = sum)
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

  # ---- LONG format: one row/month; columns per requested variable -----------
  if (output_format == "long") {
    df <- monthly_all[, c("date","variable","value")]
    wide <- reshape(df, idvar = "date", timevar = "variable", direction = "wide")
    names(wide) <- sub("^value\\.", "", names(wide))

    # keep only requested vars (in a stable order)
    natural <- c("prec","evap","tmin","tmax")
    keep_vars <- intersect(natural, variables)
    for (v in setdiff(keep_vars, names(wide))) wide[[v]] <- NA_real_
    wide <- wide[, c("date", keep_vars), drop = FALSE]

    # attach metadata and order columns
    wide$station   <- st_chr
    wide$latitude  <- lat
    wide$longitude <- lon
    wide$altitude  <- alt
    wide <- wide[, c("station","latitude","longitude","altitude","date", keep_vars), drop = FALSE]
    wide <- wide[order(wide$date), , drop = FALSE]
    rownames(wide) <- NULL

    if (!is.null(csv_file)) {
      utils::write.csv(wide, file = csv_file, row.names = FALSE)
      message("CSV file saved at: ", csv_file)
    }
    return(wide)
  }

  # ---- REDUCE format: per-variable wide (yearly) ----------------------------
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
    mm <- as.matrix(wide[, month_labels, drop = FALSE])
    ACUM   <- if (is_sum) rowSums(mm, na.rm = TRUE) else rowMeans(mm, na.rm = TRUE)
    PROM   <- rowMeans(mm, na.rm = TRUE)
    MONTHS <- rowSums(!is.na(mm))
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
