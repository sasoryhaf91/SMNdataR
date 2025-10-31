#' Batch Download of Daily Climate Data (SMN, NASA POWER, or Hybrid)
#'
#' Downloads daily meteorological series for **multiple stations** from:
#' \itemize{
#'   \item \strong{SMN} (official in-situ records),
#'   \item \strong{NASA POWER} (satellite/reanalysis),
#'   \item \strong{Hybrid}: joins SMN (full) + NASA POWER variables by \code{station + date}.
#' }
#' It harmonizes output columns (station metadata, date, core SMN variables, and optional
#' NASA POWER variables) and returns either a single tidy data frame (row-bound)
#' or a list per station.
#'
#' @inheritParams smn_dl_daily_single
#' @param stations Character/numeric vector of station codes \emph{or} a data frame
#'   with at least a \code{station} column. For \code{source = "nasa"} or \code{"hybrid"},
#'   if \code{latitude}, \code{longitude}, \code{altitude} are present, they will be used
#'   directly; otherwise the function will try to resolve them (see \code{require_coords}).
#' @param source One of \code{"smn"}, \code{"nasa"}, \code{"hybrid"}, or a custom
#'   key present in \code{.handlers}.
#' @param vars Character vector with NASA POWER parameter codes used in
#'   \code{source = "nasa"} or \code{"hybrid"} (e.g., \code{c("T2M_MIN","T2M_MAX","PRECTOTCORR")}).
#' @param community NASA POWER community for \code{"nasa"}/\code{"hybrid"}; one of
#'   \code{"AG"}, \code{"RE"}, \code{"SB"} (default \code{"AG"}).
#' @param return_list Logical; if \code{TRUE}, returns a named list (one element per station).
#'   If \code{FALSE} (default), returns a single tidy data frame with all rows bound.
#' @param .handlers Optional named list of custom handlers with signature
#'   \code{function(station, start_date, end_date, output_format, del_na, row, vars, community)}.
#' @param require_coords Logical. For \code{"nasa"}/\code{"hybrid"}: attempt to resolve
#'   missing coordinates using \code{smn_int_extract_coordinates()} when \code{latitude/longitude}
#'   are not provided in \code{stations}.
#' @param .progress Logical. Show a simple text progress bar while downloading.
#'
#' @details
#' \strong{Output columns} depend on \code{source}:
#' \itemize{
#'   \item \code{source = "smn"}: \code{station, latitude, longitude, altitude, date, prec, evap, tmax, tmin}
#'     (or reduced set if \code{output_format = "reduced"}).
#'   \item \code{source = "nasa"}: \code{station, latitude, longitude, altitude, date} + requested \code{vars}.
#'   \item \code{source = "hybrid"}: union of SMN core variables + requested NASA \code{vars}.
#' }
#' If some branch fails for a given station, the function continues and returns what is available
#' (empty data frame for that station), warning as needed. When \code{return_list = FALSE},
#' results are row-bound in the end, keeping a consistent column order per \code{source}.
#'
#' @return
#' A \strong{data frame} (default) with all stations row-bound in chronological order per station,
#' or a \strong{named list} of per-station data frames when \code{return_list = TRUE}.
#'
#' @section Custom handlers:
#' You can extend sources via \code{.handlers}, e.g.:
#' \preformatted{
#'   my_h <- list(
#'     era5 = function(station, start_date, end_date, output_format, del_na, row, vars, community) {
#'       # ... your downloader; must return a data.frame with at least
#'       # station, date, latitude, longitude, altitude, and variables
#'     }
#'   )
#'   smn_dl_daily_batch(stations = c("15021","15101"),
#'                      source = "era5",
#'                      start_date = "2000-01-01", end_date = "2000-01-10",
#'                      .handlers = my_h)
#' }
#'
#' @seealso
#' \code{\link{smn_dl_daily_single}}, \code{\link{smn_dl_daily_nasa}},
#' \code{\link{smn_int_extract_coordinates}}
#'
#' @export
#' @importFrom utils data txtProgressBar setTxtProgressBar
#' @examples
#' \dontrun{
#' # --- Minimal examples (short date window) ---
#'
#' # 1) SMN only (two stations, short period)
#' df_smn <- smn_dl_daily_batch(
#'   stations = c("15021","15101"),
#'   source   = "smn",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-01-05",
#'   output_format = "full",
#'   del_na = "no",
#'   .progress = TRUE
#' )
#' head(df_smn)
#'
#' # 2) NASA POWER only — if you already know coordinates for each station
#' st <- data.frame(
#'   station   = c("X15021","X15101"),
#'   latitude  = c(19.35, 19.70),
#'   longitude = c(-99.10, -99.20),
#'   altitude  = c(2250, 2400)
#' )
#' df_nasa <- smn_dl_daily_batch(
#'   stations = st,
#'   source   = "nasa",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-01-03",
#'   vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
#'   community = "AG",
#'   .progress = FALSE
#' )
#' head(df_nasa)
#'
#' # 3) HYBRID (SMN + NASA POWER merged by station/date)
#' df_hyb <- smn_dl_daily_batch(
#'   stations = c("15021","15101"),
#'   source   = "hybrid",
#'   start_date = "2020-01-01",
#'   end_date   = "2020-01-05",
#'   vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
#'   .progress = TRUE
#' )
#' head(df_hyb)
#' }
smn_dl_daily_batch <- function(stations,
                               source = c("smn","nasa","hybrid"),
                               start_date = "1961-01-01",
                               end_date   = Sys.Date(),
                               output_format = c("full","reduced"),
                               del_na = c("no","yes"),
                               require_coords = TRUE,
                               vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
                               community = c("AG","RE","SB"),
                               return_list = FALSE,
                               .handlers = NULL,
                               .progress = TRUE) {
  output_format <- match.arg(output_format)
  del_na        <- match.arg(del_na)
  community     <- match.arg(community)
  if (!is.list(.handlers)) .handlers <- list()
  source <- match.arg(source, choices = c("smn","nasa","hybrid", names(.handlers)))

  # normalize stations -> data.frame
  if (is.data.frame(stations)) {
    if (!"station" %in% names(stations))
      stop("When `stations` is a data.frame it must contain column `station`.")
    st_df <- stations
  } else {
    st_df <- data.frame(station = as.character(stations), stringsAsFactors = FALSE)
  }
  st_df <- st_df[!duplicated(st_df$station), , drop = FALSE]

  # dates
  sdate <- tryCatch(as.Date(start_date), error = function(e) NA)
  edate <- tryCatch(as.Date(end_date),   error = function(e) NA)
  if (is.na(sdate) || is.na(edate)) stop("`start_date` and `end_date` must be coercible to Date.")
  if (edate < sdate) stop("`end_date` must be on or after `start_date`.")

  # registry of sources
  reg <- list()

  # ---- SMN only
  reg$smn <- function(station, start_date, end_date, output_format, del_na, row, vars, community) {
    smn_dl_daily_single(
      station       = station,
      start_date    = start_date,
      end_date      = end_date,
      output_format = output_format,
      del_na        = del_na
    )
  }

  # ---- NASA only (uses coords from DF or resolves them)
  reg$nasa <- function(station, start_date, end_date, output_format, del_na, row, vars, community) {
    lat <- if (!is.null(row) && "latitude"  %in% names(row)) suppressWarnings(as.numeric(row$latitude))  else NA_real_
    lon <- if (!is.null(row) && "longitude" %in% names(row)) suppressWarnings(as.numeric(row$longitude)) else NA_real_
    alt <- if (!is.null(row) && "altitude" %in% names(row)) suppressWarnings(as.numeric(row$altitude)) else NA_real_

    if ((is.na(lat) || is.na(lon)) && isTRUE(require_coords)) {
      coords <- tryCatch(
        smn_int_extract_coordinates(station),
        error = function(e) data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
      )
      lat <- coords$latitude[1]; lon <- coords$longitude[1]
      if (is.na(alt) && "altitude" %in% names(coords)) alt <- coords$altitude[1]
      if (is.na(lat) || is.na(lon)) {
        warning("Skipping station ", station, " for NASA: coordinates unavailable.")
        return(data.frame())
      }
    }

    smn_dl_daily_nasa(
      station      = station,
      lat          = lat,
      lon          = lon,
      altitude     = alt,
      start_date   = start_date,
      end_date     = end_date,
      vars         = vars,
      community    = community,
      output_format = "all"
    )
  }

  # ---- HYBRID: SMN (full) + NASA (same coords) -> merge by station+date
  reg$hybrid <- function(station, start_date, end_date, output_format, del_na, row, vars, community) {
    # 1) SMN full
    smn_df <- tryCatch(
      smn_dl_daily_single(
        station       = station,
        start_date    = start_date,
        end_date      = end_date,
        output_format = "full",
        del_na        = del_na
      ),
      error = function(e) { warning("SMN failed for ", station, ": ", conditionMessage(e)); data.frame() }
    )

    # base coords
    lat <- if (!is.null(row) && "latitude"  %in% names(row)) suppressWarnings(as.numeric(row$latitude))  else NA_real_
    lon <- if (!is.null(row) && "longitude" %in% names(row)) suppressWarnings(as.numeric(row$longitude)) else NA_real_
    alt <- if (!is.null(row) && "altitude" %in% names(row)) suppressWarnings(as.numeric(row$altitude)) else NA_real_

    if (is.data.frame(smn_df) && nrow(smn_df)) {
      if ("latitude" %in% names(smn_df))  lat <- smn_df$latitude[1]
      if ("longitude" %in% names(smn_df)) lon <- smn_df$longitude[1]
      if ("altitude" %in% names(smn_df) && is.na(alt)) alt <- smn_df$altitude[1]
    }
    if ((is.na(lat) || is.na(lon)) && isTRUE(require_coords)) {
      coords <- tryCatch(
        smn_int_extract_coordinates(station),
        error = function(e) data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
      )
      if (is.na(lat)) lat <- coords$latitude[1]
      if (is.na(lon)) lon <- coords$longitude[1]
      if (is.na(alt) && "altitude" %in% names(coords)) alt <- coords$altitude[1]
    }

    # 2) NASA
    nasa_df <- data.frame()
    if (!is.na(lat) && !is.na(lon)) {
      nasa_df <- tryCatch(
        smn_dl_daily_nasa(
          station    = station,
          lat        = lat,
          lon        = lon,
          altitude   = alt,
          start_date = start_date,
          end_date   = end_date,
          vars       = vars,
          community  = community,
          output_format = "all"
        ),
        error = function(e) { warning("NASA failed for ", station, ": ", conditionMessage(e)); data.frame() }
      )
    } else {
      warning("HYBRID: coordinates unavailable for station ", station, " (skipping NASA).")
    }

    # 3) merge/fuse
    if (nrow(smn_df) > 0L && nrow(nasa_df) > 0L) {
      nasa_keep <- c("station","date", intersect(vars, names(nasa_df)))
      nasa_df2  <- nasa_df[, nasa_keep, drop = FALSE]
      if (!inherits(smn_df$date, "Date")) smn_df$date <- as.Date(smn_df$date)
      if (!inherits(nasa_df2$date, "Date")) nasa_df2$date <- as.Date(nasa_df2$date)
      out <- merge(smn_df, nasa_df2, by = c("station","date"), all.x = TRUE, sort = FALSE)

      smn_vars  <- intersect(c("prec","evap","tmax","tmin"), names(out))
      front     <- c("station","latitude","longitude","altitude","date")
      cols <- unique(c(front, smn_vars, intersect(vars, names(out))))
      out  <- out[, cols, drop = FALSE]
      out  <- out[order(out$date), , drop = FALSE]
      rownames(out) <- NULL
      return(out)

    } else if (nrow(smn_df) > 0L) {
      smn_vars  <- intersect(c("prec","evap","tmax","tmin"), names(smn_df))
      front     <- c("station","latitude","longitude","altitude","date")
      for (m in setdiff(vars, names(smn_df))) smn_df[[m]] <- NA_real_
      cols <- unique(c(front, smn_vars, vars))
      out <- smn_df[, intersect(cols, names(smn_df)), drop = FALSE]
      out <- out[order(out$date), , drop = FALSE]
      rownames(out) <- NULL
      return(out)

    } else if (nrow(nasa_df) > 0L) {
      smn_shell <- c("prec","evap","tmax","tmin")
      for (m in smn_shell) if (!m %in% names(nasa_df)) nasa_df[[m]] <- NA_real_
      front <- c("station","latitude","longitude","altitude","date")
      cols  <- c(front, smn_shell, vars)
      out   <- nasa_df[, intersect(cols, names(nasa_df)), drop = FALSE]
      out   <- out[order(out$date), , drop = FALSE]
      rownames(out) <- NULL
      return(out)
    }

    data.frame()
  }

  # custom handlers
  if (length(.handlers)) reg[names(.handlers)] <- .handlers
  if (is.null(reg[[source]]))
    stop("Unknown `source`: ", source, ". Provide a handler via `.handlers`.")
  handler <- reg[[source]]

  # iterate stations
  n <- nrow(st_df)
  out_list <- vector("list", n)
  names(out_list) <- st_df$station
  pb <- if (.progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

  for (i in seq_len(n)) {
    st <- as.character(st_df$station[i])
    row_i <- st_df[i, , drop = FALSE]
    res <- tryCatch(
      handler(station = st, start_date = sdate, end_date = edate,
              output_format = output_format, del_na = del_na,
              row = row_i, vars = vars, community = community),
      error = function(e) { warning("Failed for station ", st, " [", source, "]: ", conditionMessage(e)); data.frame() }
    )
    out_list[[i]] <- res
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }
  if (!is.null(pb)) close(pb)

  if (isTRUE(return_list)) return(out_list)

  # assemble final table according to `source`
  non_empty <- out_list[vapply(out_list, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]

  expected_cols <- switch(
    source,
    "smn" = if (identical(output_format, "full"))
      c("station","latitude","longitude","altitude","date","prec","evap","tmax","tmin")
    else
      c("date","prec","evap","tmax","tmin"),
    "nasa" = c("station","latitude","longitude","altitude","date", vars),
    "hybrid" = c("station","latitude","longitude","altitude","date","prec","evap","tmax","tmin", vars),
    c("station","latitude","longitude","altitude","date")
  )

  if (!length(non_empty)) {
    out <- stats::setNames(replicate(length(expected_cols), logical(0)), expected_cols)
    out <- as.data.frame(out, check.names = FALSE)
    if ("date" %in% names(out)) out$date <- as.Date(out$date)
    return(out)
  }

  # bind rows robustly
  out <- tryCatch(dplyr::bind_rows(non_empty), error = function(e) do.call(rbind, non_empty))

  keep <- intersect(expected_cols, names(out))
  out  <- out[, keep, drop = FALSE]

  if ("date" %in% names(out)) {
    if ("station" %in% names(out)) out <- out[order(out$station, out$date), , drop = FALSE]
    else                           out <- out[order(out$date), , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}
