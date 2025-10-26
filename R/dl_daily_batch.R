#' Batch Download of Daily Climate Data (SMN, NASA, or Hybrid)
#'
#' @inheritParams smn_dl_daily_single
#' @param stations Character/numeric vector of station codes, or a data frame with
#'   at least `station`; for NASA/Hybrid, if `latitude`, `longitude`, `altitude`
#'   are present they will be used directly.
#' @param source One of `"smn"`, `"nasa"`, `"hybrid"`, or a custom handler in `.handlers`.
#' @param vars Character vector of NASA POWER parameter codes (used in `"nasa"`/`"hybrid"`).
#' @param community POWER community for `"nasa"`/`"hybrid"`; one of `"AG","RE","SB"` (default `"AG"`).
#' @param return_list Logical; if `TRUE` returns list per station, else a row-bound data.frame.
#' @param .handlers Optional named list of custom handlers (same signature used internally).
#' @param require_coords Logical. For NASA/Hybrid: resolve coordinates when missing in `stations`.
#' @param .progress Logical. Show a simple text progress bar while downloading.
#' @export
#' @importFrom stats setNames reshape
#' @importFrom utils data
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

    # 3) fusion
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
      # rellena encabezado de vars NASA como NA para mantener esquema híbrido
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

  # ---- ensamblado final dependiente de `source`
  non_empty <- out_list[vapply(out_list, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]

  expected_cols <- switch(
    source,
    "smn" = if (identical(output_format, "full"))
      c("station","latitude","longitude","altitude","date","prec","evap","tmax","tmin")
    else
      c("date","prec","evap","tmax","tmin"),
    "nasa" = c("station","latitude","longitude","altitude","date", vars),
    "hybrid" = c("station","latitude","longitude","altitude","date","prec","evap","tmax","tmin", vars),
    # fallback (no debería alcanzarse)
    c("station","latitude","longitude","altitude","date")
  )

  if (!length(non_empty)) {
    out <- setNames(replicate(length(expected_cols), logical(0)), expected_cols)
    out <- as.data.frame(out, check.names = FALSE)
    if ("date" %in% names(out)) out$date <- as.Date(out$date)
    return(out)
  }

  out <- tryCatch(dplyr::bind_rows(non_empty), error = function(e) do.call(rbind, non_empty))

  # recorta/ordena según `source`
  keep <- intersect(expected_cols, names(out))
  out  <- out[, keep, drop = FALSE]

  if ("date" %in% names(out)) {
    if ("station" %in% names(out)) out <- out[order(out$station, out$date), , drop = FALSE]
    else                           out <- out[order(out$date), , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}
