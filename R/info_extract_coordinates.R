#' Extract Coordinates for Multiple Stations
#'
#' Iterates over a vector of station codes and retrieves coordinates using
#' [smn_int_extract_coordinates()]. If extraction fails for any station, the
#' row is filled with `NA` values and a warning is emitted.
#'
#' @param stations Character or numeric vector of station codes.
#' @param show_progress Logical. Show a text progress bar when interactive
#'   (default `interactive()`).
#' @param ... Optional arguments passed to [smn_int_extract_coordinates()],
#'   e.g. `max_attempts`, `encoding`, `add_meta_cols = TRUE`.
#'
#' @return A base `data.frame` with columns:
#'   `station`, `latitude`, `longitude`, `altitude`. If you pass
#'   `add_meta_cols = TRUE` in `...`, extra columns from
#'   `smn_int_extract_coordinates()` (e.g. `source_url`) are included too.
#'
#' @seealso [smn_int_extract_coordinates()]
#'
#' @examples
#' \dontrun{
#' stations <- c(15020, 15171, 12345)  # 12345 likely fails
#' coords_df <- smn_info_extract_coordinates(stations)
#' head(coords_df)
#' }
#'
#' @export
smn_info_extract_coordinates <- function(stations,
                                         show_progress = interactive(),
                                         ...) {
  # ---- validate --------------------------------------------------------------
  if (missing(stations) || length(stations) == 0L) {
    stop("`stations` must be a non-empty vector of station codes.")
  }

  stations <- as.character(stations)
  stations <- trimws(stations)
  stations <- stations[!is.na(stations) & nzchar(stations)]
  stations <- unique(stations)

  if (length(stations) == 0L) {
    # return empty data.frame with expected columns
    out <- data.frame(
      station   = character(),
      latitude  = numeric(),
      longitude = numeric(),
      altitude  = numeric(),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  # ---- progress bar (only when useful) --------------------------------------
  show_pb <- isTRUE(show_progress) && length(stations) > 1L
  if (show_pb) {
    pb <- utils::txtProgressBar(min = 0, max = length(stations), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  # ---- iterate safely --------------------------------------------------------
  res_list <- vector("list", length(stations))

  for (i in seq_along(stations)) {
    st <- stations[[i]]

    coords <- tryCatch(
      smn_int_extract_coordinates(st, ...),
      error = function(e) {
        warning("Failed to extract data for station ", st, ": ", conditionMessage(e))
        # NA-row compatible con la interfaz por omisión (sin meta cols)
        data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
      }
    )

    # --
    if (!all(c("latitude","longitude","altitude") %in% names(coords))) {
      coords <- data.frame(latitude = NA_real_, longitude = NA_real_, altitude = NA_real_)
    }

    row <- data.frame(
      station   = st,
      latitude  = suppressWarnings(as.numeric(coords$latitude))[1],
      longitude = suppressWarnings(as.numeric(coords$longitude))[1],
      altitude  = suppressWarnings(as.numeric(coords$altitude))[1],
      stringsAsFactors = FALSE
    )

    # --
    extra_cols <- setdiff(names(coords), c("latitude","longitude","altitude"))
    if (length(extra_cols)) {
      for (cn in extra_cols) row[[cn]] <- coords[[cn]][1]
    }

    res_list[[i]] <- row
    if (show_pb) utils::setTxtProgressBar(pb, i)
  }

  out <- do.call(rbind, res_list)
  rownames(out) <- NULL
  out
}

