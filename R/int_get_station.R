#' Get Metadata for a Single Station (internal)
#'
#' Retrieves the metadata row for a given station id from the package dataset
#' `stations`. The dataset is loaded from the package namespace (not from the
#' global environment) to ensure reproducibility during tests and checks.
#'
#' @param station_id Character or numeric station identifier.
#'
#' @return A base `data.frame` with the metadata for the station. If the station
#'   is not found, an empty `data.frame` (0 rows) is returned. Columns are those
#'   of `SMNdataR::stations`.
#'
#' @details
#' The comparison is performed on the `stations$station` column coerced to
#' character, and on `station_id` coerced to character as well. This avoids
#' mismatches due to numeric/character types. The function does not change the
#' column order or types of the underlying dataset.
#'
#' @keywords internal
#' @noRd
smn_int_get_station <- function(station_id) {
  # ---- validate input --------------------------------------------------------
  if (missing(station_id) || length(station_id) != 1L || is.na(station_id)) {
    stop("`station_id` must be a single non-NA value.")
  }

  # ---- load dataset from package namespace (not .GlobalEnv) ------------------
  # This loads into the current function environment to avoid global side effects.
  utils::data("stations", package = "SMNdataR", envir = environment())

  if (!exists("stations", inherits = FALSE)) {
    stop("Dataset `stations` could not be loaded from package 'SMNdataR'.")
  }
  if (!is.data.frame(stations)) {
    stop("Object `stations` is not a data.frame.")
  }
  if (!("station" %in% names(stations))) {
    stop("Dataset `stations` does not contain a 'station' column.")
  }

  # ---- normalize ids to character -------------------------------------------
  id_chr <- as.character(station_id)
  sta_chr <- as.character(stations[["station"]])

  # ---- filter (base R; deterministic) ---------------------------------------
  idx <- which(sta_chr == id_chr)

  if (length(idx) == 0L) {
    # return an empty data.frame with same columns
    out <- stations[FALSE, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  out <- stations[idx, , drop = FALSE]
  rownames(out) <- NULL
  out
}

