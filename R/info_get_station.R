#' Get Station Metadata
#'
#' Retrieves station metadata for one or multiple station IDs, or from a data
#' frame containing a station id column. The function internally queries
#' `smn_int_get_station()` for each id and binds results row-wise.
#'
#' @param input Either
#'   - a single station id (character or numeric),
#'   - a vector of station ids, or
#'   - a `data.frame` containing a station id column named **`CLAVE`** or **`station`**.
#'
#' @return A base `data.frame` with the metadata rows for the requested stations.
#'   If no id could be resolved or none matched, an **empty** `data.frame`
#'   (0 rows) with the same columns as `SMNdataR::stations` is returned.
#'
#' @examples
#' \dontrun{
#' smn_info_get_station("15101")
#' smn_info_get_station(c("15101","15021"))
#' smn_info_get_station(data.frame(CLAVE = c("15101","15021")))
#' }
#'
#' @export
#' @importFrom stats setNames reshape
#' @importFrom utils data
smn_info_get_station <- function(input) {
  # -- load reference dataset (for schema and empty-return template) ----------
  utils::data("stations", package = "SMNdataR", envir = environment())
  if (!exists("stations", inherits = FALSE) || !is.data.frame(stations)) {
    stop("Dataset `stations` could not be loaded from package 'SMNdataR'.")
  }
  if (!("station" %in% names(stations))) {
    stop("Dataset `stations` does not contain a 'station' column.")
  }

  # -- resolve station ids -----------------------------------------------------
  station_ids <- NULL

  if (is.data.frame(input)) {
    nm <- names(input)
    if ("CLAVE" %in% nm) {
      station_ids <- input[["CLAVE"]]
    } else if ("station" %in% nm) {
      station_ids <- input[["station"]]
    } else {
      stop("Input data frame must contain a 'CLAVE' or 'station' column.")
    }
  } else {
    station_ids <- input
  }

  # Coerce to character, drop NA/empty, deduplicate
  station_ids <- as.character(station_ids)
  station_ids <- trimws(station_ids)
  station_ids <- station_ids[!is.na(station_ids) & nzchar(station_ids)]
  station_ids <- unique(station_ids)

  # No ids → return empty with correct schema
  if (length(station_ids) == 0L) {
    out <- stations[FALSE, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  # -- iterate with a lightweight progress bar --------------------------------
  show_pb <- interactive() && length(station_ids) > 2L
  if (show_pb) {
    pb <- utils::txtProgressBar(min = 0, max = length(station_ids), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  res_list <- vector("list", length(station_ids))
  for (i in seq_along(station_ids)) {
    if (show_pb) utils::setTxtProgressBar(pb, i)
    res_list[[i]] <- tryCatch(
      smn_int_get_station(station_ids[[i]]),
      error = function(e) {
        # On error, return empty row set with correct columns
        stations[FALSE, , drop = FALSE]
      }
    )
  }

  # bind rows deterministically (base R)
  out <- do.call(rbind, res_list)
  rownames(out) <- NULL
  out
}

