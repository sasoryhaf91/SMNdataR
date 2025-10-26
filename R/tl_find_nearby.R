#' Find Nearby SMN Stations and/or NASA Grid Nodes (Hybrid)
#'
#' Given a reference point (station code or coordinates), return nearby nodes
#' from the SMN station catalog and/or a prebuilt NASA-like grid (e.g. 1-km DEM
#' grid) with great-circle distances. Two selection modes are supported:
#' \itemize{
#'   \item \code{mode = "radius"}: all nodes within \code{radius_km}
#'   \item \code{mode = "k"}: the \code{k} nearest nodes
#' }
#'
#' The function expects two datasets bundled in the package (or preloaded):
#' \itemize{
#'   \item \code{stations}: SMN catalog with columns \code{station, latitude, longitude, altitude} (altitude may be \code{NA})
#'   \item \code{gridmex}: Grid of candidate nodes with columns \code{station, latitude, longitude, altitude}
#' }
#'
#' @param station Optional. SMN station code to use as reference point.
#' @param lat,lon Optional numeric coordinates (decimal degrees WGS84) for the
#'   reference point. If \code{station} is not supplied, both \code{lat} and
#'   \code{lon} must be provided.
#' @param sources Character vector: \code{"smn"}, \code{"nasa"} or \code{"both"}
#'   (default). Controls which catalogs are queried.
#' @param mode Selection mode: \code{"radius"} (default) or \code{"k"}.
#' @param radius_km Numeric radius in kilometers (used when \code{mode="radius"}).
#'   Default 20.
#' @param k Integer number of nearest neighbors (used when \code{mode="k"}).
#'   Default 10.
#' @param bbox_pad_km Numeric padding (km) for a quick prefiltering bounding
#'   box before exact distances (speeds up large catalogs). Default 2 km.
#'
#' @return A data frame with rows for the nearby nodes and columns:
#'   \code{source} (factor: \code{"SMN"} or \code{"NASA_POWER"}),
#'   \code{station} (character ID; for grid is the numeric node code),
#'   \code{latitude}, \code{longitude}, \code{altitude},
#'   \code{distance_km} (great-circle distance to the reference point).
#'   Rows are ordered by \code{distance_km}.
#'
#' @details
#' The great-circle distance is computed with the haversine formula
#' via \pkg{geosphere}. A coarse degree bounding box
#' (\code{radius_km + bbox_pad_km})/111 is applied first for speed.
#'
#' If \code{station} (reference) is supplied, its coordinates are taken from
#' \code{stations}. If not found, an error is raised unless \code{lat/lon} are
#' provided.
#'
#' @examples
#' \dontrun{
#' # 1) 20 km around station 15101, combining SMN + Grid
#' smn_tl_find_nearby(station = "15101", mode = "radius", radius_km = 20)
#'
#' # 2) 15 vecinos más cercanos solo del grid, a coordenadas dadas
#' smn_tl_find_nearby(lat = 19.29, lon = -99.66, sources = "nasa",
#'                    mode = "k", k = 15)
#' }
#' @export
#' @importFrom stats setNames reshape
#' @importFrom utils data
smn_tl_find_nearby <- function(station = NULL,
                               lat = NULL, lon = NULL,
                               sources = c("both", "smn", "nasa"),
                               mode = c("radius", "k"),
                               radius_km = 20,
                               k = 10,
                               bbox_pad_km = 2) {

  # ---- match args ----
  sources <- match.arg(sources)
  mode    <- match.arg(mode)

  # ---- load catalogs if not already in memory ----
  if (!exists("stations", inherits = TRUE)) {
    suppressWarnings(try(data("stations", package = "SMNdataR", envir = environment()), silent = TRUE))
  }
  if (!exists("gridmex", inherits = TRUE)) {
    suppressWarnings(try(data("gridmex", package = "SMNdataR", envir = environment()), silent = TRUE))
  }

  # ---- resolve reference point ----
  if (!is.null(station) && nzchar(as.character(station))) {
    st_chr <- as.character(station)
    if (!exists("stations", inherits = TRUE) || !"station" %in% names(stations)) {
      stop("`stations` catalog not available to resolve station coordinates.")
    }
    ref <- stations[as.character(stations$station) == st_chr, c("latitude","longitude"), drop = FALSE]
    if (!nrow(ref)) {
      if (is.null(lat) || is.null(lon)) {
        stop("Station '", st_chr, "' not found in `stations` and no lat/lon provided.")
      }
      lat0 <- as.numeric(lat); lon0 <- as.numeric(lon)
    } else {
      lat0 <- as.numeric(ref$latitude[1]); lon0 <- as.numeric(ref$longitude[1])
    }
  } else {
    if (is.null(lat) || is.null(lon)) stop("Provide `station` or both `lat` and `lon`.")
    lat0 <- as.numeric(lat); lon0 <- as.numeric(lon)
  }

  if (!is.finite(lat0) || !is.finite(lon0)) stop("Reference coordinates must be finite numbers.")

  # ---- assemble candidate pool by source ----
  pools <- list()

  if (sources %in% c("both","smn")) {
    if (!exists("stations", inherits = TRUE)) stop("`stations` dataset not found.")
    smn_df <- stations[, intersect(c("station","latitude","longitude","altitude"), names(stations)), drop = FALSE]
    smn_df$source <- "SMN"
    pools$smn <- smn_df
  }

  if (sources %in% c("both","nasa")) {
    if (!exists("gridmex", inherits = TRUE)) stop("`gridmex` dataset not found.")
    nasa_df <- gridmex[, intersect(c("station","latitude","longitude","altitude"), names(gridmex)), drop = FALSE]
    nasa_df$source <- "NASA_POWER"
    pools$nasa <- nasa_df
  }

  cand <- do.call(rbind, pools)
  if (!nrow(cand)) return(data.frame(
    source = character(), station = character(),
    latitude = numeric(), longitude = numeric(),
    altitude = numeric(), distance_km = numeric(),
    stringsAsFactors = FALSE
  ))

  # ---- fast bbox prefilter (degrees) ----
  rad_full <- if (mode == "radius") radius_km else max(radius_km, bbox_pad_km)
  deg <- (rad_full + bbox_pad_km) / 111.0
  in_bb <- cand$latitude  >= (lat0 - deg) & cand$latitude  <= (lat0 + deg) &
    cand$longitude >= (lon0 - deg) & cand$longitude <= (lon0 + deg)
  sub <- cand[in_bb, , drop = FALSE]
  if (!nrow(sub)) sub <- cand  # fallback if bbox too tight in sparse areas

  # ---- distances (haversine) ----
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("Package 'geosphere' required. Please install it.")
  }
  d <- geosphere::distHaversine(
    p1 = cbind(sub$longitude, sub$latitude),
    p2 = matrix(c(lon0, lat0), nrow = nrow(sub), ncol = 2, byrow = TRUE)
  ) / 1000.0
  sub$distance_km <- as.numeric(d)

  # ---- select by mode ----
  if (mode == "radius") {
    out <- sub[sub$distance_km <= radius_km, , drop = FALSE]
  } else { # k-NN
    ord <- order(sub$distance_km)
    out <- sub[ord, , drop = FALSE]
    if (nrow(out) > k) out <- out[seq_len(k), , drop = FALSE]
  }

  if (!nrow(out)) {
    # return empty schema
    return(data.frame(
      source = character(), station = character(),
      latitude = numeric(), longitude = numeric(),
      altitude = numeric(), distance_km = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # ---- standardize types / columns & order ----
  out$station   <- as.character(out$station)
  out$source    <- factor(out$source, levels = c("SMN","NASA_POWER"))
  out <- out[, c("source","station","latitude","longitude","altitude","distance_km"), drop = FALSE]
  out <- out[order(out$distance_km), , drop = FALSE]
  rownames(out) <- NULL
  out
}

