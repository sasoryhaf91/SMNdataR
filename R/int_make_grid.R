#' Build a local square grid around a point (km-based) and clip by radius
#' (robust CRS handling)
#' @export
smn_int_make_grid <- function(lat, lon,
                                cell_km   = 5,
                                radius_km = 20,
                                clip      = c("intersect", "centroid"),
                                crs_out   = 4326,
                                return    = c("polygons","centroids","both")) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it.", call. = FALSE)
  }

  clip   <- match.arg(clip)
  return <- match.arg(return)

  stopifnot(is.numeric(lat), length(lat) == 1,
            is.numeric(lon), length(lon) == 1,
            is.numeric(cell_km), cell_km > 0,
            is.numeric(radius_km), radius_km > 0)

  cell_m   <- cell_km   * 1000
  radius_m <- radius_km * 1000
  crs_wgs84 <- 4326

  # -- AEQD centrado en el punto (intento 1: con +type=crs)
  aeqd_str1 <- sprintf("+proj=aeqd +lat_0=%.10f +lon_0=%.10f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs",
                       lat, lon)
  crs_local <- try(sf::st_crs(aeqd_str1), silent = TRUE)

  # -- Fallback si tu PROJ no parsea lo anterior
  if (inherits(crs_local, "try-error") || is.na(crs_local$input)) {
    aeqd_str2 <- sprintf("+proj=aeqd +lat_0=%.10f +lon_0=%.10f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                         lat, lon)
    crs_local <- sf::st_crs(aeqd_str2)
  }
  if (is.na(crs_local$input)) {
    stop("No se pudo crear el CRS local AEQD. Actualiza PROJ/sf o prueba otro sistema.", call. = FALSE)
  }

  # -- Centro y buffer (en metros, CRS local)
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = crs_wgs84)
  pt_local <- sf::st_transform(pt, crs_local)
  circle_local <- sf::st_buffer(pt_local, dist = radius_m)

  # -- Extensión y grilla (alineada al tamaño de celda)
  floor_to <- function(x, base) base * floor(x / base)
  ceil_to  <- function(x, base) base * ceiling(x / base)
  bb  <- sf::st_bbox(circle_local)
  minx <- floor_to(bb["xmin"], cell_m)
  miny <- floor_to(bb["ymin"], cell_m)
  maxx <- ceil_to(bb["xmax"], cell_m)
  maxy <- ceil_to(bb["ymax"], cell_m)

  # polygon del bbox en CRS local (evita crs NA)
  bbox_poly <- sf::st_polygon(list(matrix(
    c(minx, miny,
      maxx, miny,
      maxx, maxy,
      minx, maxy,
      minx, miny), ncol = 2, byrow = TRUE)))
  bbox_sfc <- sf::st_sfc(bbox_poly, crs = crs_local)

  grid_local <- sf::st_make_grid(
    x        = bbox_sfc,
    cellsize = c(cell_m, cell_m),
    what     = "polygons",
    square   = TRUE
  )
  grid_local <- sf::st_sf(geometry = grid_local)

  # -- Clip: intersect o centroid
  if (clip == "intersect") {
    keep_lgl <- sf::st_intersects(grid_local, circle_local, sparse = FALSE)[,1]
  } else {
    keep_lgl <- sf::st_within(sf::st_centroid(grid_local), circle_local, sparse = FALSE)[,1]
  }
  grid_keep <- grid_local[keep_lgl, , drop = FALSE]

  if (nrow(grid_keep) == 0) {
    warning("No cells found with the given parameters.")
    if (return == "polygons")  return(grid_keep)
    if (return == "centroids") return(sf::st_sf(id=integer(), row=integer(), col=integer(),
                                                x_m=numeric(), y_m=numeric(),
                                                lon=numeric(), lat=numeric(),
                                                geometry=sf::st_sfc(), crs = crs_out))
    return(list(polygons = grid_keep, centroids = grid_keep[0,]))
  }

  # -- Índices y atributos
  cents_local <- sf::st_centroid(grid_keep)
  xy <- sf::st_coordinates(cents_local)
  row_idx <- as.integer(floor((xy[,2] - miny) / cell_m))
  col_idx <- as.integer(floor((xy[,1] - minx) / cell_m))

  grid_keep$row <- row_idx
  grid_keep$col <- col_idx
  grid_keep$id  <- seq_len(nrow(grid_keep))
  grid_keep$x_m <- xy[,1]
  grid_keep$y_m <- xy[,2]

  cents_ll <- sf::st_transform(cents_local, crs = crs_wgs84)
  ll <- sf::st_coordinates(cents_ll)
  grid_keep$lon <- ll[,1]
  grid_keep$lat <- ll[,2]

  grid_out <- sf::st_transform(grid_keep, crs = crs_out)

  centroids_out <- sf::st_as_sf(
    data.frame(
      id  = grid_out$id,
      row = grid_out$row,
      col = grid_out$col,
      x_m = grid_keep$x_m,  # metros del CRS local
      y_m = grid_keep$y_m,
      lon = grid_keep$lon,
      lat = grid_keep$lat
    ),
    geometry = sf::st_centroid(sf::st_geometry(grid_out)),
    crs = sf::st_crs(grid_out)
  )

  if (return == "polygons")  return(grid_out[ , c("id","row","col","x_m","y_m","lon","lat","geometry")])
  if (return == "centroids") return(centroids_out[ , c("id","row","col","x_m","y_m","lon","lat","geometry")])
  list(
    polygons  = grid_out[ , c("id","row","col","x_m","y_m","lon","lat","geometry")],
    centroids = centroids_out[ , c("id","row","col","x_m","y_m","lon","lat","geometry")]
  )
}

