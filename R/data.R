#' SMN station metadata (Mexico)
#'
#' A data frame with station metadata compiled for SMNdataR.
#'
#' @format A data frame with one row per station and the following variables:
#' \describe{
#'   \item{station}{Character. SMN station code.}
#'   \item{name}{Character. Station name.}
#'   \item{latitude}{Numeric. Decimal degrees (WGS84).}
#'   \item{longitude}{Numeric. Decimal degrees (WGS84).}
#'   \item{altitude}{Numeric. Elevation in meters.}
#'   \item{Clicom}{Character. CLICOM identifier (if available).}
#'   \item{Referencia}{Character. Reference field.}
#'   \item{SMN}{Character. Internal SMN id (if available).}
#'   \item{cuenca}{Character. Basin name/identifier.}
#'   \item{cve_sta}{Character. Station code alternative.}
#'   \item{municipality}{Character. Municipality name.}
#'   \item{org_cuenca}{Character. Basin organization.}
#'   \item{organism}{Character. Operating organism.}
#'   \item{situation}{Character. Station situation/status.}
#'   \item{subcuenca}{Character. Sub-basin.}
#'   \item{type}{Character. Station type/class.}
#' }
#' @source SMN and compiled sources.
"stations"
#' National 1-km grid for Mexico with altitude
#'
#' @description 1-km lattice nodes covering Mexico with altitude.
#' @format A data frame with columns:
#' \describe{
#'   \item{station}{character: synthetic node id}
#'   \item{latitude}{numeric, WGS84}
#'   \item{longitude}{numeric, WGS84}
#'   \item{altitude}{numeric, meters}
#' }
#' @details Created from (put DEM source).
#' @source (DEM source / processing steps)
"gridmex"
