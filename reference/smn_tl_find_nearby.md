# Find Nearby SMN Stations and/or NASA Grid Nodes (Hybrid)

Given a reference point (station code or coordinates), return nearby
nodes from the SMN station catalog and/or a prebuilt NASA-like grid
(e.g. 1-km DEM grid) with great-circle distances. Two selection modes
are supported:

- `mode = "radius"`: all nodes within `radius_km`

- `mode = "k"`: the `k` nearest nodes

## Usage

``` r
smn_tl_find_nearby(
  station = NULL,
  lat = NULL,
  lon = NULL,
  sources = c("both", "smn", "nasa"),
  mode = c("radius", "k"),
  radius_km = 20,
  k = 10,
  bbox_pad_km = 2
)
```

## Arguments

- station:

  Optional. SMN station code to use as reference point.

- lat, lon:

  Optional numeric coordinates (decimal degrees WGS84) for the reference
  point. If `station` is not supplied, both `lat` and `lon` must be
  provided.

- sources:

  Character vector: `"smn"`, `"nasa"` or `"both"` (default). Controls
  which catalogs are queried.

- mode:

  Selection mode: `"radius"` (default) or `"k"`.

- radius_km:

  Numeric radius in kilometers (used when `mode="radius"`). Default 20.

- k:

  Integer number of nearest neighbors (used when `mode="k"`). Default
  10.

- bbox_pad_km:

  Numeric padding (km) for a quick prefiltering bounding box before
  exact distances (speeds up large catalogs). Default 2 km.

## Value

A data frame with rows for the nearby nodes and columns: `source`
(factor: `"SMN"` or `"NASA_POWER"`), `station` (character ID; for grid
is the numeric node code), `latitude`, `longitude`, `altitude`,
`distance_km` (great-circle distance to the reference point). Rows are
ordered by `distance_km`.

## Details

The function expects two datasets bundled in the package (or preloaded):

- `stations`: SMN catalog with columns
  `station, latitude, longitude, altitude` (altitude may be `NA`)

- `gridmex`: Grid of candidate nodes with columns
  `station, latitude, longitude, altitude`

The great-circle distance is computed with the haversine formula via
geosphere. A coarse degree bounding box (`radius_km + bbox_pad_km`)/111
is applied first for speed.

If `station` (reference) is supplied, its coordinates are taken from
`stations`. If not found, an error is raised unless `lat/lon` are
provided.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) 20 km around station 15101, combining SMN + Grid
smn_tl_find_nearby(station = "15101", mode = "radius", radius_km = 20)

# 2) 15 vecinos más cercanos solo del grid, a coordenadas dadas
smn_tl_find_nearby(lat = 19.29, lon = -99.66, sources = "nasa",
                   mode = "k", k = 15)
} # }
```
