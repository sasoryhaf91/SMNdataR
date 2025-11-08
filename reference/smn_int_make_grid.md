# Build a local square grid around a point (km-based) and clip by radius (robust CRS handling)

Build a local square grid around a point (km-based) and clip by radius
(robust CRS handling)

## Usage

``` r
smn_int_make_grid(
  lat,
  lon,
  cell_km = 5,
  radius_km = 20,
  clip = c("intersect", "centroid"),
  crs_out = 4326,
  return = c("polygons", "centroids", "both")
)
```

## Arguments

- lat:

  Numeric latitude of center (WGS84).

- lon:

  Numeric longitude of center (WGS84).

- cell_km:

  Numeric cell size in kilometers (default 1).

- radius_km:

  Numeric radius (km) for square/round extent.

- clip:

  Logical; clip by country polygon (if available).

- crs_out:

  Output CRS EPSG code (default 4326).

- return:

  Character: "sf" or "data.frame".
