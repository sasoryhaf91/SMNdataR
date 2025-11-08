# Download Daily NASA POWER (MERRA-2) Data for a Named Point

Downloads daily NASA POWER variables for a specific point (lat/lon) and
tags the result with a user-defined `station` (node) name so you can
join/trace later.

## Usage

``` r
smn_dl_daily_nasa(
  station,
  lat,
  lon,
  altitude = NA_real_,
  start_date = "1984-01-01",
  end_date = Sys.Date(),
  vars = c("T2M_MAX", "T2M_MIN", "RH2M", "PRECTOTCORR", "EVLAND"),
  community = "AG",
  retries = 5,
  sleep_sec = 3,
  output_format = c("all", "reduce")
)
```

## Arguments

- station:

  Character. A label or node name to tag the time series (e.g.,
  "Toluca_Node").

- lat:

  Numeric. Latitude in decimal degrees (WGS84).

- lon:

  Numeric. Longitude in decimal degrees (WGS84).

- altitude:

  Numeric (optional). Altitude of the point (m a.s.l.). Default
  `NA_real_`.

- start_date:

  Start date (YYYY-MM-DD). If earlier than 1984-01-01, it is
  automatically adjusted to `1984-01-01` (POWER coverage).

- end_date:

  End date (YYYY-MM-DD). Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- vars:

  Character vector of POWER parameter short names. Defaults to a compact
  set: `c("T2M_MAX","T2M_MIN","RH2M","PRECTOTCORR","EVLAND")`.

- community:

  POWER community. Default `"AG"`. (Common alternatives: `"SSE"`,
  `"RE"`)

- retries:

  Integer. Max GET retries on failure (default 5).

- sleep_sec:

  Numeric. Seconds to sleep between retries (default 3).

- output_format:

  Either `"all"` (default) or `"reduce"`. The `"reduce"` option keeps
  only `station, latitude, longitude, altitude, date` and the requested
  variables.

## Value

A `data.frame` ordered by `date` with columns:
`station, latitude, longitude, altitude, date, <requested POWER variables>`.
POWER missing values `-999` are returned as `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
nasa_df <- smn_dl_daily_nasa(
  station = "Toluca_Node",
  lat = 19.289, lon = -99.657, altitude = 2670,
  start_date = "2020-01-01", end_date = "2020-12-31",
  vars = c("T2M_MAX","T2M_MIN","PRECTOTCORR"),
  output_format = "reduce"
)
} # }
```
