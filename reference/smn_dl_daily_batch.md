# Batch Download of Daily Climate Data (SMN, NASA POWER, or Hybrid)

Downloads daily meteorological series for **multiple stations** from:

- **SMN** (official in-situ records),

- **NASA POWER** (satellite/reanalysis),

- **Hybrid**: joins SMN (full) + NASA POWER variables by
  `station + date`.

It harmonizes output columns (station metadata, date, core SMN
variables, and optional NASA POWER variables) and returns either a
single tidy data frame (row-bound) or a list per station.

## Usage

``` r
smn_dl_daily_batch(
  stations,
  source = c("smn", "nasa", "hybrid"),
  start_date = "1961-01-01",
  end_date = Sys.Date(),
  output_format = c("full", "reduced"),
  del_na = c("no", "yes"),
  require_coords = TRUE,
  vars = c("T2M_MIN", "T2M_MAX", "PRECTOTCORR"),
  community = c("AG", "RE", "SB"),
  return_list = FALSE,
  .handlers = NULL,
  .progress = TRUE
)
```

## Arguments

- stations:

  Character/numeric vector of station codes *or* a data frame with at
  least a `station` column. For `source = "nasa"` or `"hybrid"`, if
  `latitude`, `longitude`, `altitude` are present, they will be used
  directly; otherwise the function will try to resolve them (see
  `require_coords`).

- source:

  One of `"smn"`, `"nasa"`, `"hybrid"`, or a custom key present in
  `.handlers`.

- start_date:

  Start date (inclusive). `Date` or `character`. Default `"1961-01-01"`.

- end_date:

  End date (inclusive). `Date` or `character`. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- output_format:

  Output format: `"full"` (default) or `"reduced"`.

  - **full**:
    `station, latitude, longitude, altitude, date, prec, evap, tmax, tmin`

  - **reduced**: `date, prec, evap, tmax, tmin`

- del_na:

  `"no"` (default) or `"yes"`. If `"yes"`, rows containing `NA` are
  dropped at the end.

- require_coords:

  Logical. For `"nasa"`/`"hybrid"`: attempt to resolve missing
  coordinates using
  [`smn_int_extract_coordinates()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md)
  when `latitude/longitude` are not provided in `stations`.

- vars:

  Character vector with NASA POWER parameter codes used in
  `source = "nasa"` or `"hybrid"` (e.g.,
  `c("T2M_MIN","T2M_MAX","PRECTOTCORR")`).

- community:

  NASA POWER community for `"nasa"`/`"hybrid"`; one of `"AG"`, `"RE"`,
  `"SB"` (default `"AG"`).

- return_list:

  Logical; if `TRUE`, returns a named list (one element per station). If
  `FALSE` (default), returns a single tidy data frame with all rows
  bound.

- .handlers:

  Optional named list of custom handlers with signature
  `function(station, start_date, end_date, output_format, del_na, row, vars, community)`.

- .progress:

  Logical. Show a simple text progress bar while downloading.

## Value

A **data frame** (default) with all stations row-bound in chronological
order per station, or a **named list** of per-station data frames when
`return_list = TRUE`.

## Details

**Output columns** depend on `source`:

- `source = "smn"`:
  `station, latitude, longitude, altitude, date, prec, evap, tmax, tmin`
  (or reduced set if `output_format = "reduced"`).

- `source = "nasa"`: `station, latitude, longitude, altitude, date` +
  requested `vars`.

- `source = "hybrid"`: union of SMN core variables + requested NASA
  `vars`.

If some branch fails for a given station, the function continues and
returns what is available (empty data frame for that station), warning
as needed. When `return_list = FALSE`, results are row-bound in the end,
keeping a consistent column order per `source`.

## Custom handlers

You can extend sources via `.handlers`, e.g.:

      my_h <- list(
        era5 = function(station, start_date, end_date, output_format, del_na, row, vars, community) {
          # ... your downloader; must return a data.frame with at least
          # station, date, latitude, longitude, altitude, and variables
        }
      )
      smn_dl_daily_batch(stations = c("15021","15101"),
                         source = "era5",
                         start_date = "2000-01-01", end_date = "2000-01-10",
                         .handlers = my_h)

## See also

[`smn_dl_daily_single`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md),
[`smn_dl_daily_nasa`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_nasa.md),
[`smn_int_extract_coordinates`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Minimal examples (short date window) ---

# 1) SMN only (two stations, short period)
df_smn <- smn_dl_daily_batch(
  stations = c("15021","15101"),
  source   = "smn",
  start_date = "2020-01-01",
  end_date   = "2020-01-05",
  output_format = "full",
  del_na = "no",
  .progress = TRUE
)
head(df_smn)

# 2) NASA POWER only — if you already know coordinates for each station
st <- data.frame(
  station   = c("X15021","X15101"),
  latitude  = c(19.35, 19.70),
  longitude = c(-99.10, -99.20),
  altitude  = c(2250, 2400)
)
df_nasa <- smn_dl_daily_batch(
  stations = st,
  source   = "nasa",
  start_date = "2020-01-01",
  end_date   = "2020-01-03",
  vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
  community = "AG",
  .progress = FALSE
)
head(df_nasa)

# 3) HYBRID (SMN + NASA POWER merged by station/date)
df_hyb <- smn_dl_daily_batch(
  stations = c("15021","15101"),
  source   = "hybrid",
  start_date = "2020-01-01",
  end_date   = "2020-01-05",
  vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR"),
  .progress = TRUE
)
head(df_hyb)
} # }
```
