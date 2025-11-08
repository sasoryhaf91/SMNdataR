# Download and Structure Daily Station Data (SMN)

Downloads raw daily data for a given SMN station, filters it by a date
window, attaches station coordinates, and returns a cleaned table in
either *full* or *reduced* format. This is the basic building block used
by higher-level functions (e.g.,
[`smn_dl_daily_batch`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_batch.md)
or the hybrid workflow).

## Usage

``` r
smn_dl_daily_single(
  station,
  start_date = "1961-01-01",
  end_date = Sys.Date(),
  output_format = c("full", "reduced"),
  del_na = c("no", "yes")
)
```

## Arguments

- station:

  Character or numeric SMN station code (e.g., `"15101"`).

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

## Value

A **data.frame** with the columns defined by `output_format`, ordered by
date. If no rows fall within the requested window or the download fails,
returns an empty data frame with the appropriate columns for the chosen
format.

## Details

The function:

1.  Downloads raw daily data via
    [`smn_dl_daily_raw`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_raw.md)
    (best effort).

2.  Filters by `start_date <= date <= end_date`.

3.  Ensures variables `prec, evap, tmax, tmin` exist (filling missing
    ones with `NA`).

4.  Attaches coordinates using
    [`smn_int_extract_coordinates`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md)
    (falls back to `NA` on failure).

The goal is to deliver a stable, tidy-ready table for downstream
analyses and for integration with auxiliary sources (e.g., NASA POWER)
in hybrid workflows.

## See also

[`smn_dl_daily_raw`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_raw.md),
[`smn_int_extract_coordinates`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md),
[`smn_dl_daily_batch`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal example (short window) with 'full' output
df_full <- smn_dl_daily_single(
  station = "15101",
  start_date = "2020-01-01",
  end_date   = "2020-01-07",
  output_format = "full",
  del_na = "no"
)
head(df_full)

# Same interval but 'reduced' output
df_red <- smn_dl_daily_single(
  station = "15101",
  start_date = "2020-01-01",
  end_date   = "2020-01-07",
  output_format = "reduced"
)
head(df_red)
} # }
```
