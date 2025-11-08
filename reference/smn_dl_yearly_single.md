# Download and Aggregate **Yearly** Climate Data (Reduce Format)

Downloads **daily** data for an SMN station using
[`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md)
and aggregates to **yearly** values for one or more variables, with
quality control based on an allowed fraction of missing days per year.
The output is always in **reduce (wide)** format: one row per year with
the requested variables as columns plus station metadata.

## Usage

``` r
smn_dl_yearly_single(
  station,
  start_date = "1961-01-01",
  end_date = Sys.Date(),
  variables = "all",
  max_missing_frac = 0.2,
  aggregator = NULL,
  csv_file = NULL
)
```

## Arguments

- station:

  Character or numeric. SMN station code (e.g., `"15101"`).

- start_date:

  Character or `Date`. Start of the daily window (default
  `"1961-01-01"`, inclusive).

- end_date:

  Character or `Date`. End of the daily window (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html), inclusive).

- variables:

  Character vector of variables to summarize. Any subset of
  `c("prec","evap","tmax","tmin")`. Use `"all"` to include all four
  (default `"all"`).

- max_missing_frac:

  Numeric in \[0, 1\]. Maximum allowed **fraction of missing days per
  year** to keep that year's aggregate for a variable (default `0.2`).
  When the missing fraction exceeds the threshold, that yearly value is
  set to `NA`.

- aggregator:

  Either:

  - `NULL` (default): uses `sum` for `"prec","evap"` and `mean` for
    `"tmax","tmin"`, or

  - a **single function** (e.g., `median`) applied to all variables, or

  - a **named list** mapping variables to functions, e.g.
    `list(prec = sum, evap = sum, tmax = mean, tmin = mean)`.

- csv_file:

  Optional file path. If non-`NULL`, the result is written to CSV
  (`row.names = FALSE`) and also returned.

## Value

A base `data.frame` in **reduce** (wide) format with columns:
`station, latitude, longitude, altitude, YEAR, <variables...>`. If no
data are available, returns an empty table with the same schema.

## Details

Internally calls
[`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md)
with `output_format = "full"` to fetch daily data and station metadata.
The missing-data rule is computed **per year and per variable** as
`#(NA) / #(days in year)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# All variables in reduce format (default)
y1 <- smn_dl_yearly_single(
  station = "15101",
  start_date = "2000-01-01",
  end_date   = "2005-12-31",
  variables  = "all"
)

# Subset of variables and custom aggregator
y2 <- smn_dl_yearly_single(
  station = "15101",
  start_date = "1990-01-01",
  end_date   = "1999-12-31",
  variables  = c("prec","tmax"),
  aggregator = list(prec = sum, tmax = median)
)
} # }
```
