# Download and Aggregate Monthly Climate Data (Single Station)

Downloads daily SMN data for a station using
[`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md),
then aggregates it to monthly values for one or more variables applying
a completeness rule per month. The function can return either a *long*
table (one row per month and a column per requested variable), or
*reduce* tables (one per variable, with `YEAR`, `JAN..DEC`, and annual
summaries).

## Usage

``` r
smn_dl_monthly_single(
  station,
  variables = "all",
  start_date = "1961-01-01",
  end_date = Sys.Date(),
  max_missing_frac = 0.2,
  output_format = c("reduce", "long"),
  aggregator = NULL,
  csv_file = NULL,
  daily_del_na = "no"
)
```

## Arguments

- station:

  Character or numeric. SMN station code (passed to
  [`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md)).

- variables:

  Character vector of variables to aggregate, or `"all"`. Valid names
  are `"prec"`, `"evap"`, `"tmax"`, `"tmin"`. Default `"all"`.

- start_date:

  Start date (inclusive). Coerced with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). Default
  `"1961-01-01"`. Passed to
  [`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md).

- end_date:

  End date (inclusive). Coerced with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html). Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). Passed to
  [`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md).

- max_missing_frac:

  Numeric in `[0, 1]`. If the fraction of missing days in a month
  exceeds this threshold, the monthly value is set to `NA`. Default
  `0.2`.

- output_format:

  Output format. Either `"long"` or `"reduce"` (default `"reduce"`).

- aggregator:

  Optional **single function** to apply to all variables (e.g.,
  `median`) **or** a **named list** mapping variable → function, e.g.,
  `list(prec = sum, tmax = mean)`. If `NULL`, defaults are used: `sum`
  for `prec,evap` and `mean` for `tmax,tmin`.

- csv_file:

  Optional path to write CSV output.

  - For `"long"`: a single CSV at `csv_file`.

  - For `"reduce"` with multiple variables: one file per variable, using
    `paste0(file_path_sans_ext(csv_file), "_", var, ".csv")`.

- daily_del_na:

  Character `"yes"` or `"no"`; forwarded to
  [`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md)
  to control row removal at the **daily** stage. Default `"no"`. Note:
  the monthly completeness rule is applied **after** downloading daily
  data; this argument is only about cleaning rows at the daily function.

## Value

- If `output_format = "long"`: a single `data.frame` with columns
  `station, latitude, longitude, altitude, date, <vars...>`.

- If `output_format = "reduce"` and a single variable was requested: a
  wide `data.frame`.

- If `output_format = "reduce"` and multiple variables were requested: a
  **named list** of wide `data.frame`s (one per variable).

## Details

### Long format

If `output_format = "long"`, the result contains one row per month with
columns: `station, latitude, longitude, altitude, date, <vars...>`,
where `<vars...>` are the requested subset among
`prec, evap, tmin, tmax`. The `date` column is the first day of each
month.

### Reduce format

If `output_format = "reduce"`, the result is a wide table (or a named
list of tables when multiple variables are requested) with columns:
`YEAR, JAN..DEC, ACUM, PROM, MONTHS`. For sum-type variables (`prec`,
`evap`) `ACUM` is the row **sum**; for mean-type variables (`tmax`,
`tmin`) `ACUM` is the row **mean**. `PROM` is always the row mean across
valid months; `MONTHS` is the count of non-`NA` months.

Daily data are obtained via
[`smn_dl_daily_single()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_dl_daily_single.md)
with `output_format = "full"` (to carry station metadata). Missing
fraction is computed against the **calendar** number of days per month
within the requested window; absent daily rows count as missing. The
`date` in the long table is set to the first day of each month
(`YYYY-MM-01`).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) All variables, monthly, long format (one row per month)
m_long <- smn_dl_monthly_single(
  station = "15101",
  variables = "all",
  start_date = "1984-01-01",
  end_date   = "2020-12-31",
  output_format = "long"
)

# 2) Only precipitation, reduce format (wide)
m_prec <- smn_dl_monthly_single("15101", variables = "prec", output_format = "reduce")

# 3) Two variables, strict completeness (<= 10% missing), custom aggregator
m_custom <- smn_dl_monthly_single(
  station = "15101",
  variables = c("tmax","tmin"),
  max_missing_frac = 0.10,
  aggregator = median,
  output_format = "long"
)

# 4) Control daily cleaning from here (do not drop NAs before monthly)
m_cfg <- smn_dl_monthly_single(
  station = "15101",
  variables = "all",
  daily_del_na = "no",
  output_format = "long"
)
} # }
```
