# Download Raw Daily Data for a Station (robust header detection)

Downloads and parses the raw daily data file from SMN using the internal
URL. The function is resilient to changes in the header length by
detecting the first data row via a date pattern (`YYYY-MM-DD`) rather
than assuming a fixed number of header lines.

## Usage

``` r
smn_dl_daily_raw(
  station,
  add_meta_cols = FALSE,
  max_attempts = 10,
  encoding = "UTF-8"
)
```

## Arguments

- station:

  Station code (character or numeric).

- add_meta_cols:

  Logical. If `TRUE`, includes `station` and `source_url` as explicit
  columns in the returned data (default `FALSE` to keep the original
  column schema). Metadata is always attached as attributes.

- max_attempts:

  Integer. Maximum retry attempts via `smn_int_handle_error` (default
  `10`).

- encoding:

  Character. Encoding passed to
  [`readLines()`](https://rdrr.io/r/base/readLines.html) (default
  `"UTF-8"`).

## Value

A base `data.frame` with columns: `date`, `prec`, `evap`, `tmax`,
`tmin`. Attributes `station` and `source_url` are attached for
provenance.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- smn_dl_daily_raw("15021")
attr(df, "source_url")
} # }
```
