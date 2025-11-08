# Extract Station Coordinates Robustly (internal)

Retrieves the metadata lines for a given station using the URL generated
by `smn_int_get_url()`, searches for lines containing
latitude/longitude/altitude labels (case-insensitive, ES/EN variants),
and extracts the first numeric value from each line. If a coordinate
cannot be extracted or fails range validation, `NA` is returned for that
coordinate.

## Usage

``` r
smn_int_extract_coordinates(
  station,
  max_attempts = 10,
  encoding = "UTF-8",
  add_meta_cols = FALSE
)
```

## Arguments

- station:

  Station code (character or numeric).

- max_attempts:

  Integer. Maximum retries for the metadata download (default `10`).

- encoding:

  Character. Encoding for
  [`readLines()`](https://rdrr.io/r/base/readLines.html) (default
  `"UTF-8"`).

- add_meta_cols:

  Logical. If `TRUE`, include `station` and `source_url` as explicit
  columns in the returned data (default `FALSE`). These are always
  attached as attributes for provenance.

## Value

A one-row `data.frame` with columns `latitude`, `longitude`, `altitude`.
Attributes `station` and `source_url` are attached.

## Examples

``` r
NULL
#> NULL
```
