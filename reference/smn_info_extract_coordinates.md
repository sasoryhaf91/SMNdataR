# Extract Coordinates for Multiple Stations

Iterates over a vector of station codes and retrieves coordinates using
[`smn_int_extract_coordinates()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md).
If extraction fails for any station, the row is filled with `NA` values
and a warning is emitted.

## Usage

``` r
smn_info_extract_coordinates(stations, show_progress = interactive(), ...)
```

## Arguments

- stations:

  Character or numeric vector of station codes.

- show_progress:

  Logical. Show a text progress bar when interactive (default
  [`interactive()`](https://rdrr.io/r/base/interactive.html)).

- ...:

  Optional arguments passed to
  [`smn_int_extract_coordinates()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md),
  e.g. `max_attempts`, `encoding`, `add_meta_cols = TRUE`.

## Value

A base `data.frame` with columns: `station`, `latitude`, `longitude`,
`altitude`. If you pass `add_meta_cols = TRUE` in `...`, extra columns
from
[`smn_int_extract_coordinates()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md)
(e.g. `source_url`) are included too.

## See also

[`smn_int_extract_coordinates()`](https://sasoryhaf91.github.io/SMNdataR/reference/smn_int_extract_coordinates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- c(15020, 15171, 12345)  # 12345 likely fails
coords_df <- smn_info_extract_coordinates(stations)
head(coords_df)
} # }
```
