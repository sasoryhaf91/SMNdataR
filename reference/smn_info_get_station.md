# Get Station Metadata

Retrieves station metadata for one or multiple station IDs, or from a
data frame containing a station id column. The function internally
queries `smn_int_get_station()` for each id and binds results row-wise.

## Usage

``` r
smn_info_get_station(input)
```

## Arguments

- input:

  Either

  - a single station id (character or numeric),

  - a vector of station ids, or

  - a `data.frame` containing a station id column named **`CLAVE`** or
    **`station`**.

## Value

A base `data.frame` with the metadata rows for the requested stations.
If no id could be resolved or none matched, an **empty** `data.frame` (0
rows) with the same columns as
[`SMNdataR::stations`](https://sasoryhaf91.github.io/SMNdataR/reference/stations.md)
is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
smn_info_get_station("15101")
smn_info_get_station(c("15101","15021"))
smn_info_get_station(data.frame(CLAVE = c("15101","15021")))
} # }
```
