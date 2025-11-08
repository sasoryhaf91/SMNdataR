# Getting started with SMNdataR

## Load package

``` r
library(SMNdataR)
```

## What you’ll learn

- The shape of daily tables returned by smn_dl_daily_single().

- A minimal, offline example (no internet needed to build this
  vignette).

- Where to look for real downloads (code provided with eval = FALSE).

## A tiny offline example

We mimic the “full” daily table with synthetic data:

``` r
toy <- data.frame(
station   = "15101",
latitude  = 19.50,
longitude = -99.25,
altitude  = 2250,
date      = as.Date("2020-01-01") + 0:6,
prec      = c(0, 2.1, 0, 5.3, NA, 0.5, 1.2),
evap      = c(NA, 3.0, 3.2, 3.1, 2.9, 3.0, 2.8),
tmax      = c(22, 23, 21, 20, 22, 21, 24),
tmin      = c( 8,  9,  7,  6,  8,  7,  9),
stringsAsFactors = FALSE
)
str(toy)
#> 'data.frame':    7 obs. of  9 variables:
#>  $ station  : chr  "15101" "15101" "15101" "15101" ...
#>  $ latitude : num  19.5 19.5 19.5 19.5 19.5 19.5 19.5
#>  $ longitude: num  -99.2 -99.2 -99.2 -99.2 -99.2 ...
#>  $ altitude : num  2250 2250 2250 2250 2250 2250 2250
#>  $ date     : Date, format: "2020-01-01" "2020-01-02" ...
#>  $ prec     : num  0 2.1 0 5.3 NA 0.5 1.2
#>  $ evap     : num  NA 3 3.2 3.1 2.9 3 2.8
#>  $ tmax     : num  22 23 21 20 22 21 24
#>  $ tmin     : num  8 9 7 6 8 7 9
head(toy)
#>   station latitude longitude altitude       date prec evap tmax tmin
#> 1   15101     19.5    -99.25     2250 2020-01-01  0.0   NA   22    8
#> 2   15101     19.5    -99.25     2250 2020-01-02  2.1  3.0   23    9
#> 3   15101     19.5    -99.25     2250 2020-01-03  0.0  3.2   21    7
#> 4   15101     19.5    -99.25     2250 2020-01-04  5.3  3.1   20    6
#> 5   15101     19.5    -99.25     2250 2020-01-05   NA  2.9   22    8
#> 6   15101     19.5    -99.25     2250 2020-01-06  0.5  3.0   21    7
```

## A simple monthly aggregation (demo only):

``` r
toy$ym <- format(toy$date, "%Y-%m")
aggregate(prec ~ ym, data = toy, sum, na.rm = TRUE)
#>        ym prec
#> 1 2020-01  9.1
```

## Real downloads (examples, not executed here)

### SMN daily

``` r
# df_smn <- smn_dl_daily_single(
# station = "15101",
# start_date = "2020-01-01", end_date = "2020-01-10",
# output_format = "full"
# )
# head(df_smn)
```

### NASA POWER daily

``` r
# df_np <- smn_dl_daily_nasa(
# station = "Toluca_Node",
# lat = 19.289, lon = -99.657,
# start_date = "2020-01-01", end_date = "2020-01-10",
# vars = c("T2M_MAX","T2M_MIN","PRECTOTCORR"),
# output_format = "reduce"
# )
# head(df_np)
```

### Hybrid (SMN + NASA over the same coordinates)

``` r
# stations <- data.frame(
# station = c("15101","15021"),
# latitude = c(NA, 19.5),
# longitude = c(NA, -99.3),
# altitude = NA
# )
# df_hyb <- smn_dl_daily_batch(
# stations,
# source = "hybrid",
# start_date = "2020-01-01", end_date = "2020-01-05",
# vars = c("T2M_MIN","T2M_MAX","PRECTOTCORR")
# )
# head(df_hyb)
```

## Session info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] SMNdataR_0.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.54         cachem_1.1.0      knitr_1.50        htmltools_0.5.8.1
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
#> [17] compiler_4.5.2    tools_4.5.2       ragg_1.5.0        evaluate_1.0.5   
#> [21] bslib_0.9.0       yaml_2.3.10       jsonlite_2.0.0    rlang_1.1.6      
#> [25] fs_1.6.6
```
