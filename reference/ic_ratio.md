# Compute IC-Ratio

Compute IC-Ratio

## Usage

``` r
ic_ratio(
  series,
  frequency = NULL,
  trend_length = NULL,
  musgrave = FALSE,
  multiplicative = FALSE,
  seasonally_adjust = TRUE,
  sma = "S3X5",
  ...
)
```

## Arguments

- series:

  input time series

- frequency:

  frequency of the seasonal component. Can be NULL if series is ts

- trend_length:

  length of Henderson filter used for trend adjustment. If NULL it is
  nextodd(frequency)

- musgrave:

  Boolean indicating if Musgrave asymmetric filters should be used in
  trend adjustment

- multiplicative:

  boolean indicating if the decomposition is multiplicative or additive

- seasonally_adjust:

  boolean. Should series be seasonally adjusted?

- sma:

  seasonal moving average used in x11plus()

- ...:

  additional arguments for trend adjustment. See ?henderson

## Details

In the traditional calculation of the IC-ratio, no asymmetric filters
used for the preliminary trend estimation (see Ladiray and Quenneville
2001, Table B7).

## References

Ladiray, D., Quenneville, B. (2001). The Various Tables. In: Seasonal
Adjustment with the X-11 Method. Lecture Notes in Statistics, vol 158.
Springer, New York, NY. https://doi.org/10.1007/978-1-4613-0175-2_5

## Examples

``` r
series <- rjd3toolkit::Retail$AllOtherGenMerchandiseStores
ic_ratio(series)
#> Error in rJava::.jcall("jdplus/x12plus/base/r/X11Decomposition", "Ljdplus/x12plus/base/r/X11Decomposition$Results;",     "process", as.numeric(series), as.numeric(period), multiplicative,     as.integer(trend_horizon), as.integer(trend_degree), tkernel,     asym, seas0, seas1, extreme_lsig, extreme_usig): RcallMethod: cannot determine object class
```
