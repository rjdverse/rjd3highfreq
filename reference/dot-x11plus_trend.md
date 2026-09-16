# X-11 Decomposition With Custom Trend Filters

Perform the X-11 decomposition using custom trend filter

## Usage

``` r
.x11plus_trend(
  series,
  period = stats::frequency(series),
  trend_coefs,
  multiplicative = TRUE,
  seas_s0 = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
  seas_s1 = c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
  extreme_lsig = 1.5,
  extreme_usig = 2.5,
  user_defined = NULL
)
```

## Arguments

- series:

  input time-series.

- period:

  period.

- trend_coefs:

  coefficients of the filters used for the trend-cycle extraction from
  the real-time asymmetric filter to the symmetric filter. Can be a,
  object of class `"list"`, `"matrix"`, `"lp_filter"` or
  `"rkhs_filter"`.

- multiplicative:

  boolean indicating if the decomposition mode is multiplicative.

- seas_s0, seas_s1:

  seasonal filters.

- extreme_lsig, extreme_usig:

  boundaries used for outlier correction in irregular.

- user_defined:

  a vector containing the additional output variables.
