# Perform an X-11 like decomposition with any (non-integer) periodicity.

Perform an X-11 like decomposition with any (non-integer) periodicity.

## Usage

``` r
.x11plus_default(
  series,
  period,
  multiplicative = TRUE,
  trend_horizon = 6,
  trend_degree = 3,
  trend_kernel = c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform",
    "Triangular", "Epanechnikov", "Trapezoidal"),
  trend_asymmetric = c("CutAndNormalize", "Direct", "MMSRE"),
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

  Period of the seasonal component, any positive real number.

- multiplicative:

  Boolean indicating if the decomposition mode is multiplicative (TRUE).

- trend_horizon:

  bandwidth of trend filters.

- trend_degree:

  polynomial order in local trend model.

- trend_kernel:

  kernel weights in objective function.

- trend_asymmetric:

  truncation type for symmetric filter. `"matrix"`, `"lp_filter"` or
  `"rkhs_filter"`.

- seas_s0:

  Seasonal filter for B5, C5, D5.

- seas_s1:

  seasonal filter for B10, C10, D10.

- extreme_lsig:

  lower boundary used for outlier correction in irregular.

- extreme_usig:

  upper boundary used for outlier correction in irregular.

- user_defined:

  a vector containing the additional output variables.

## Value

An object of the class 'hf_decomposition', containing the decomposition
and the parameters

## Details

If trend_coefs are provided, the other trend-settings are ignored and
.x11plus_trend() is run.

## Examples

``` r
q <- x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/information/InformationExtractors has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
