# Outlier Detection for Extended Airline Models

This internal function performs automatic outlier detection in
high-frequency time series using Extended Airline models.

## Usage

``` r
find_outliers(
  input,
  types = c("AO"),
  start = 0,
  end = 0,
  critical_value = 0,
  max_outliers = 30,
  max_round = 30,
  precision = 1e-05,
  deps = 1e-04
)
```

## Arguments

- input:

  time series

- types:

  Character vector specifying the types of outliers to detect. Default:
  c("AO") (additive outliers only). Common options include:

  - "AO" - Additive Outlier

  - "LS" - Level Shift

  - "WO" - Switch Outlier

- start:

  Integer specifying the starting position (1-based R indexing) for
  outlier detection. Default: 0 (detection from the beginning of the
  series).

- end:

  Integer specifying the ending position (1-based R indexing) for
  outlier detection. Default: 0 (detection until the end of the series).

- critical_value:

  Numeric value for the critical value threshold. Uses the maximum value
  among the specified value and a global max-t threshold based on
  extreme-value theory, roughly of order sqrt(2 \* log(n))). Default: 0
  (global max-t threshold is used).

- max_outliers:

  Integer specifying the maximum number of outliers to detect. Default:
  30.

- max_round:

  Integer specifying the maximum number of detection iterations.
  Default: 30.

- precision:

  Numeric value specifying the tolerance for convergence of optimization
  algorithms. Default: 1e-5.

- deps:

  Numeric scalar. Step in the computation of the numerical derivatives,
  used in the optimisation routine. Default:1e-4.

## Value

A numeric matrix with dimensions \[number of outliers detected × 2\].

## References

Outlier critical value using the Ljung algorithm as given in Ljung, G.
M. (1993). On outlier detection in time series. Journal of Royal
Statistical Society B 55, 559-567. Solution proposed by Brian Monsell
(LBS), January 2022

## Examples

``` r

series <- rjd3toolkit::Exports$Malta
find_outliers(series, critical_value = 3.5, types = c("LS", "WO"))
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/sa/base/api/DecompositionMode has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
