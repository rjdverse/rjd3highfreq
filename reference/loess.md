# Fit a LOESS smoother

Applies a LOESS smoother to a numeric time series.

## Usage

``` r
loess(series, window, degree = 1, jump = 0)
```

## Arguments

- series:

  Numeric vector. Input time series to be smoothed.

- window:

  Integer. Length of the LOESS smoothing window. Larger values produce a
  smoother result.

- degree:

  Integer. Degree of the local polynomial. Supported values are `0` for
  locally constant smoothing and `1` for locally linear smoothing. The
  default is `1`.

- jump:

  Integer. Number of jumps used to speed up the computation. If
  `jump = 0`, the smoother is evaluated at every observation. Values
  greater than zero evaluate the smoother at fewer points and
  interpolate between them. The default is `0`.

## Value

A numeric vector containing the smoothed series.

## Details

This function provides a simple R interface to the JD+ Java
implementation of the LOESS smoother used internally by the stlplus
decomposition routines. It smoothes the input series using a local
polynomial of degree 0 or 1.

This function is not the same as
[`loess`](https://rdrr.io/r/stats/loess.html) from the stats package. It
does not use a formula interface and is intended for smoothing a single
numeric series with the JD+ stlplus\|loess implementation. Also, it can
handle time series with missing values.

## See also

[`stlplus`](https://rjdverse.github.io/rjd3highfreq/reference/stlplus.md),
[`mstl`](https://rjdverse.github.io/rjd3highfreq/reference/mstl.md),
[`istl`](https://rjdverse.github.io/rjd3highfreq/reference/istl.md),
[`loess`](https://rdrr.io/r/stats/loess.html)

## Examples

``` r
q <- stlplus(
  series = rjd3toolkit::ABS$X0.2.09.10.M,
  period = 12
)
#> Error in rJava::.jcall("jdplus/stl/base/r/StlDecomposition", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",     "stl", as.numeric(series), as.integer(period), as.logical(multiplicative),     as.integer(swindow), as.integer(twindow), as.integer(lwindow),     as.integer(sdegree), as.integer(tdegree), as.integer(ldegree),     as.integer(sjump), as.integer(tjump), as.integer(ljump),     as.integer(ninnerloop), as.integer(nouterloop), as.numeric(weight_threshold),     toupper(as.character(weight_function)), as.logical(legacy)): RcallMethod: cannot determine object class

trend <- q$decomposition[, "t"]
#> Error in q$decomposition: object of type 'closure' is not subsettable

smoothed_trend <- loess(
  series = trend,
  window = 121
)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/stl/base/r/StlDecomposition has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

ts.plot(
  cbind(trend, smoothed_trend),
  col = c("black", "red")
)
#> Error: object 'trend' not found
```
