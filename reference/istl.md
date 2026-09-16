# Iterative multiple seasonal decomposition using ISTL

Performs an iterative stlplus-type decomposition for time series with
multiple seasonal periods. This function provides an R interface to the
JD+ Java implementation. It decomposes a time series into a trend
component, several seasonal components, and an irregular component using
an iterative STL-based procedure.

## Usage

``` r
istl(
  series,
  period,
  multiplicative = TRUE,
  swindow = NULL,
  twindow = NULL,
  robust = NULL,
  ninnerloop = 1,
  nouterloop = 15,
  nojump = FALSE,
  weight_threshold = 0.001,
  weight_function = c("biweight", "uniform", "triangular", "epanechnikov", "tricube",
    "triweight")
)
```

## Arguments

- series:

  Numeric vector. Input time series to be decomposed.

- period:

  Numeric vector. Seasonal periods to be modelled. For example,
  `c(7, 365)` for daily data with weekly and yearly seasonal patterns.
  Values are passed to the underlying Java implementation as integers.

- multiplicative:

  Logical. If `TRUE`, a multiplicative decomposition is used. If
  `FALSE`, an additive decomposition is used.

- swindow:

  Optional integer vector. Lengths of the seasonal smoothing windows,
  one for each seasonal period. If `NULL`, the seasonal windows are
  selected automatically by the underlying Java implementation.

- twindow:

  Optional integer vector. Lengths of the trend smoothing windows. If
  `NULL`, the trend windows are selected automatically by the underlying
  Java implementation.

- robust:

  Boolean. Analogue to robust parameter in stats::stl (see details)

- ninnerloop:

  Integer. Number of inner iterations of the ISTL algorithm.

- nouterloop:

  Integer. Number of outer iterations used to compute robust weights.
  Set to `0` to disable robust fitting.

- nojump:

  Logical. If `TRUE`, disables jump-based acceleration in the smoothing
  computations. If `FALSE`, the underlying implementation may use jumps
  to speed up the decomposition.

- weight_threshold:

  Numeric scalar in `[0, 0.3]`. Threshold used in the computation of
  robust weights.

- weight_function:

  Character string specifying the weighting function used by the LOESS
  smoothers. One of `"biweight"`, `"uniform"`, `"triangular"`,
  `"epanechnikov"`, `"tricube"` or `"triweight"`.

## Value

An object of class `"hf_decomposition"`, consisting of a list with two
elements:

- `decomposition`:

  A `data.frame` containing the original series, the seasonally adjusted
  series, the trend, one seasonal component for each period, the
  irregular component, fitted values and robust weights.

- `parameters`:

  A list containing the main parameters used for the decomposition.

## Details

The returned decomposition contains the following columns:

- `series`:

  The original input series.

- `sa`:

  The seasonally adjusted series (trend+irregular for additive,
  trend\*irregular for multiplicative decomposition).

- `t`:

  The trend component.

- `s<period>`:

  One seasonal component for each value supplied in `period`. For
  example, if `period = c(7, 365)`, the output contains columns `s7` and
  `s365`.

- `i`:

  The irregular component.

- `fit`:

  The fitted values from the decomposition (trend+seasonal for additive,
  trend\*seasonal for multiplicative decomposition).

- `weights`:

  The final robust weights.

If `multiplicative = TRUE`, the decomposition is interpreted as a
multiplicative decomposition. If `multiplicative = FALSE`, it is
interpreted as an additive decomposition.

If `robust = TRUE`, the parameters nouterloop and ninnerloop are
overwritten, so that 15 iterations of the outer loop and one run of the
inner loop are completed. If `robust = FALSE`, nouterloop is set to 0,
and ninnerloop is set to 2.

## See also

[`stlplus`](https://rjdverse.github.io/rjd3highfreq/reference/stlplus.md),
[`mstl`](https://rjdverse.github.io/rjd3highfreq/reference/mstl.md),
[`plot.hf_decomposition`](https://rjdverse.github.io/rjd3highfreq/reference/plot.hf_decomposition.md)

## Examples

``` r
q <- istl(
  series = rjd3toolkit::ABS$X0.2.09.10.M,
  period = c(12, 19)
)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/x12plus/base/r/X11Decomposition has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

plot(q)
#> Error in x(x): one of "yes", "no", "ask" or "default" expected.
```
