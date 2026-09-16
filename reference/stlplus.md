# Seasonal decomposition using STL+

Performs an STL-like seasonal decomposition. It can handle missing
values and does allow a multiplicative decomposition.

## Usage

``` r
stlplus(
  series,
  period,
  multiplicative = TRUE,
  swindow = 7,
  twindow = 0,
  lwindow = 0,
  sdegree = 0,
  tdegree = 1,
  ldegree = 1,
  sjump = 0,
  tjump = 0,
  ljump = 0,
  robust = NULL,
  ninnerloop = 1,
  nouterloop = 15,
  weight_threshold = 0.001,
  weight_function = c("biweight", "uniform", "triangular", "epanechnikov", "tricube",
    "triweight"),
  legacy = FALSE
)
```

## Arguments

- series:

  Numeric vector. Input time series to be decomposed.

- period:

  Numeric scalar. Seasonal period of the series. For example, use `12`
  for monthly data with yearly seasonality, `4` for quarterly data, or
  `7` for daily data with weekly seasonality. In the current
  implementation, this value is passed to Java as an integer.

- multiplicative:

  Logical. If `TRUE`, a multiplicative decomposition is used. If
  `FALSE`, an additive decomposition is used.

- swindow:

  Integer. Length of the seasonal smoothing window.

- twindow:

  Integer. Length of the trend smoothing window. If set to `0`, the
  value is selected automatically by the underlying Java implementation.

- lwindow:

  Integer. Length of the low-pass filter used to remove the trend from
  the seasonal component. If set to `0`, the value is selected
  automatically by the underlying Java implementation.

- sdegree:

  Integer. Degree of the local polynomial used for seasonal smoothing.
  Usually `0` or `1`.

- tdegree:

  Integer. Degree of the local polynomial used for trend smoothing.
  Usually `0` or `1`.

- ldegree:

  Integer. Degree of the local polynomial used for low-pass smoothing.
  Usually `0` or `1`.

- sjump:

  Integer. Number of jumps used in the computation of the seasonal
  component. Values greater than zero speed up the computation by
  evaluating the smoother at fewer points and interpolating between
  them.

- tjump:

  Integer. Number of jumps used in the computation of the trend
  component.

- ljump:

  Integer. Number of jumps used in the computation of the low-pass
  component.

- robust:

  Boolean. Analogue to robust parameter in stats::stl (see details)

- ninnerloop:

  Integer. Number of inner iterations of the STL algorithm.

- nouterloop:

  Integer. Number of outer iterations used to compute robust weights.
  Set to `0` to disable robust fitting.

- weight_threshold:

  Numeric scalar in `[0, 0.3]`. Threshold used in the computation of
  robust weights.

- weight_function:

  Character string specifying the weighting function used by the LOESS
  smoothers. One of `"biweight"`, `"uniform"`, `"triangular"`,
  `"epanechnikov"`, `"tricube"` or `"triweight"`.

- legacy:

  Logical. If `TRUE`, uses the legacy MAD computation of the underlying
  implementation. This option is mainly provided for backward
  compatibility.

## Value

An object of class `"hf_decomposition"`, consisting of a list with two
elements:

- `decomposition`:

  A `data.frame` containing the original series, the seasonally adjusted
  series, the trend, seasonal and irregular components, fitted values
  and robust weights (see details).

- `parameters`:

  A list containing the main parameters used for the decomposition.

## Details

This function provides an R interface to the JD+ Java implementation of
STL decomposition. It decomposes a time series into trend, seasonal and
irregular components and returns the result as an object of class
`"hf_decomposition"`.

The returned decomposition contains the following columns:

- `series`:

  The original input series.

- `sa`:

  The seasonally adjusted series (trend+irregular for additive,
  trend\*irregular for multiplicative decomposition).

- `t`:

  The trend component.

- `s`:

  The seasonal component.

- `i`:

  The irregular component.

- `fit`:

  The fitted values from the decomposition (trend+seasonal(s) for
  additive, trend\*seasonal(s) for multiplicative decomposition).

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

[`plot.hf_decomposition`](https://rjdverse.github.io/rjd3highfreq/reference/plot.hf_decomposition.md)

## Examples

``` r
decomp <- stlplus(
  series = rjd3toolkit::ABS$X0.2.09.10.M,
  period = 12
)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/highfreq/base/r/FractionalAirlineProcessor has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

plot(decomp)
#> Error: object 'decomp' not found
```
