# Convert a raw multi-period AMB decomposition to an R-friendly object

This function takes a Java object returned by
`multiAirlineDecomposition_raw()` and converts it into a structured R
object of class `JDFractionalAirlineDecomposition`. The result includes
the estimated Ucarima model, the decomposition components, standard
errors (optional), parameter estimates, and likelihood diagnostics.

## Usage

``` r
.jd2r_multi_airline_decomposition(
  jrslt,
  stde = FALSE,
  period,
  log = FALSE,
  series_time = NULL
)
```

## Arguments

- jrslt:

  Java object returned by `multiAirlineDecomposition_raw()`.

- stde:

  logical. If TRUE, include standard deviations of the components in the
  returned decomposition. Default is FALSE.

- period:

  numeric vector of seasonal periods corresponding to the decomposition.

- log:

  logical. If TRUE, indicates that the decomposition was performed on a
  log-transformed series. Default is FALSE.

- series_time:

  optional vector of time indices associated with the `series`. Default
  is NULL.

## Value

An object of class `JDFractionalAirlineDecomposition` containing:

- `ucarima`: the Ucarima model with its components,

- `decomposition`: list of original series, seasonally adjusted series,
  and component time series (with optional standard deviations),

- `estimation`: estimated parameters, covariance matrix, and score,

- `likelihood`: likelihood diagnostics.
