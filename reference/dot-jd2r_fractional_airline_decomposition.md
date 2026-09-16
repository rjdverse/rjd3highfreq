# Convert a raw fractional airline decomposition to an R-friendly object

This function takes a Java object returned by
`fractionalAirlineDecomposition_raw()` and converts it into a structured
R object of class `JDFractionalAirlineDecomposition`. The result
includes the estimated Ucarima model, the decomposition components,
standard errors (optional), parameter estimates, and likelihood
diagnostics.

## Usage

``` r
.jd2r_fractional_airline_decomposition(
  jrslt,
  sn = FALSE,
  stde = FALSE,
  period,
  log = FALSE,
  series_time = NULL
)
```

## Arguments

- jrslt:

  Java object returned by `fractionalAirlineDecomposition_raw()`.

- sn:

  logical. If TRUE, perform a signal–noise decomposition (2 components
  only: seasonally adjusted series and seasonal component). Default is
  FALSE.

- stde:

  logical. If TRUE, include standard deviations of the components in the
  returned decomposition. Default is FALSE.

- period:

  numeric. Seasonal period corresponding to the decomposition.

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
  and components (trend, irregular, seasonal), with optional standard
  deviations,

- `estimation`: estimated parameters, covariance matrix, and score,

- `likelihood`: likelihood diagnostics.
