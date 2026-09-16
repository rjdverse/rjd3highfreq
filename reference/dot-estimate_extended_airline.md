# Estimate an Extended Airline RegARIMA model

This function combines the three main Extended Airline steps:

## Usage

``` r
.estimate_extended_airline(
  series,
  period,
  differencing = -1,
  ar = FALSE,
  to_int = FALSE,
  mean = FALSE,
  xreg = NULL,
  eps = 1e-09,
  deps = 1e-09,
  exact_hessian = FALSE
)
```

## Arguments

- series:

  Numeric vector containing the time series to be modelled.

- period:

  Numeric vector of periods present in the data. For example,
  `c(7, 365.25)` for weekly and annual seasonality.

- differencing:

  Differencing order to apply. The default value `-1` activates
  automatic computation.

- ar:

  Logical. If `TRUE`, uses a regular stationary AR polynomial instead of
  an MA polynomial. Default is `FALSE`.

- to_int:

  Logical. If `TRUE`, rounds periodicity values to integers. Default is
  `FALSE`.

- mean:

  Logical. If `TRUE`, includes a mean correction term in the RegARIMA
  model. Default is `FALSE`.

- xreg:

  Optional matrix of regression variables. Default is `NULL`.

- eps:

  Numeric scalar specifying the convergence tolerance for the
  optimisation algorithm. Default is `1e-9`.

- deps:

  Numeric scalar used for the computation of numerical derivatives.
  Default is `1e-9`.

- exact_hessian:

  Logical. If `TRUE`, computes the exact Hessian at the optimum. Default
  is `FALSE`.

## Value

A list with elements:

- `spec`:

  The Java ExtendedAirlineSpec object.

- `regarima`:

  The initialised Java RegArimaModel object.

- `estimation`:

  A formatted R list containing extracted estimation components.

- `estimation_raw`:

  The original raw Java estimation object.

## Details

1\. create an Extended Airline specification, 2. initialise a RegARIMA
model, 3. estimate the model parameters.

The Java estimation result is post-processed so that selected vector
components, such as the input series and full residuals, are returned as
regular R vectors.
