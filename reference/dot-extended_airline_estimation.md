# Estimate parameters of an Extended Airline RegARIMA model

This internal function performs maximum likelihood estimation of an
Extended Airline model that has been previously specified and
initialized.

## Usage

``` r
.extended_airline_estimation(
  jregarima,
  jspec,
  eps = 1e-09,
  deps = 1e-09,
  exact_hessian = FALSE
)
```

## Arguments

- jregarima:

  A Java RegArimaModel object, for instance created using
  [`.extended_airline_regarima`](https://rjdverse.github.io/rjd3highfreq/reference/dot-extended_airline_regarima.md).

- jspec:

  A Java ExtendedAirlineSpec object, for instance created using
  [`.extended_airline_spec`](https://rjdverse.github.io/rjd3highfreq/reference/dot-extended_airline_spec.md).

- eps:

  Numeric scalar specifying the convergence tolerance for the
  optimization algorithm.

- deps:

  Numeric scalar. Step in the computation of the numerical derivatives,
  used in the optimisation routine. Default:1e-4.

- exact_hessian:

  Logical. If `TRUE`, computes the exact Hessian matrix at the optimum
  for calculating standard errors. If `FALSE` (default), uses a
  numerical approximation.

## Value

A list object containing detailed estimation results.
