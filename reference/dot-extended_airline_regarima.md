# Create a RegARIMA model based on an Extended Airline specification

This internal function constructs a Java RegArimaModel object by fitting
an Extended Airline model.

## Usage

``` r
.extended_airline_regarima(series, jspec, mean = FALSE, xreg = NULL)
```

## Arguments

- series:

  time series

- jspec:

  A Java ExtendedAirlineSpec object, for instance created using
  [`.extended_airline_spec`](https://rjdverse.github.io/rjd3highfreq/reference/dot-extended_airline_spec.md).

- mean:

  Logical. If `TRUE`, includes a mean correction term in the model.
  Default: `FALSE`.

- xreg:

  Optional matrix of regression variables. Default: `NULL` (no
  regressors).

## Value

A Java object of class `RegArimaModel` from the JDemetra+ toolkit.
