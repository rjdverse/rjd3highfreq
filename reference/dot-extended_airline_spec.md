# Create a specification for the Extended Airline model

This internal function constructs a Java ExtendedAirlineSpec object. The
Extended Airline model is an extension of the classic seasonal ARIMA
model that can handle multiple simultaneous periods.

## Usage

``` r
.extended_airline_spec(period, differencing = -1, ar = FALSE, to_int = FALSE)
```

## Arguments

- period:

  Numeric vector of periods present in the data. For example,
  `c(7, 365.25)` indicates weekly and annual seasonality.

- differencing:

  Differencing order to apply. The default value `-1` activates
  automatic computation based on the number of periods: if `ar=FALSE`,
  the order will be `length(period) + 1`, otherwise it will equal
  `length(period)`. Positive values manually specify the differencing
  order.

- ar:

  Logical. If `TRUE`, uses a regular stationary autoregressive (AR)
  polynomial instead of a moving average (MA) polynomial. Default:
  `FALSE`. This choice affects the automatic differencing order.

- to_int:

  Logical. If `TRUE`, rounds periodicity values to integers before
  processing. Default: `FALSE`.

## Value

A Java object of class `ExtendedAirlineSpec`.
