# Perform an Arima Model Based (AMB) decomposition with multiple periodicities

This function performs an AMB decomposition based on (fractional)
airline models allowing for multiple seasonal periodicities at once. It
is intended for high-frequency time series where more than one seasonal
cycle may be present (e.g. weekly and annual effects).

## Usage

``` r
multi_airline_decomposition(
  series,
  period,
  ndiff = 2,
  ar = FALSE,
  stde = FALSE,
  nbcasts = 0,
  nfcasts = 0,
  eps = 1e-09,
  deps = 1e-04,
  log = FALSE,
  series_time = NULL
)
```

## Arguments

- series:

  input time series.

- period:

  numeric vector of seasonal periods. Each value must be a positive real
  number (e.g. 7 for weekly, 365.2425 for annual seasonality).

- ndiff:

  integer specifying the number of regular differences. Default is 2.

- ar:

  logical. If TRUE, an autoregressive component is included in the
  model. Default is FALSE.

- stde:

  logical. If TRUE, compute standard deviations of the components. In
  some cases (e.g. memory limits), it may not be possible to compute
  them. Default is FALSE.

- nbcasts:

  number of backcasts. Default is 0.

- nfcasts:

  number of forecasts. Default is 0.

- eps:

  precision of the optimisation routine. Default: 1e-9.

- deps:

  step in the computation of the numerical derivatives, used in the
  optimisation routine. Default:1e-4

- log:

  logical. If TRUE, the decomposition is returned on the log-scale.
  Default is FALSE.

- series_time:

  optional vector of time indices associated with `series`.

## Value

A decomposition object containing the estimated components for each
periodicity. If multiple periods are provided, a multi-period
decomposition is returned.

## Details

If a single period is supplied, the function falls back to
[`fractional_airline_decomposition()`](https://rjdverse.github.io/rjd3highfreq/reference/fractional_airline_decomposition.md).

## Examples

``` r
series <- rnorm(200)+100
dual_season <- multi_airline_decomposition(
  series,
  period = c(7, 30.4),
  log = TRUE,
  series_time = seq.Date(from=as.Date("2025-01-01"),
                         by = "days",
                         length.out = length(series))
)
#> Error in rJava::.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",     "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",     "decompose", as.numeric(series), rJava::.jarray(period),     as.integer(ndiff), ar, stde, as.integer(nbcasts), as.integer(nfcasts),     as.numeric(eps), as.numeric(deps)): RcallMethod: cannot determine object class
```
