# Perform an Arima Model Based (AMB) decomposition on several periodcities at once

Perform an Arima Model Based (AMB) decomposition on several periodcities
at once

## Usage

``` r
multiAirlineDecomposition(
  y,
  periods,
  ndiff = 2,
  ar = FALSE,
  stde = FALSE,
  nbcasts = 0,
  nfcasts = 0,
  log = FALSE,
  y_time = NULL
)
```

## Arguments

- y:

  input time series.

- periods:

  vector of periods values of the seasonal component, any positive real
  numbers.

- stde:

  Boolean: TRUE: compute standard deviations of the components. In some
  cases (memory limits), it is currently not possible to compute them

- nbcasts:

  number of backcasts.

- nfcasts:

  number of forecasts.

- y_time:

  vector of times at which \`y\` is indexed

- adjust:

  Boolean: TRUE: actual fractional airline model is to be used, FALSE:
  the period is rounded to the nearest integer.

- sn:

  decomposition into signal and noise (2 components only). The signal is
  the seasonally adjusted series and the noise the seasonal component.
