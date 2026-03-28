# Perform an Arima Model Based (AMB) decomposition

Perform an Arima Model Based (AMB) decomposition

## Usage

``` r
fractionalAirlineDecomposition(
  y,
  period,
  sn = FALSE,
  stde = FALSE,
  nbcasts = 0,
  nfcasts = 0,
  eps = 1e-09,
  deps = 1e-04,
  log = FALSE,
  y_time = NULL
)
```

## Arguments

- y:

  input time series.

- period:

  period of the seasonal component, any positive real number.

- sn:

  decomposition into signal and noise (2 components only). The signal is
  the seasonally adjusted series and the noise the seasonal component.

- stde:

  Boolean: TRUE: compute standard deviations of the components. In some
  cases (memory limits), it is currently not possible to compute them

- nbcasts:

  number of backcasts.

- nfcasts:

  number of forecasts.

- eps:

  precision of the optimisation routine

- deps:

  step in the computation of the numerical derivatives, used in the
  optimisation routine

- y_time:

  vector of times at which \`y\` is indexed

- adjust:

  Boolean: TRUE: actual fractional airline model is to be used, FALSE:
  the period is rounded to the nearest integer.
